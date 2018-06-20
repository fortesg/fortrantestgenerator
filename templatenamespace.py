from assertions import assertType, assertTypeAll
from source import Subroutine, SourceFile, VariableReference, Variable
from string import find
import re
from twisted.spread.jelly import reference_atom

# TODO Gemeinsamkeiten zwischen Capture- und ReplayTemplatesNameSpace in Oberklasse zusammenfuehren
class TemplatesNameSpace(object):
    def __init__(self, subroutine, typeArgumentReferences, globalsReferences, testDataDir):
        assertType(subroutine, 'subroutine', Subroutine)
        assertType(typeArgumentReferences, 'typeArgumentReferences', list)
        assertType(globalsReferences, 'globalsReferences', list)
        
        self.__subroutine = subroutine
        self._typeArgumentReferences = typeArgumentReferences
        
        self._globalsReferences = []
        for reference in globalsReferences:
            reference = reference.cleanCopy()
            variableName = reference.getVariableName() 
            if reference.getDeclaredInName() != subroutine.getModuleName():
                variable = reference.getLevel0Variable()
                moduleName = variable.getDeclaredInName()
                if variable is not None and moduleName is not None:
                    newName = moduleName + '__' + variableName
                    alias = variable.getAlias(newName)
                    reference.setLevel0Variable(alias)
            self._globalsReferences.append(reference)
            
        self.subroutine = SubroutineNameSpace(subroutine)
        self.module = ModuleNameSpace(subroutine.getModuleName())
        self.args = ArgumentList(subroutine.getArguments(), typeArgumentReferences)
        self.globals = GlobalsNameSpace(subroutine, subroutine.getSourceFile(), self._globalsReferences, False)
        self.dataDir = testDataDir.rstrip('/');
    
    def totalDim(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            return reference.getTotalDimensions()
        return -1
    
    def isAllocatable(self, variableName):
        var = self._findVariable(variableName)
        return var is not None and var.isAllocatable()
    
    def isPointer(self, variableName):
        var = self._findVariable(variableName)
        return var is not None and var.isPointer()
    
    def isAllocatableOrPointer(self, variableName):
        var = self._findVariable(variableName)
        return var is not None and (var.isAllocatable() or var.isPointer())
    
    def isInArgument(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            var = reference.getVariable()
            if var is not None:
                return var.isInArgument()
            
        return False
    
    def isOutArgument(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            var = reference.getVariable()
            if var is not None:
                return var.isOutArgument()
            
        return False
    
    def isReferencable(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            if find(variableName, '%') > 0:
                return reference.isReferencable()
            return True
        
        return False
    
    def getNumberOfMandatoryDimensions(self, variableName):
        if find(variableName, '%') > 0:
            reference = self._findReference(variableName)
            if reference is not None:
                for level in reference.getLevels(True):
                    variable = reference.getVariable(level)
                    if variable is not None and (variable.isAllocatable() or variable.isPointer() or variable.isArray()):
                        dims = 0
                        for cLevel in range(level - 1, -1, -1):
                            cVariable = reference.getVariable(cLevel)
                            if cVariable is not None and cVariable.isArray():
                                dims += cVariable.getDimension()
                        return dims
        return 0
        
    def lbound(self, variableName, dim, *placeholder):
        bound = self.__bound(variableName, dim, placeholder)
        if bound != '':
            return 'L' + bound
        return ''
    
    def ubound(self, variableName, dim, *placeholder):
        bound = self.__bound(variableName, dim, placeholder)
        if bound != '':
            return 'U' + bound
        return ''

    def __bound(self, variableName, dim, placeholder):
        noDim = False
        if dim <= 0:
            noDim = True
        
        reference = self._findReference(variableName)
        if reference is not None and reference.isOneVariableArray():
            if noDim:
                dim = reference.getTotalDimensions()
            elif dim > reference.getTotalDimensions():
                return ''
            
            top = 0
            perc = ''
            bound = 'BOUND('
            for level in reference.getLevels():
                variable = reference.getVariable(level)
                if variable is None:
                    return ''
                bound += perc + variable.getName()
                perc = '%'
                bot = top 
                top += variable.getDimension()
                if top < dim:
                    if top > bot:
                        bound += '('
                        sep = ''
                        for i in range(bot, top):
                            bound += sep + placeholder[i]
                            sep = ', '
                        bound += ')'
                else:
                    break
            if not noDim:
                bound += ', ' + str(dim - bot)
            bound += ')'
            return bound
                
        return ''
    
    def allocatedOrAssociated(self, variable, dim, *placeholder):
        assertType(variable, 'variable', UsedVariable)
        
        ref = variable.getReference()
        totalDim = ref.getTotalDimensions()
        if dim > totalDim:
            dim = totalDim
        top = 0
        pointer = False
        allocatable = False
        perc = ''
        aa = '('
        totalAllocatablesAndPointers = ref.getNumberOfPointerAndAllocatableLevels()
        allocatablesAndPointers = 0
        for level in ref.getLevels():
            variable = ref.getVariable(level)
            if variable is None:
                return ''
            aa += perc + variable.getName()
            perc = '%'
            pointer = variable.isPointer()
            allocatable = variable.isAllocatable()
            allocatablesAndPointers += (pointer or allocatable)
            bot = top 
            top += variable.getDimension()
            if top < dim or (allocatablesAndPointers < totalAllocatablesAndPointers and dim == totalDim):
                if top > bot:
                    aa += '('
                    sep = ''
                    for i in range(bot, top):
                        aa += sep + placeholder[i]
                        sep = ', '
                    aa += ')'
            else:
                break
        aa += ')'
        if allocatable:
            return 'ALLOCATED' + aa
        elif pointer:
            return 'ASSOCIATED' + aa
        return ''
    
    def fillIndices(self, variableName, dim, *indices):
        reference = self._findReference(variableName)
        if reference is not None:
            perc = ''
            d = 0
            filled = ''
            for level in reference.getLevels():
                variable = reference.getVariable(level)
                if variable is None:
                    return variableName
                filled += perc + variable.getName()
                perc = '%'
                if variable.isArray() and d < dim:
                    filled += '('
                    sep = ''
                    for _ in range(0, variable.getDimension()):
                        filled += sep
                        if d < dim and d < len(indices):
                            filled += indices[d]
                        else:
                            filled += ':'
                        sep = ', '
                        d += 1
                    filled += ')'
            return filled
        
        return ''
    
    def writeVarNameWithFilledIndicesToString(self, variableName, destination, dim, *indices):
        parts = []
        for index in indices:
            parts.append("', " + index + ", '") 
        
        filled = self.fillIndices(variableName, dim, *parts)
        if not filled:
            return ''
        if filled == variableName.lower():
            return destination + ' = "' + variableName + '"'
        
        write = "WRITE (" + destination + ",'("
        write += 'A,I0,' * min(dim, len(indices), self.totalDim(variableName))
        write += "A)') '" + filled + "'"
        
        return write
    
    def _findVariable(self, variableName):
        # TODO Kann man hier nicht _findReference benutzen?
        variableName = variableName.lower()
        if not variableName:
            return None
        elif find(variableName, '%') < 0:
            if self.__subroutine.hasVariable(variableName):
                return self.__subroutine.getVariable(variableName)
        for reference in (self._typeArgumentReferences + self._globalsReferences):
            expression = reference.getExpression().lower()
            if expression == variableName:
                return reference.getLevelNVariable()
            elif expression.startswith(variableName + '%'):
                return reference.getVariable(variableName.count('%'))

        return None
    
    def _findReference(self, expression):
        expression = expression.lower()
        expression = self.__removeBrackets(expression)
        if not expression:
            return None
        elif find(expression, '%') < 0:
            if self.__subroutine.hasVariable(expression):
                return VariableReference(expression, self.__subroutine.getName(), 0, self.__subroutine.getVariable(expression))
        for reference in self._typeArgumentReferences + self._globalsReferences:
            refExpression = reference.getExpression().lower()
            if refExpression == expression:
                return reference
            elif refExpression.startswith(expression + "%"):
                return reference.getSubReference(expression.count('%'))
        return None   
    
    def __removeBrackets(self, text):
        regEx = re.compile(r'.*\([^\(\)]*\).*')
        while regEx.match(text) is not None:
            text = re.sub(r'\([^\(\)]*\)', '', text)
        return text


class CaptureTemplatesNameSpace(TemplatesNameSpace):

    def __init__(self, subroutine, typeArgumentReferences, globalsReferences, testDataDir):
        
        super(CaptureTemplatesNameSpace, self).__init__(subroutine, typeArgumentReferences, globalsReferences, testDataDir)
        self.__registered = set()
        
    def needsRegistration(self, variable):
        assertType(variable, 'variable', UsedVariable)
        return not self.alreadyRegistered(variable)
    
    def containerNeedsRegistration(self, variable):
        assertType(variable, 'variable', UsedVariable)
        
        for level in variable.levels(True):
            if not self.alreadyRegistered(variable.container(level)):
                return True
                
        return False
    
    def setRegistered(self, variable):
        assertType(variable, 'variable', UsedVariable)
        self.__registered.add(variable)
        
    def alreadyRegistered(self, variable):
        assertType(variable, 'variable', UsedVariable)
        return variable in self.__registered
    
    def resetRegistrations(self):
        self.__registered = set()
        
class ReplayTemplatesNameSpace(TemplatesNameSpace):
 
    def __init__(self, subroutine, typeArgumentReferences, globalsReferences, testDataDir):
        
        super(ReplayTemplatesNameSpace, self).__init__(subroutine, typeArgumentReferences, globalsReferences, testDataDir)
        self.globals = GlobalsNameSpace(subroutine, subroutine.getSourceFile(), self._globalsReferences, True)        
        self.types = TypesNameSpace(subroutine, self._typeArgumentReferences, self._globalsReferences, True)
        self.__allocated = set()
        
    def needsAllocationFilled(self, variableName, dim, *indices): 
        var = self._findVariable(variableName)
        filled = self.fillIndices(variableName, dim, *indices)
        return var is not None and (var.isAllocatable() or var.isPointer() or var.hasClassType()) and not self.alreadyAllocated(filled)

    def needsAllocation(self, variableName):
        var = self._findVariable(variableName)
        return var is not None and (var.isAllocatable() or var.isPointer() or var.hasClassType()) and not self.alreadyAllocated(variableName)
    
    def containerNeedsAllocation(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            for level in reference.getLevels(True):
                variable = reference.getVariable(level)
                if variable is not None and (variable.isAllocatable() or variable.isPointer() or variable.hasClassType()) and not self.alreadyAllocated(reference.getExpression(level)):
                    return True
                
        return False
    
    def setAllocated(self, variableName):
        self.__allocated.add(variableName)
        
    def alreadyAllocated(self, variableName):
        return variableName in self.__allocated

    def alloc(self, variableName, dim, *dimSizes):
        alloc = 'ALLOCATE(' + variableName
        if dim > 0:
            alloc += '('
            sep = ''
            for d in range(min(dim, len(dimSizes))):
                alloc += sep + dimSizes[d]
                sep = ', '
            alloc += ')'
        alloc += ')'
        
        self.setAllocated(variableName)
            
        return alloc
    

class SubroutineNameSpace(object):
    def __init__(self, subroutine):
        assertType(subroutine, 'subroutine', Subroutine)
        
        self.name = subroutine.getSimpleName().lower()
        self.export = ''
        if self.name not in subroutine.getModule().getPublicElements():
            self.export = 'PUBLIC :: ' + self.name

class ModuleNameSpace(object):
    def __init__(self, moduleName):
        assertType(moduleName, 'moduleName', str)

        self.name = moduleName

class UsedVariable(object):
    
    def __init__(self, reference):
        assertType(reference, 'reference', VariableReference)
        self.__ref = reference
        
    def __eq__(self, other):
        if (other is None or not isinstance(other, UsedVariable)):
            return False;
        else:
            return self.__ref == other.__ref
    
    def __ne__(self, other):
        return not self == other
        
    def __hash__(self):
        return hash(self.__ref);
        
    def __str__(self):
        return self.__ref.getExpression()
    
    def getReference(self):
        return self.__ref
    
    def levels(self, decrementing = False):
        return self.__ref.getLevels(decrementing)
        
    def dim(self):
        return self.__ref.getLevelNDimension()
    
    def type(self):
        var = self.__ref.getLevelNVariable()
        if var is None:
            return ''
        return var.getTypeName()
    
    def containsArray(self):
        return self.__ref.isOneVariableArray()
    
    def referencable(self):
        return self.__ref.isReferencable()
    
    def container(self, level):
        return UsedVariable(self.__ref.getSubReference(level))

class Argument(object):
    
    def __init__(self, variable, references):
        assertType(variable, 'variable', Variable)
        assert variable.isArgument()
        assertTypeAll(references, 'references', VariableReference)
        
        self.__var = variable
        self.__used = []
        for ref in references:
            if ref.getLevel0Variable() == self.__var:
                self.__used.append(UsedVariable(ref))
        if not self.__used and variable.hasBuiltInType():
            self.__used.append(UsedVariable(VariableReference(variable.getName(), variable.getDeclaredIn().getName(), 0, variable)))

    def intent(self):
        return self.__var.getIntent().lower()
    
    def intentIn(self):
        return self.intent() == 'in'
    
    def intentOut(self):
        return self.intent() == 'out'
    
    def intentInout(self):
        return self.intent() == 'inout'
    
    def isIn(self):
        return self.intentIn() or self.intentInout()
    
    def isOut(self):
        return self.intentOut() or self.intentInout()
    
    def optional(self):
        return self.__var.isOptionalArgument()
    
    def required(self):
        return not self.__var.isOptionalArgument()
    
    def builtInType(self):
        return self.__var.hasBuiltInType()
    
    def derivedType(self):
        return self.__var.hasDerivedType()
    
    def array(self):
        return self.__var.isArray()
    
    def pointer(self):
        return self.__var.isPointer()
    
    def allocatable(self):
        return self.__var.isAllocatable()
    
    def allocatableOrPointer(self):
        return self.allocatable() or self.pointer()
    
    def name(self):
        return self.__var.getName()
    
    def spec(self, intent = None, allocatable = None, charLengthZero = False):
        alias = self.__var.getAlias()
        if intent is not None:
            alias.setIntent(intent)
        if allocatable is not None:
            if allocatable: 
                if alias.getDimension() > 0 or alias.hasClassType():
                    alias.setAllocatable(True)
            else:
                alias.setAllocatable(False)
        if charLengthZero and alias.hasBuiltInType() and alias.getTypeName().startswith('CHARACTER'):
            alias.setTypeName('CHARACTER(len=0)')
        alias.setTarget(False)
        return str(alias)
    
    def usedVariables(self):
        return self.__used
    

class ArgumentList(object):
    def __init__(self, arguments, typeArgumentReferences = None):
        if typeArgumentReferences is None:
            assertTypeAll(arguments, 'arguments', Argument)
            self.__arguments = arguments
        else:
            assertTypeAll(arguments, 'arguments', Variable)
            assertTypeAll(typeArgumentReferences, 'typeArgumentReferences', VariableReference)
            self.__arguments = [Argument(var, typeArgumentReferences) for var in arguments]
        
    def __iter__(self):
        return iter(self.__arguments)
    
    def filter(self, predicate):
        return ArgumentList([arg for arg in self.__arguments if predicate(arg)])
    
    def intentIn(self):
        return self.filter(lambda a : a.intentIn())
    
    def intentOut(self):
        return self.filter(lambda a : a.intentOut())
    
    def intentInout(self):
        return self.filter(lambda a : a.intentInout())
    
    def allIn(self):
        return self.filter(lambda a : a.isIn())
    
    def allOut(self):
        return self.filter(lambda a : a.isOut())
    
    def optionals(self):
        return self.filter(lambda a : a.optional())
    
    def requireds(self):
        return self.filter(lambda a : a.required())
    
    def builtInTypes(self):
        return self.filter(lambda a : a.builtInType())
    
    def derivedTypes(self):
        return self.filter(lambda a : a.derivedType())
    
    def pointers(self):
        return self.filter(lambda a : a.pointer())
    
    def allocatables(self):
        return self.filter(lambda a : a.allocatable())
    
    def allocatablesOrPointers(self):
        return self.filter(lambda a : a.allocatableOrPointer())
    
    def names(self):
        return [arg.name() for arg in self.__arguments]
    
    def joinNames(self, sep = ', '):
        return sep.join(self.names())
    
    def specs(self, intent = None, allocatable = None, charLengthZero = False):
        return "\n".join([arg.spec(intent, allocatable, charLengthZero) for arg in self.__arguments])
    
    def usedVariables(self):
        return sum([arg.usedVariables() for arg in self.__arguments], [])

class GlobalsNameSpace(object):
    
    def __init__(self, subroutine, sourceFile, globalsReferences, includeTestModule):
        assertType(subroutine, 'subroutine', Subroutine)
        assertType(sourceFile, 'sourceFile', SourceFile)
        assertTypeAll(globalsReferences, 'globalsReferences', VariableReference)
        assertType(includeTestModule, 'includeTestModule', bool)

        self.usedVariables = []
        variables = set()
        types = set()
        for ref in globalsReferences:
            self.usedVariables.append(UsedVariable(ref))
            variable = ref.getLevel0Variable()
            variables.add(variable)
            if variable.hasDerivedType() and variable.isTypeAvailable():
                types.add(variable.getType())
        
        testModule = subroutine.getName().getModuleName()
        modules = dict()    
        for variable in variables:
            moduleName = variable.getDeclaredInName()
            if moduleName != testModule or includeTestModule:
                if moduleName not in modules:
                    modules[moduleName] = []
                varName = variable.getName() 
                if varName != variable.getOriginalName():
                    varName += ' => ' + variable.getOriginalName()
                modules[moduleName].append(varName)
        for typE in types:
            moduleName = typE.getDeclaredInName()
            if isinstance(moduleName, str) and (moduleName != testModule or includeTestModule):
                if moduleName not in modules:
                    modules[moduleName] = []
                modules[moduleName].append(typE.getName())
         
        self.imports = ''
        for module, elements in modules.iteritems():
            self.imports += 'USE ' + module + ', ONLY: '
            for element in elements:
                self.imports += element
                #TODO alias
                self.imports += ', '
            self.imports  = self.imports.strip(', ')
            self.imports += "\n"
        self.imports = self.imports.strip("\n")
        
        exportGlobals = ExportGlobalsNameSpace(testModule, sourceFile, globalsReferences)
        self.exports = exportGlobals.exports

class TypesNameSpace(object):
    
    def __init__(self, subroutine, typeArgumentReferences, globalsReferences, includeTestModule):
        assertType(subroutine, 'subroutine', Subroutine)
        assertTypeAll(typeArgumentReferences, 'typeArgumentReferences', VariableReference)
        assertTypeAll(globalsReferences, 'globalsReferences', VariableReference)
        assertType(includeTestModule, 'includeTestModule', bool)
        
        variables = set(subroutine.getDerivedTypeArguments())
        for reference in globalsReferences:
            variables.add(reference.getLevel0Variable())

        self.__types = dict()
        for variable in variables:
            if variable.hasDerivedType() and variable.isTypeAvailable():
                typE = variable.getType()
                if typE.getName() not in self.__types:
                    self.__types[typE.getName()] = typE
                    #self.__addMemberTypesToTypSet(typE)
                    
        testModule = subroutine.getName().getModuleName()
        modules = dict()    
        for typE in self.__types.values():
            moduleName = typE.getDeclaredInName()
            if  isinstance(moduleName, str) and (moduleName != testModule or includeTestModule):
                if moduleName not in modules:
                    modules[moduleName] = []
                modules[moduleName].append(typE.getName())
         
        self.imports = ''
        for module, typeNames in modules.iteritems():
            self.imports += '  USE ' + module + ', ONLY: '
            for typeName in typeNames:
                self.imports += typeName + ', '
            self.imports  = self.imports.strip(', ')
            self.imports += "\n"
        self.imports = self.imports.strip("\n")
        
    def __addMemberTypesToTypSet(self, typE):
        for member in typE.getMembers():
            if member.hasDerivedType() and member.isPointer() and member.isTypeAvailable():
                memberType = member.getType()
                if memberType.getName() not in self.__types:
                    self.__types[memberType.getName] = memberType 
                    self.__addMemberTypesToTypSet(memberType)
        
class ExportNameSpace(object):
    
    def __init__(self, moduleName, sourceFile, globalsReferences):
        assertType(moduleName, 'moduleName', str)
        assertType(sourceFile, 'sourceFile', SourceFile)
        assertType(globalsReferences, 'globalsReferences', list)
        
        self.module = ModuleNameSpace(moduleName)
        self.globals = ExportGlobalsNameSpace(moduleName, sourceFile, globalsReferences)
        
        
class ExportGlobalsNameSpace(object):
    
    def __init__(self, moduleName, sourceFile, globalsReferences):
        assertType(moduleName, 'moduleName', str)
        assertType(sourceFile, 'sourceFile', SourceFile)
        assertType(globalsReferences, 'globalsReferences', list)
        
        publicElements = sourceFile.getPublicElements()
        
        self.exports = 'PUBLIC :: '
        variables = set()
        types = set()
        for ref in globalsReferences:
            variable = ref.getLevel0Variable()
            refModule = variable.getDeclaredInName()
            if refModule == moduleName:
                variableName = variable.getOriginalName().lower()
                if variableName not in variables and not variable.isPublic() and variableName not in publicElements:
                    self.exports += variableName + ", "
                    variables.add(variableName)
            if variable.hasDerivedType() and variable.isTypeAvailable():
                typE = variable.getType()
                refModule = typE.getDeclaredInName()
                if refModule == moduleName:
                    typeName = typE.getName().lower()
                    if typeName not in types and typeName not in publicElements:
                        self.exports += typeName + ", "
                        types.add(typeName)
        self.exports = self.exports.strip(', ')
        
        if self.exports == 'PUBLIC ::':
            self.exports = ''
