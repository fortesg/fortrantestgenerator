from assertions import assertType, assertTypeAll
from source import Subroutine, SourceFile, VariableReference, Variable

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

    def lbound(self, variable, dim, *placeholder):
        assertType(variable, 'variable', UsedVariable, True)
        assertType(dim, 'dim', int)
        assertTypeAll(placeholder, 'placeholder', str)
        
        bound = self.__bound(variable, dim, placeholder)
        if bound != '':
            return 'L' + bound
        return ''
    
    def ubound(self, variable, dim, *placeholder):
        assertType(variable, 'variable', UsedVariable, True)
        assertType(dim, 'dim', int)
        assertTypeAll(placeholder, 'placeholder', str)
        
        bound = self.__bound(variable, dim, placeholder)
        if bound != '':
            return 'U' + bound
        return ''

    def __bound(self, variable, dim, placeholder):
        assertType(variable, 'variable', UsedVariable, True)
        assertType(dim, 'dim', int)
        assertTypeAll(placeholder, 'placeholder', str)
        
        if variable is None:
            return ''
        
        noDim = False
        if dim <= 0:
            noDim = True
        
        ref = variable.getReference()
        if ref.isOneVariableArray():
            if noDim:
                dim = ref.getTotalDimensions()
            elif dim > ref.getTotalDimensions():
                return ''
            
            top = 0
            perc = ''
            bound = 'BOUND('
            for level in ref.getLevels():
                var = ref.getVariable(level)
                if var is None:
                    return ''
                bound += perc + var.getName()
                perc = '%'
                bot = top 
                top += var.getDimension()
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
        assertType(variable, 'variable', UsedVariable, True)
        assertType(dim, 'dim', int)
        assertTypeAll(placeholder, 'placeholder', str)
        
        if variable is None:
            return ''
        
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
            var = ref.getVariable(level)
            if var is None:
                return ''
            aa += perc + var.getName()
            perc = '%'
            pointer = var.isPointer()
            allocatable = var.isAllocatable()
            allocatablesAndPointers += (pointer or allocatable)
            bot = top 
            top += var.getDimension()
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
    
    def fillIndices(self, variable, dim, *indices):
        assertType(variable, 'variable', UsedVariable, True)
        assertType(dim, 'dim', int)
        assertTypeAll(indices, 'indices', str)
        
        if variable is None:
            return ''
        
        ref = variable.getReference()
        perc = ''
        d = 0
        filled = ''
        for level in ref.getLevels():
            var = ref.getVariable(level)
            if var is None:
                return variable.expression()
            filled += perc + var.getName()
            perc = '%'
            if var.isArray() and d < dim:
                filled += '('
                sep = ''
                for _ in range(0, var.getDimension()):
                    filled += sep
                    if d < dim and d < len(indices):
                        filled += indices[d]
                    else:
                        filled += ':'
                    sep = ', '
                    d += 1
                filled += ')'
        return filled
    
    def writeVarNameWithFilledIndicesToString(self, variable, destination, dim, *indices):
        assertType(variable, 'variable', UsedVariable, True)
        
        if variable is None:
            return ''
        
        parts = []
        for index in indices:
            parts.append("', " + index + ", '") 
        
        filled = self.fillIndices(variable, dim, *parts)
        if not filled:
            return ''
        if filled == variable.expression():
            return destination + ' = "' + variable.expression() + '"'
        
        write = "WRITE (" + destination + ",'("
        write += 'A,I0,' * min(dim, len(indices), variable.totalDim())
        write += "A)') '" + filled + "'"
        
        return write

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
        
    def needsAllocationFilled(self, variable, dim, *indices): 
        assertType(variable, 'variable', UsedVariable)
        assertType(dim, 'dim', int)
        assertTypeAll(indices, 'indices', str)
        
        filled = self.fillIndices(variable, dim, *indices)
        return (variable.allocatableOrPointer() or variable.hasClassType()) and not self.alreadyAllocated(filled)

    def needsAllocation(self, variable):
        assertType(variable, 'variable', UsedVariable)
        return (variable.allocatableOrPointer() or variable.hasClassType() or (variable.fromArgument() and variable.level() == 0 and variable.dim() > 0)) and not self.alreadyAllocated(variable)
    
    def containerNeedsAllocation(self, variable):
        assertType(variable, 'variable', UsedVariable)
        for level in variable.levels(True):
            container = variable.container(level)
            if self.needsAllocation(container):
                return True
        return False
    
    def setAllocated(self, variable):
        assertType(variable, 'variable', [UsedVariable, str])
        self.__allocated.add(variable)
        
    def alreadyAllocated(self, variable):
        assertType(variable, 'variable', [UsedVariable, str])
        return variable in self.__allocated

    def alloc(self, variable, dim, *dimSizes): 
        assertType(variable, 'variable', UsedVariable, True)
        assertType(dim, 'dim', int)
        assertTypeAll(dimSizes, 'dimSizes', str)
        
        if variable is None:
            return ''
        
        alloc = 'ALLOCATE(' + variable.expression()
        if dim > 0:
            alloc += '('
            sep = ''
            for d in range(min(dim, len(dimSizes))):
                alloc += sep + dimSizes[d]
                sep = ', '
            alloc += ')'
        alloc += ')'
        
        self.setAllocated(variable)
            
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
        
        publicElements = sourceFile.getModule(moduleName).getPublicElements()
        
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
        return hash(self.__ref)
        
    def __str__(self):
        return self.expression()
    
    def getReference(self):
        return self.__ref
    
    def type(self):
        var = self.__ref.getLevelNVariable()
        if var is None:
            return ''
        return var.getTypeName()
    
    def hasClassType(self):
        var = self.__ref.getLevelNVariable()
        if var is None:
            return ''
        return var.hasClassType()
    
    def expression(self):
        return self.__ref.getExpression().lower()
    
    def level(self):
        return self.__ref.getLevel()
    
    def levels(self, decrementing = False):
        return self.__ref.getLevels(decrementing)
        
    def dim(self):
        return self.__ref.getLevelNDimension()
    
    def allocatable(self):
        var = self.__ref.getLevelNVariable()
        if var is None:
            return False
        return var.isAllocatable()
    
    def pointer(self):
        var = self.__ref.getLevelNVariable()
        if var is None:
            return False
        return var.isPointer()
    
    def allocatableOrPointer(self):
        var = self.__ref.getLevelNVariable()
        if var is None:
            return False
        return var.isAllocatable() or var.isPointer()
    
    def totalDim(self):
        return self.__ref.getTotalDimensions()
    
    def containsArray(self):
        return self.__ref.isOneVariableArray()
    
    def referencable(self):
        return self.__ref.isReferencable()
    
    def mandatoryDimensions(self):
        for level in self.__ref.getLevels(True):
            variable = self.__ref.getVariable(level)
            if variable is not None and (variable.isAllocatable() or variable.isPointer() or variable.isArray()):
                dims = 0
                for cLevel in range(level - 1, -1, -1):
                    cVariable = self.__ref.getVariable(cLevel)
                    if cVariable is not None and cVariable.isArray():
                        dims += cVariable.getDimension()
                return dims
        return 0
    
    def container(self, level = -1):
        if level < 0:
            level = level + self.level()
        level = min(level, self.level())
        level = max(level, 0)

        return UsedVariable(self.__ref.getSubReference(level))
    
    def containerByDimension(self, dim):
        cDims = 0
        for level in self.__ref.getLevels():
            variable = self.__ref.getVariable(level)
            if variable is not None and variable.isArray():
                cDims += variable.getDimension()
                if cDims >= dim:
                    return UsedVariable(self.__ref.getSubReference(level)) 
        return self
    
    def fromArgument(self):
        return self.__ref.getLevel0Variable().isArgument()
    
    def fromGlobal(self):
        return self.__ref.getLevel0Variable().isModuleVar()

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
            
    def __eq__(self, other):
        if (other is None or not isinstance(other, Argument)):
            return False;
        else:
            return self.__var == other.__var
    
    def __ne__(self, other):
        return not self == other
        
    def __hash__(self):
        return hash(self.__var)
        
    def __str__(self):
        return self.name()

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
    
    def __nonzero__(self):
        return bool(self.__arguments)
    
    def __len__(self):
        return len(self.__arguments)
    
    def __getitem__(self, key):
        return self.__arguments[key]
        
    def __iter__(self):
        return iter(self.__arguments)
    
    def __reversed__(self):
        return reversed(self.__arguments)
    
    def __contains__(self, item):
        return item in self.__arguments
    
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
