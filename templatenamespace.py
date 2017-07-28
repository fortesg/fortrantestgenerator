from utils import assertType, assertTypeAll
from source import Subroutine, SourceFile, VariableReference
from string import find
import re
from symbol import argument

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
            if reference.getDeclaredIn() != subroutine.getModuleName():
                variable = reference.getLevel0Variable()
                if variable is not None and variable.getDeclaredIn() is not None:
                    newName = variable.getDeclaredIn() + '__' + variableName
                    alias = variable.getAlias(newName)
                    reference.setLevel0Variable(alias)
            self._globalsReferences.append(reference)
            
        self.subroutine = SubroutineNameSpace(subroutine)
        self.module = ModuleNameSpace(subroutine.getModuleName())
        self.arguments = ArgumentsNameSpace(subroutine, typeArgumentReferences)
        self.globals = GlobalsNameSpace(subroutine, subroutine.getSourceFile(), self._globalsReferences, False)
        self.dataDir = testDataDir.rstrip('/');
     
    def getExpression(self, variableName, level):
        reference = self._findReference(variableName)
        if reference is not None:
            return reference.getExpression(level)
        return ''    
        
    def levels(self, variableName, decrementing = False):
        reference = self._findReference(variableName)
        if reference is not None:
            return reference.getLevels(decrementing)
        return []
    
    def dim(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            return reference.getLevelNDimension()
        return -1
    
    def totalDim(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            return reference.getTotalDimensions()
        return -1
    
    def type(self, variableName):
        var = self._findVariable(variableName)
        if var is None:
            return ''
        return var.getTypeName()

    def isAllocatable(self, variableName):
        var = self._findVariable(variableName)
        return var is not None and var.isAllocatable()
    
    def isPointer(self, variableName):
        var = self._findVariable(variableName)
        return var is not None and var.isPointer()
    
    def isAllocatableOrPointer(self, variableName):
        var = self._findVariable(variableName)
        return var is not None and (var.isAllocatable() or var.isPointer())
    
    def isArray(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            return reference.isOneVariableArray()
            
        return False
    
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
    
        
    def getContainer(self, variableName, dim):
        reference = self._findReference(variableName)
        if reference is not None:
            top = 0
            perc = ''
            cont = ''
            for level in reference.getLevels():
                variable = reference.getVariable(level)
                if variable is None:
                    return ''
                cont += perc + variable.getName()
                perc = '%'
                top += variable.getDimension()
                if top >= dim:
                    break
            return cont
                
        return ''
    
    def allocatedOrAssociated(self, variableName, dim, *placeholder):
        reference = self._findReference(variableName)
        if reference is not None:
            
            totalDim = reference.getTotalDimensions()
            if dim > totalDim:
                dim = totalDim
            top = 0
            if reference.getExpression() == 'dist_cell_owner%dt_info%extent':
                print '*** DEBUG ***' + str(reference) + ' // ' + str(totalDim) 
            pointer = False
            allocatable = False
            perc = ''
            aa = '('
            for level in reference.getLevels():
                variable = reference.getVariable(level)
                if variable is None:
                    return ''
                aa += perc + variable.getName()
                perc = '%'
                pointer = variable.isPointer()
                allocatable = variable.isAllocatable()
                bot = top 
                top += variable.getDimension()
                if top < dim:
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
        if filled == variableName:
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
        
    def needsRegistration(self, variableName):
        var = self._findVariable(variableName)
        return var is not None and not self.alreadyRegistered(variableName)
    
    def containerNeedsRegistration(self, variableName):
        reference = self._findReference(variableName)
        if reference is not None:
            for level in reference.getLevels(True):
                variable = reference.getVariable(level)
                if variable is not None and not self.alreadyRegistered(reference.getExpression(level)):
                    return True
                
        return False
    
    def setRegistered(self, variableName):
        self.__registered.add(variableName)
        
    def alreadyRegistered(self, variableName):
        return variableName in self.__registered
    
    def resetRegistrations(self):
        self.__registered = set()
        
class ReplayTemplatesNameSpace(TemplatesNameSpace):
 
    def __init__(self, subroutine, typeArgumentReferences, globalsReferences, testDataDir):
        
        super(ReplayTemplatesNameSpace, self).__init__(subroutine, typeArgumentReferences, globalsReferences, testDataDir)
        self.globals = GlobalsNameSpace(subroutine, subroutine.getSourceFile(), self._globalsReferences, True)        
        self.types = TypesNameSpace(subroutine, self._typeArgumentReferences, self._globalsReferences, True)
        self.__allocated = set()
        
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

class ArgumentsNameSpace(object):
    
    def __init__(self, subroutine, typeArgumentReferences):
        assertType(subroutine, 'subroutine', Subroutine)
        assertTypeAll(typeArgumentReferences, 'typeArgumentReferences', VariableReference)
        
        self.input = ArgumentsSubNameSpace(subroutine.getInArguments(), typeArgumentReferences)
        self.output = ArgumentsSubNameSpace(subroutine.getOutArguments(), typeArgumentReferences)
        self.all = ArgumentsSubNameSpace(subroutine.getArguments(), typeArgumentReferences)

class ArgumentsSubNameSpace(object):
    def __init__(self, arguments, typeArgumentReferences):
        self.__arguments = arguments
        self._typeArgumentReferences = typeArgumentReferences
        
        self.__names = []
        for argument in self.__arguments:
            self.__names.append(argument.getName())
        
    def names(self):
        return ', '.join(self.__names)
    
    def specifications(self, intent = '', allocatable = False, charLengthZero = False):
        specs = []
        for argument in self.__arguments:
            argCopy = argument.getAlias(argument.getName())
            argCopy.setIntent(intent)
            if allocatable and (argCopy.hasBuiltInType() and argCopy.getDimension() > 0) or (argCopy.hasClassType()):
                argCopy.setAllocatable(True)
            if charLengthZero and argCopy.hasBuiltInType() and argCopy.getTypeName().startswith('CHARACTER'):
                argCopy.setTypeName('CHARACTER(len=0)')
            argCopy.setTarget(False)
            specs.append(str(argCopy))
            
        return "\n".join(specs)
    
    def basic(self):
        basic = []
        for argument in self.__arguments:
            if argument.hasBuiltInType():
                name = argument.getName()
                if not argument.isOptionalArgument():
                    basic.append(name)
        return basic
    
    def optional(self):
        optional = []
        for argument in self.__arguments:
            if argument.hasBuiltInType():
                name = argument.getName()
                if argument.isOptionalArgument():
                    optional.append(name)
        return optional
    
    def usedTypeMembers(self):
        usedTypeMembers = []
        for reference in self._typeArgumentReferences:
            if reference.getVariableName(0) in self.__names:
                usedTypeMembers.append(reference.getExpression())
        return usedTypeMembers

class GlobalsNameSpace(object):
    
    def __init__(self, subroutine, sourceFile, globalsReferences, includeTestModule):
        assertType(subroutine, 'subroutine', Subroutine)
        assertType(sourceFile, 'sourceFile', SourceFile)
        assertTypeAll(globalsReferences, 'globalsReferences', VariableReference)
        assertType(includeTestModule, 'includeTestModule', bool)

        self.usedVariables = []
        variables = set()
        types = set()
        for reference in globalsReferences:
            self.usedVariables.append(reference.getExpression())
            variable = reference.getLevel0Variable()
            variables.add(variable)
            if variable.hasDerivedType() and variable.isTypeAvailable():
                types.add(variable.getType())
        
        testModule = subroutine.getName().getModuleName()
        modules = dict()    
        for variable in variables:
            moduleName = variable.getDeclaredIn()
            if moduleName != testModule or includeTestModule:
                if moduleName not in modules:
                    modules[moduleName] = []
                varName = variable.getName() 
                if varName != variable.getOriginalName():
                    varName += ' => ' + variable.getOriginalName()
                modules[moduleName].append(varName)
        for typE in types:
            moduleName = typE.getDeclaredIn()
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
            moduleName = typE.getDeclaredIn()
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
            refModule = variable.getDeclaredIn()
            if refModule == moduleName:
                variableName = variable.getOriginalName().lower()
                if variableName not in variables and not variable.isPublic() and variableName not in publicElements:
                    self.exports += variableName + ", "
                    variables.add(variableName)
            if variable.hasDerivedType() and variable.isTypeAvailable():
                typE = variable.getType()
                refModule = typE.getDeclaredIn()
                if refModule == moduleName:
                    typeName = typE.getName().lower()
                    if typeName not in types and typeName not in publicElements:
                        self.exports += typeName + ", "
                        types.add(typeName)
        self.exports = self.exports.strip(', ')
        
        if self.exports == 'PUBLIC ::':
            self.exports = ''
