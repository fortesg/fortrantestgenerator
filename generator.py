import os
import sys
import re
from Cheetah.Template import Template
from Cheetah import ImportHooks
from assertions import assertType, assertTypeAll
from source import SourceFiles, SubroutineFullName, Subroutine
from trackvariable import VariableTracker
from globals import GlobalVariableTracker
from usetraversal import UseTraversal
from supertypes import CallGraphBuilder
from printout import printLine, printInline, printWarning
from typefinder import TypeCollection
from postprocessor import CodePostProcessor
import time

class CodeGenerator(object):
    
    DEFAULT_SUFFIX = '.f90'
    
    def __init__(self, sourceFiles, templatePath, graphBuilder, postProcessor, settings):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templatePath, 'templatePath', str)
        if not os.path.isfile(templatePath):
            raise IOError("Template file not found: " + templatePath)
        assertType(graphBuilder, 'graphBuilder', CallGraphBuilder)
        assertType(postProcessor, 'postProcessor', CodePostProcessor)
        
        assertType(settings, 'settings', CodeGeneratorSettings)
        assertTypeAll(settings.excludeModules, 'settings.excludeModules', str)
        assertTypeAll(settings.ignoredModulesForGlobals, 'settings.ignoredModulesForGlobals', str)        
        assertTypeAll(settings.ignoredTypes, 'settings.ignoredTypes', str)        
        assertType(settings.ignorePrefix, 'settings.ignorePrefix', str)        
        assertType(settings.abstractTypes, 'settings.abstractTypes', dict)        
        assertTypeAll(settings.abstractTypes, 'settings.abstractTypes', str)        
        assertType(settings.measureTime, 'settings.measureTime', bool)        
        assertType(settings.clearCache, 'settings.clearCache', bool)        
        
        self._sourceFiles = sourceFiles
        self.__templatePath = templatePath
        self.__graphBuilder = graphBuilder
        self._postProcessor = postProcessor
        self._settings = settings
        
        self.__excludeModules = excludeModules
        self.__ignoredModulesForGlobals = ignoredModulesForGlobals
        self.__ignoredTypes = ignoredTypes
        self.__ignorePrefix = ignorePrefix
        self.__abstractTypes = abstractTypes
        self.__measureTime = measureTime
        
        templateDir = os.path.dirname(os.path.realpath(self.__templatePath))
        templateDirParent = os.path.abspath(os.path.join(templateDir, os.pardir))
        for name in os.listdir(templateDirParent):
            templateDirSibling = os.path.join(templateDirParent, name)
            if os.path.isdir(templateDirSibling):
                sys.path.append(templateDirSibling)
        ImportHooks.install()
        
    def generate(self, subroutineFullName):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)

        if self.__ignorePrefix != '':
            ignoreRegex = re.compile('^' + self.__ignorePrefix + subroutineFullName.getSimpleName() + '_.*$')
        else:
            ignoreRegex = None
            
        subroutine = self._findSubroutine(subroutineFullName)
        if subroutine is None:
            raise LookupError("Subroutine not found: " + str(subroutineFullName))

        if self.__measureTime: self.__initTime()
        
        printLine('Build Call Graph', indent = 1)
        callgraph = self.__graphBuilder.buildCallGraph(subroutineFullName)

        if self.__measureTime: self.__time(1)
        
        printLine('Find Interfaces and Types', indent = 1)
        useTraversal = UseTraversal(self._sourceFiles, self.__excludeModules, self.__abstractTypes)
        useTraversal.parseModules(callgraph.getRoot())
        interfaces = useTraversal.getInterfaces()
        types = useTraversal.getTypes()
        
        if self.__measureTime: self.__time(1)

        printLine('Analyse Source Code', indent = 1)
        argumentTracker = VariableTracker(self._sourceFiles, self.__excludeModules, self.__ignoredTypes, interfaces, types, callGraphBuilder = self.__graphBuilder)
        argumentTracker.setIgnoreRegex(ignoreRegex)
        
        printLine('Find References to Type Argument Members', indent = 2)
        typeArgumentReferences = argumentTracker.trackDerivedTypeArguments(callgraph)
        
        subroutine = self._findSubroutine(subroutineFullName)
        if subroutine.isFunction() and subroutine.getResultVariable().hasDerivedType():
            printLine('Find References to Type Result', indent = 2)
            typeResultReferences = argumentTracker.trackDerivedTypeResult(callgraph)
        else:
            typeResultReferences = []
        
        printLine('Find References to Global Variables', indent = 2)
        globalsTracker = GlobalVariableTracker(self._sourceFiles, self.__excludeModules, self.__ignoredModulesForGlobals, self.__ignoredTypes, interfaces, types, callGraphBuilder = self.__graphBuilder)
        globalsTracker.setIgnoreRegex(ignoreRegex)
        globalsReferences = globalsTracker.trackGlobalVariables(callgraph)
        
        sourceFile = subroutine.getSourceFile()
        sourceFilePath = sourceFile.getPath()
        if not os.path.isfile(sourceFile.getPath()):
            raise IOError("File not found: " + sourceFilePath);
        
        if self.__measureTime: self.__time(1)
        
        self.addCode(subroutineFullName, typeArgumentReferences, typeResultReferences, globalsReferences, callgraph, types)
        
        if self.__measureTime: self.__time(1, True, 1)
        
    def addCode(self, subroutineFullName, typeArgumentReferences, typeResultReferences, globalsReferences, callgraph, types):
        raise NotImplementedError()
    
    def _findSubroutine(self, subroutineFullName):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)
        
        return self._sourceFiles.findSubroutine(subroutineFullName)
    
    def _setTypesToSubroutineVariables(self, subroutine, types):
        assertType(subroutine, 'subroutine', Subroutine)
        assertType(types, 'types', TypeCollection)
        
        for variable in subroutine.getVariables():
            if variable.hasDerivedType() and not variable.isTypeAvailable() and variable.getDerivedTypeName() in types:
                variable.setType(types[variable.getDerivedTypeName()])
    
    def _findModule(self, moduleName):
        assertType(moduleName, 'moduleName', str)
        
        return self._sourceFiles.findModule(moduleName);

    def _processTemplate(self, sourceFilePath, lineNumber, part, templateNameSpace):
        printInline('Process Template ' + os.path.basename(self.__templatePath) + ' [' + part + ']' + ' on file ' + sourceFilePath, indent = 2)
        source = self._readFile(sourceFilePath)
        codeToAdd = self.__loadTemplate(part, templateNameSpace)
        
        if codeToAdd:
            source = source[:lineNumber] + ["\n"] + [codeToAdd] + ["\n", "\n"] + source[lineNumber:]
            self._writeFile(sourceFilePath, source)
            printLine()
            return True
        else:
            printLine(' >>> EMPTY')
            return False
        
    def __loadTemplate(self, part, templateNameSpace):
        template = Template(file=self.__templatePath, searchList=[templateNameSpace])
        template.part = part
        text = str(template).strip()
        return self._postProcessor.process(text, part)
    
    def _readFile(self, path):
        f = open(path, 'r')
        lines = f.readlines()
        f.close()
        
        return lines
    
    def _writeFile(self, path, lines):
        f = open(path, 'w')
        f.writelines(lines)
        f.close()

    def __initTime(self):
        try:
            self.__startTime = time.perf_counter()  # @UndefinedVariable
            self.__lastTime = self.__startTime
        except AttributeError:
            printWarning('Time measurement not support with your Python version (<3.3)')
            self.__measureTime

    def __time(self, indent = 0, printTotal = False, totalIndent = 0):
        secFormat = '{:8.4f}'
        now = time.perf_counter()  # @UndefinedVariable
        printLine('*** Duration: ' + secFormat.format(now - self.__lastTime) + ' Seconds ***', indent = indent)
        if printTotal:
            printLine('****** Total: ' + secFormat.format(now - self.__startTime) + ' Seconds ***', indent = totalIndent)
        self.__lastTime = now
        
class CodeGeneratorSettings(object):
    
    def ___init__(self):
        self.excludeModules = []
        self.ignoredModulesForGlobals = []
        self.ignoredTypes   = []
        self.abstractTypes  = []
        self.measureTime    = False
        self.clearCache     = False

        