import os
import re
from generator import CodeGenerator 
from assertions import assertType, assertTypeAll
from source import SourceFiles, VariableReference, SubroutineFullName
from templatenamespace import ReplayTemplatesNameSpace
from printout import printLine

class ReplayCodeGenerator(CodeGenerator):
    
    TEST_TEMPLATE_PART = 'replay'
    TEMP_TEST_FILE = 'ftg_temp_test.f90'
    
    def __init__(self, sourceFiles, templatePath, testSourceDir, testDataDir, graphBuilder, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignoreRegex = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templatePath, 'templatePath', str)
        if not os.path.isfile(templatePath):
            raise IOError("Template file not found: " + templatePath)
        assertType(testDataDir, 'testDataDir', str)
        if not os.path.isdir(testDataDir):
            raise IOError("Not a directory: " + testDataDir);

        super(ReplayCodeGenerator, self).__init__(sourceFiles, templatePath, graphBuilder, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)        
        self.__testSourceDir = testSourceDir
        self.__testDataDir = testDataDir
        
    def addCode(self, subroutineFullName, typeArgumentReferences, typeResultReferences, globalsReferences):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)
        assertTypeAll(typeArgumentReferences, 'typeArgumentReferences', VariableReference)
        assertTypeAll(typeResultReferences, 'typeResultReferences', VariableReference)
        assertTypeAll(globalsReferences, 'globalsReferences', VariableReference)
        
        printLine('Create code in new test source file', indent = 1)
        tempTestFile = os.path.join(self.__testSourceDir, self.TEMP_TEST_FILE)

        printLine('Create file ' + tempTestFile, indent = 2)
        self._writeFile(tempTestFile, [])
        
        subroutine = self._findSubroutine(subroutineFullName)
        templateNameSpace = ReplayTemplatesNameSpace(subroutine, typeArgumentReferences, typeResultReferences, globalsReferences, self.__testDataDir)
        self._processTemplate(tempTestFile, 0, self.TEST_TEMPLATE_PART, templateNameSpace)
        
        testModuleName = self.__findModuleNameInTestFile(tempTestFile)
        if testModuleName is not None:
            testFilePath = os.path.join(self.__testSourceDir, testModuleName + '.f90')
            printLine('Rename file to ' + testFilePath, indent = 2)
            os.rename(tempTestFile, testFilePath)
        
    def __findModuleNameInTestFile(self, testFilePath):
        
        regEx = re.compile(r'^((MODULE)|(PROGRAM))\s+(?P<name>[a-z0-9_]{1,63})(\s+.*)?$', re.IGNORECASE)
        
        for line in self._readFile(testFilePath):
            line = line.strip()
            regExMatch = regEx.match(line)
            if regExMatch is not None:
                return regExMatch.group('name')
            
        return None