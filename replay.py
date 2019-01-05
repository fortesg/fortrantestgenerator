import os
import re
from generator import CodeGenerator 
from assertions import assertType
from source import SourceFiles
from templatenamespace import ReplayTemplatesNameSpace

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
        
    def addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        print "  Create code in new test source file"
        
        tempTestFile = os.path.join(self.__testSourceDir, self.TEMP_TEST_FILE)
        
        print "      Create file " + tempTestFile
        self._writeFile(tempTestFile, [])
        
        templateNameSpace = ReplayTemplatesNameSpace(subroutine, typeArgumentReferences, globalsReferences, self.__testDataDir)
        self._processTemplate(tempTestFile, 0, self.TEST_TEMPLATE_PART, templateNameSpace)
        
        testModuleName = self.__findModuleNameInTestFile(tempTestFile)
        if testModuleName is not None:
            testFilePath = os.path.join(self.__testSourceDir, testModuleName + '.f90')
            print "      Rename file to " + testFilePath
            os.rename(tempTestFile, testFilePath)
        
    def __findModuleNameInTestFile(self, testFilePath):
        
        regEx = re.compile(r'^((MODULE)|(PROGRAM))\s+(?P<name>[a-z0-9_]{1,63})(\s+.*)?$', re.IGNORECASE)
        
        for line in self._readFile(testFilePath):
            line = line.strip()
            regExMatch = regEx.match(line)
            if regExMatch is not None:
                return regExMatch.group('name')
            
        return None