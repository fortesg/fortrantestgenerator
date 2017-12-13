import os
import re
from generator import CodeGenerator 
from assertions import assertType
from source import SourceFiles
from templatenamespace import ReplayTemplatesNameSpace

class ReplayCodeGenerator(CodeGenerator):
    
    TEST_TEMPLATE = 'replay.test.tmpl'
    TEMP_TEST_FILE = 'ftg_temp_test.f90'
    
    def __init__(self, sourceFiles, templateDir, testSourceDir, testDataDir, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignoreRegex = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templateDir, 'templateDir', str)
        if not os.path.isdir(templateDir):
            raise IOError("Not a directory: " + templateDir);
        assertType(testDataDir, 'testDataDir', str)
        if not os.path.isdir(testDataDir):
            raise IOError("Not a directory: " + testDataDir);

        super(ReplayCodeGenerator, self).__init__(sourceFiles, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)        
        self.__testTemplate = os.path.join(templateDir, self.TEST_TEMPLATE)
        self.__testSourceDir = testSourceDir
        self.__testDataDir = testDataDir
        
    def addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        print "  Create code"
        print "    ...in new test source file"
        
        tempTestFile = os.path.join(self.__testSourceDir, self.TEMP_TEST_FILE)
        
        print "      Create file " + tempTestFile
        self._writeFile(tempTestFile, [])
        
        templateNameSpace = ReplayTemplatesNameSpace(subroutine, typeArgumentReferences, globalsReferences, self.__testDataDir)
        self._processTemplate(tempTestFile, 0, self.__testTemplate, templateNameSpace)
        
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