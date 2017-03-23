import os
import re
from generator import CodeGenerator 
from utils import assertType
from source import SourceFiles
from templatenamespace import ReplayTemplatesNameSpace

class ReplayCodeGenerator(CodeGenerator):
    
    TEST_TEMPLATE = 'replay.test.tmpl'
    TEMP_TEST_FILE = 'ftg_temp_test.f90'
    
    def __init__(self, sourceFiles, templateFolder, testSourceFolder, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignoreRegex = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templateFolder, 'templateFolder', str)
        if not os.path.isdir(templateFolder):
            raise IOError("Not a directory: " + templateFolder);

        super(ReplayCodeGenerator, self).__init__(sourceFiles, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)        
        self.__testTemplate = templateFolder.rstrip('/') + '/' + self.TEST_TEMPLATE
        self.__testSourceFolder = testSourceFolder.rstrip('/')
        
    def _addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        print "  Create code"
        print "    ...in new test source file"
        
        tempTestFile = self.__testSourceFolder + '/' + self.TEMP_TEST_FILE
        
        print "      Create file " + tempTestFile
        self._writeFile(tempTestFile, [])
        
        templateNameSpace = ReplayTemplatesNameSpace(subroutine, typeArgumentReferences, globalsReferences)
        self._processTemplate(tempTestFile, 0, self.__testTemplate, templateNameSpace)
        
        testModuleName = self.__findModuleNameInTestFile(tempTestFile)
        if testModuleName is not None:
            testFilePath = self.__testSourceFolder + "/" + testModuleName + '.f90'
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