import os
from generator import CodeGenerator 
from utils import assertType
from source import SourceFiles
from capture import CaptureCodeGenerator
from replay import ReplayCodeGenerator

class CombinedCodeGenerator(CodeGenerator):
    
    def __init__(self, sourceFiles, modifySourceFiles, templateDir, testSourceDir, testDataDir, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignoreRegex = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(modifySourceFiles, 'modifySourceFiles', SourceFiles)
        assertType(templateDir, 'templateDir', str)
        if not os.path.isdir(templateDir):
            raise IOError("Not a directory: " + templateDir);
        assertType(testDataDir, 'testDataDir', str)
        if not os.path.isdir(testDataDir):
            raise IOError("Not a directory: " + testDataDir);

        super(CombinedCodeGenerator, self).__init__(sourceFiles, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)
        self.__capture = CaptureCodeGenerator(sourceFiles, modifySourceFiles, templateDir, testDataDir, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)        
        self.__replay = ReplayCodeGenerator(sourceFiles, templateDir, testSourceDir, testDataDir, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)        
        
    def addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        self.__capture.addCode(subroutine, typeArgumentReferences, globalsReferences)
        self.__replay.addCode(subroutine, typeArgumentReferences, globalsReferences)