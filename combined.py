import os
from generator import CodeGenerator 
from assertions import assertType
from source import SourceFiles
from capture import CaptureCodeGenerator
from replay import ReplayCodeGenerator
from export import ExportCodeGenerator
from backup import BackupFileFinder

class CombinedCodeGenerator(CodeGenerator):
    
    def __init__(self, capture, replay, sourceFiles, modifySourceFiles, templatePath, testSourceDir, testDataDir, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignoreRegex = ''):
        assertType(capture, 'capture', bool)
        assertType(replay, 'replay', bool)
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(modifySourceFiles, 'modifySourceFiles', SourceFiles)
        assertType(templatePath, 'templatePath', str)
        if not os.path.isfile(templatePath):
            raise IOError("Template file not found: " + templatePath)
        assertType(testDataDir, 'testDataDir', str)
        if not os.path.isdir(testDataDir):
            raise IOError("Not a directory: " + testDataDir);

        super(CombinedCodeGenerator, self).__init__(sourceFiles, templatePath, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)
        self.__export = ExportCodeGenerator(sourceFiles, modifySourceFiles, templatePath, graphBuilder, BackupFileFinder.EXPORT_SUFFIX_PREFIX + backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)
        self.__capture = None
        self.__replay = None        
        if capture:
            self.__capture = CaptureCodeGenerator(sourceFiles, modifySourceFiles, templatePath, testDataDir, graphBuilder, BackupFileFinder.CAPTURE_SUFFIX_PREFIX + backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)
        if replay:
            self.__replay = ReplayCodeGenerator(sourceFiles, templatePath, testSourceDir, testDataDir, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)
    
    def addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        self.__export.addCode(subroutine, typeArgumentReferences, globalsReferences)
        if self.__capture:
            self.__capture.addCode(subroutine, typeArgumentReferences, globalsReferences)
        if self.__replay:
            self.__replay.addCode(subroutine, typeArgumentReferences, globalsReferences)