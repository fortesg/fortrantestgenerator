import os
from generator import CodeGenerator 
from utils import assertType
from source import SourceFiles, SourceFile
from templatenamespace import CaptureTemplatesNameSpace, ExportNameSpace

class CaptureCodeGenerator(CodeGenerator):
    
    AFTER_LAST_LINE_TEMPLATE = 'capture.aftersubroutine.tmpl'
    BEFORE_CONTAINS_TEMPLATE = 'capture.beforecontains.tmpl'
    AFTER_LAST_USE_TEMPLATE = 'capture.afterlastuse.tmpl'
    AFTER_LAST_SPECIFICATION_TEMPLATE = 'capture.afterlastspecification.tmpl'
    BEFORE_END_TEMPLATE = 'capture.beforeend.tmpl'
    EXPORT_TEMPLATE = 'export.beforecontains.tmpl'
    
    def __init__(self, sourceFiles, templateDir, testDataDir, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignoreRegex = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templateDir, 'templateDir', str)
        if not os.path.isdir(templateDir):
            raise IOError("Not a directory: " + templateDir);
        assertType(testDataDir, 'testDataDir', str)
        if not os.path.isdir(testDataDir):
            raise IOError("Not a directory: " + testDataDir);

        super(CaptureCodeGenerator, self).__init__(sourceFiles, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)
        
        self.__afterLastLineTemplate = templateDir + '/' + self.AFTER_LAST_LINE_TEMPLATE
        self.__beforeContainsTemplate = templateDir + '/' + self.BEFORE_CONTAINS_TEMPLATE
        self.__afterLastUseTemplate = templateDir + '/' + self.AFTER_LAST_USE_TEMPLATE
        self.__afterLastSpecificationTemplate = templateDir + '/' + self.AFTER_LAST_SPECIFICATION_TEMPLATE
        self.__beforeEndTemplate = templateDir + '/' + self.BEFORE_END_TEMPLATE
        self.__exportTemplate = templateDir + '/' + self.EXPORT_TEMPLATE
        
        self.__testDataDir = testDataDir

        
    def addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        print "  Add Code"
        
        sourceFilePath = subroutine.getSourceFile().getPath()
        if sourceFilePath.endswith(self._backupSuffix):
            sourceFilePath = sourceFilePath.replace(self._backupSuffix, CodeGenerator.DEFAULT_SUFFIX)
            subroutine = SourceFile(sourceFilePath).getSubroutine(subroutine.getName())
                    
        print "    ...to Module under Test"
        self._createFileBackup(sourceFilePath)
        templateNameSpace = CaptureTemplatesNameSpace(subroutine, typeArgumentReferences, globalsReferences, self.__testDataDir)
        # Reihenfolge wichtig: von unten nach oben!!!
        self._processTemplate(sourceFilePath, subroutine.getLastLineNumber(), self.__afterLastLineTemplate, templateNameSpace)
        lastLine = subroutine.getContainsLineNumber()
        if lastLine < 0:
            lastLine = subroutine.getLastLineNumber()
        self._processTemplate(sourceFilePath, lastLine - 1, self.__beforeEndTemplate, templateNameSpace)
        self._processTemplate(sourceFilePath, subroutine.getLastSpecificationLineNumber(), self.__afterLastSpecificationTemplate, templateNameSpace)
        self._processTemplate(sourceFilePath, subroutine.getModule().getContainsLineNumber() - 1, self.__beforeContainsTemplate, templateNameSpace)
        
        print "    ...to Used Modules"
        moduleName = subroutine.getModuleName()
        refModules = list(self._extractModulesFromVariableReferences(globalsReferences))
        refModules.sort(reverse = True)
        for refModule in refModules:
            refModuleName = refModule.getName() 
            if refModuleName != moduleName:
                usedSourceFile = refModule.getSourceFile()
                if not usedSourceFile.isPublic():
                    usedSourceFilePath = usedSourceFile.getPath()
                    if usedSourceFilePath.endswith(self._backupSuffix):
                        usedSourceFilePath = usedSourceFilePath.replace(self._backupSuffix, CodeGenerator.DEFAULT_SUFFIX)
                        usedSourceFile = SourceFile(usedSourceFilePath)
                    backup = self._createFileBackup(usedSourceFilePath)
                    exportTemplateNameSpace = ExportNameSpace(refModuleName, usedSourceFile, globalsReferences)
                    result = self._processTemplate(usedSourceFilePath, refModule.getContainsLineNumber() - 1, self.__exportTemplate, exportTemplateNameSpace)
                    if backup and not result:
                        self._removeFileBackup(usedSourceFilePath)
