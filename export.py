import os
from generator import CodeGenerator 
from assertions import assertType
from source import SourceFiles, SourceFile, SubroutineFullName
from templatenamespace import ExportNameSpace

class ExportCodeGenerator(CodeGenerator):
    
    EXPORT_TEMPLATE_PART = 'export'
    
    def __init__(self, sourceFiles, modifySourceFiles, templatePath, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignoreRegex = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templatePath, 'templatePath', str)
        if not os.path.isfile(templatePath):
            raise IOError("Template file not found: " + templatePath)

        super(ExportCodeGenerator, self).__init__(sourceFiles, templatePath, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)
        self.__modifySourceFiles = modifySourceFiles      

    def addCode(self, subroutine, typeArgumentReferences, globalsReferences):  # @UnusedVariable
        print "  Add Code to Used Modules"
        moduleName = subroutine.getModuleName()
        refModules = list(self.__extractModulesFromVariableReferences(globalsReferences))
        refModules.sort(reverse = True)
        for refModule in refModules:
            refModuleName = refModule.getName() 
            if refModuleName != moduleName:
                if not refModule.isPublic():
                    usedSourceFile = refModule.getSourceFile()
                    usedSourceFilePath = usedSourceFile.getPath()
                    if usedSourceFilePath.endswith(self._backupSuffix):
                        usedSourceFilePath = usedSourceFilePath.replace(self._backupSuffix, CodeGenerator.DEFAULT_SUFFIX)
                        usedSourceFile = SourceFile(usedSourceFilePath, usedSourceFile.isPreprocessed())
                    backup = self._createFileBackup(usedSourceFilePath)
                    exportTemplateNameSpace = ExportNameSpace(refModuleName, usedSourceFile, globalsReferences)
                    result = self._processTemplate(usedSourceFilePath, refModule.getContainsLineNumber() - 1, self.EXPORT_TEMPLATE_PART, exportTemplateNameSpace)
                    if backup and not result:
                        self._removeFileBackup(usedSourceFilePath)

    def _findSubroutine(self, subroutineFullName):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)
        
        return self.__modifySourceFiles.findSubroutine(subroutineFullName)
    
    def __extractModulesFromVariableReferences(self, references):
        modules = set()
        for ref in references:
            declaredIn = ref.getDeclaredIn() 
            if declaredIn is not None:
                module = declaredIn.getModule()
                if module is not None:
                    if self.__modifySourceFiles != self._sourceFiles:
                        module = self.__modifySourceFiles.findModule(module.getName())
                    if module is not None:
                        modules.add(module)
        
        return modules
