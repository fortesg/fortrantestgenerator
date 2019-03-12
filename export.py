import os
from generator import CodeGenerator 
from assertions import assertType, assertTypeAll
from source import SourceFiles, SourceFile, SubroutineFullName, VariableReference
from templatenamespace import ExportTemplatesNameSpace
from backup import BackupFileFinder
from printout import printLine, printWarning
from callgraph import CallGraph
from typefinder import TypeCollection

class ExportCodeGenerator(CodeGenerator):
    
    AFTER_USE_TEMPLATE_PART = 'exportAfterUse'
    BEFORE_CONTAINS_TEMPLATE_PART = 'exportBeforeContains'
    
    def __init__(self, sourceFiles, modifySourceFiles, templatePath, graphBuilder, postProcessor, backupFinder, settings):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templatePath, 'templatePath', str)
        if not os.path.isfile(templatePath):
            raise IOError("Template file not found: " + templatePath)
        assertType(backupFinder, 'backupFinder', BackupFileFinder)

        super(ExportCodeGenerator, self).__init__(sourceFiles, templatePath, graphBuilder, postProcessor, settings)
        self.__modifySourceFiles = modifySourceFiles     
        self.__backupFinder = backupFinder 

    def addCode(self, subroutineFullName, typeArgumentReferences, typeResultReferences, globalsReferences, callgraph, types):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)
        assertTypeAll(typeArgumentReferences, 'typeArgumentReferences', VariableReference)
        assertTypeAll(typeResultReferences, 'typeResultReferences', VariableReference)
        assertTypeAll(globalsReferences, 'globalsReferences', VariableReference)
        assertType(callgraph, 'callgraph', CallGraph)
        assertType(types, 'types', TypeCollection)
        
        printLine('Add Code to Used Modules', indent = 1)
        self.__backupFinder.setBackupSuffixPrefix(self.__backupFinder.EXPORT_SUFFIX_PREFIX)
        refModules = list(self.__getModulesFromCallgraph(callgraph).union(self.__extractModulesFromVariableReferences(globalsReferences)))
        refModules.sort(reverse = True)
        for refModule in refModules:
            usedSourceFile = refModule.getSourceFile()
            usedSourceFilePath = usedSourceFile.getPath()
            if usedSourceFilePath.endswith(self.__backupFinder.getBackupSuffix()):
                usedSourceFilePath = usedSourceFilePath.replace(self.__backupFinder.getBackupSuffix(), CodeGenerator.DEFAULT_SUFFIX)
                usedSourceFile = SourceFile(usedSourceFilePath, usedSourceFile.isPreprocessed())
            self.__backupFinder.setBackupSuffixPrefix(BackupFileFinder.EXPORT_SUFFIX_PREFIX)
            backup = self.__backupFinder.create(usedSourceFilePath)
            subroutine = self._findSubroutine(subroutineFullName)
            self._setTypesToSubroutineVariables(subroutine, types)
            exportTemplateNameSpace = ExportTemplatesNameSpace(refModule, typeArgumentReferences, typeResultReferences, globalsReferences, subroutine, callgraph, self._postProcessor)
            lastLine = refModule.getContainsLineNumber()
            if lastLine < 0:
                lastLine = refModule.getLastLineNumber()
            result = self._processTemplate(usedSourceFilePath, lastLine - 1, self.BEFORE_CONTAINS_TEMPLATE_PART, exportTemplateNameSpace)
            lastUseLineNumber = refModule.getLastUseLineNumber()
            lastUseLineNumber = self._shiftLineNumberByPreprocesserorEndifs(refModule, lastUseLineNumber)
            result = self._processTemplate(usedSourceFilePath, lastUseLineNumber, self.AFTER_USE_TEMPLATE_PART, exportTemplateNameSpace) or result
            if backup and not result:
                self.__backupFinder.remove(usedSourceFilePath)

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
    
    def __getModulesFromCallgraph(self, callgraph):
        modules = set()
        for moduleName in callgraph.getAllModuleNames():
            module = self.__modifySourceFiles.findModule(moduleName)
            if module is not None:
                modules.add(module)
            elif moduleName not in self._settings.excludeModules:
                printWarning('Module not found: ' + moduleName)
        
        return modules
