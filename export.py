import os
from generator import CodeGenerator 
from assertions import assertType, assertTypeAll
from source import SourceFiles, SourceFile, SubroutineFullName, VariableReference
from templatenamespace import ExportNameSpace
from backup import BackupFileFinder
from printout import printLine
from callgraph import CallGraph
from typefinder import TypeCollection

class ExportCodeGenerator(CodeGenerator):
    
    EXPORT_TEMPLATE_PART = 'export'
    
    def __init__(self, sourceFiles, modifySourceFiles, templatePath, graphBuilder, postProcessor, backupFinder, settings):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templatePath, 'templatePath', str)
        if not os.path.isfile(templatePath):
            raise IOError("Template file not found: " + templatePath)
        assertType(backupFinder, 'backupFinder', BackupFileFinder)

        super(ExportCodeGenerator, self).__init__(sourceFiles, templatePath, graphBuilder, postProcessor, settings)
        self.__modifySourceFiles = modifySourceFiles     
        self.__backupFinder = backupFinder 

    def addCode(self, subroutineFullName, typeArgumentReferences, typeResultReferences, globalsReferences, callgraph, types):  # @UnusedVariable
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)
        assertTypeAll(typeArgumentReferences, 'typeArgumentReferences', VariableReference)
        assertTypeAll(typeResultReferences, 'typeResultReferences', VariableReference)
        assertTypeAll(globalsReferences, 'globalsReferences', VariableReference)
        assertType(callgraph, 'callgraph', CallGraph)
        assertType(types, 'types', TypeCollection)
        
        printLine('Add Code to Used Modules', indent = 1)
        self.__backupFinder.setBackupSuffixPrefix(self.__backupFinder.EXPORT_SUFFIX_PREFIX)
        refModules = list(self.__extractModulesFromVariableReferences(globalsReferences))
        refModules.sort(reverse = True)
        for refModule in refModules:
            refModuleName = refModule.getName() 
            if not refModule.isPublic():
                usedSourceFile = refModule.getSourceFile()
                usedSourceFilePath = usedSourceFile.getPath()
                if usedSourceFilePath.endswith(self.__backupFinder.getBackupSuffix()):
                    usedSourceFilePath = usedSourceFilePath.replace(self.__backupFinder.getBackupSuffix(), CodeGenerator.DEFAULT_SUFFIX)
                    usedSourceFile = SourceFile(usedSourceFilePath, usedSourceFile.isPreprocessed())
                self.__backupFinder.setBackupSuffixPrefix(BackupFileFinder.EXPORT_SUFFIX_PREFIX)
                backup = self.__backupFinder.create(usedSourceFilePath)
                subroutine = self._findSubroutine(subroutineFullName)
                self._setTypesToSubroutineVariables(subroutine, types)
                exportTemplateNameSpace = ExportNameSpace(refModuleName, usedSourceFile, globalsReferences, subroutine, callgraph, self._postProcessor)
                result = self._processTemplate(usedSourceFilePath, refModule.getContainsLineNumber() - 1, self.EXPORT_TEMPLATE_PART, exportTemplateNameSpace)
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
