import os
from generator import CodeGenerator 
from assertions import assertType
from source import SourceFiles, SourceFile, SubroutineFullName
from templatenamespace import CaptureTemplatesNameSpace, ExportNameSpace
from linenumbers import LastSpecificationLineFinder

class CaptureCodeGenerator(CodeGenerator):
    
    AFTER_LAST_LINE_TEMPLATE = 'capture.aftersubroutine.tmpl'
    BEFORE_CONTAINS_TEMPLATE = 'capture.beforecontains.tmpl'
    AFTER_LAST_SPECIFICATION_TEMPLATE = 'capture.afterlastspecification.tmpl'
    BEFORE_END_TEMPLATE = 'capture.beforeend.tmpl'
    EXPORT_TEMPLATE = 'export.beforecontains.tmpl'
    
    def __init__(self, sourceFiles, modifySourceFiles, templateDir, testDataDir, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignoreRegex = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templateDir, 'templateDir', str)
        if not os.path.isdir(templateDir):
            raise IOError("Not a directory: " + templateDir);
        assertType(testDataDir, 'testDataDir', str)
        if not os.path.isdir(testDataDir):
            raise IOError("Not a directory: " + testDataDir);

        super(CaptureCodeGenerator, self).__init__(sourceFiles, graphBuilder, backupSuffix, excludeModules, ignoredModulesForGlobals, ignoredTypes, ignoreRegex)
        self.__modifySourceFiles = modifySourceFiles      
        
        self.__afterLastLineTemplate = os.path.join(templateDir, self.AFTER_LAST_LINE_TEMPLATE)
        self.__beforeContainsTemplate = os.path.join(templateDir, self.BEFORE_CONTAINS_TEMPLATE)
        self.__afterLastSpecificationTemplate = os.path.join(templateDir, self.AFTER_LAST_SPECIFICATION_TEMPLATE)
        self.__beforeEndTemplate = os.path.join(templateDir, self.BEFORE_END_TEMPLATE)
        self.__exportTemplate = os.path.join(templateDir, self.EXPORT_TEMPLATE)

        self.__testDataDir = testDataDir

    def addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        print "  Add Code"
        
        originalSourceFile = subroutine.getSourceFile()
        sourceFilePath = originalSourceFile.getPath()
        if sourceFilePath.endswith(self._backupSuffix):
            sourceFilePath = sourceFilePath.replace(self._backupSuffix, CodeGenerator.DEFAULT_SUFFIX)
            subroutine = SourceFile(sourceFilePath, originalSourceFile.isPreprocessed()).getSubroutine(subroutine.getName())
                    
        print "    ...to Module under Test"
        self._createFileBackup(sourceFilePath)
        templateNameSpace = CaptureTemplatesNameSpace(subroutine, typeArgumentReferences, globalsReferences, self.__testDataDir)
        # Reihenfolge wichtig: von unten nach oben!!!
        self._processTemplate(sourceFilePath, subroutine.getLastLineNumber(), self.__afterLastLineTemplate, templateNameSpace)
        lastLine = subroutine.getContainsLineNumber()
        if lastLine < 0:
            lastLine = subroutine.getLastLineNumber()
        self._processTemplate(sourceFilePath, lastLine - 1, self.__beforeEndTemplate, templateNameSpace)
        lastSpecificationLineNumber = subroutine.getLastSpecificationLineNumber()
        lastSpecificationLineNumber = self.__shiftLineNumberByPreprocesserorEndifs(subroutine, lastSpecificationLineNumber)
        self._processTemplate(sourceFilePath, lastSpecificationLineNumber, self.__afterLastSpecificationTemplate, templateNameSpace)
        self._processTemplate(sourceFilePath, subroutine.getModule().getContainsLineNumber() - 1, self.__beforeContainsTemplate, templateNameSpace)
        
        print "    ...to Used Modules"
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
                    result = self._processTemplate(usedSourceFilePath, refModule.getContainsLineNumber() - 1, self.__exportTemplate, exportTemplateNameSpace)
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
    
    def __shiftLineNumberByPreprocesserorEndifs(self, subroutine, fromLineNumber):
        found = False
        toLineNumber = -1
        for i, _, j in subroutine.getStatements():
            if found:
                toLineNumber = i
                break
            elif j >= fromLineNumber:
                found = True
                toLineNumber = j
        
        shiftedLineNumber = fromLineNumber
        for lineNumber in range(fromLineNumber, toLineNumber):
            line = subroutine.getLine(lineNumber)
            if line.strip() == '#endif':
                shiftedLineNumber = lineNumber
                
        return shiftedLineNumber