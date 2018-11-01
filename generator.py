import os
import sys
import shutil
from Cheetah.Template import Template
from Cheetah import ImportHooks
from assertions import assertType, assertTypeAll
from source import SourceFiles, SubroutineFullName
from trackvariable import VariableTracker
from globals import GlobalVariableTracker
from usetraversal import UseTraversal
from supertypes import CallGraphBuilder
import re

class CodeGenerator(object):
    
    MAX_LINE_LENGTH = 132
    INDENT_LENGTH = 2
    DEFAULT_SUFFIX = '.f90'
    
    def __init__(self, sourceFiles, templatePath, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignorePrefix = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templatePath, 'templatePath', str)
        if not os.path.isfile(templatePath):
            raise IOError("Template file not found: " + templatePath)
        assertType(graphBuilder, 'graphBuilder', CallGraphBuilder)
        assertType(backupSuffix, 'backupSuffix', str)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertTypeAll(ignoredModulesForGlobals, 'ignoredModulesForGlobals', str)        
        assertTypeAll(ignoredTypes, 'ignoredTypes', str)        
        
        self._sourceFiles = sourceFiles
        self.__templatePath = templatePath
        self.__graphBuilder = graphBuilder
        self._backupSuffix = '.' + backupSuffix.lstrip('.')
        self.__excludeModules = excludeModules
        self.__ignoredModulesForGlobals = ignoredModulesForGlobals;
        self.__ignoredTypes = ignoredTypes;
        self.__ignorePrefix = ignorePrefix; 
        
        templateDir = os.path.dirname(os.path.realpath(self.__templatePath))
        templateDirParent = os.path.abspath(os.path.join(templateDir, os.pardir))
        for name in os.listdir(templateDirParent):
            templateDirSibling = os.path.join(templateDirParent, name)
            if os.path.isdir(templateDirSibling):
                sys.path.append(templateDirSibling)
        ImportHooks.install()
        
    def generate(self, subroutineFullName):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)

        if self.__ignorePrefix != '':
            ignoreRegex = re.compile('^' + self.__ignorePrefix + subroutineFullName.getSimpleName() + '_.*$')
        else:
            ignoreRegex = None

        print "  Build Call Graph"
        callGraph = self.__graphBuilder.buildCallGraph(subroutineFullName)
        
        print "  Find Interfaces"
        useTraversal = UseTraversal(self._sourceFiles, self.__excludeModules)
        useTraversal.parseModules(callGraph.getRoot())
        interfaces = useTraversal.getInterfaces()
        types = useTraversal.getTypes()

        print "  Analyse Source Code"
        print "    Find References to Type Argument Members"
        argumentTracker = VariableTracker(self._sourceFiles, self.__excludeModules, self.__ignoredTypes, interfaces, types)
        argumentTracker.setIgnoreRegex(ignoreRegex)
        typeArgumentReferences = argumentTracker.trackDerivedTypeArguments(callGraph)
        print "    Find References to Global Variables"
        globalsTracker = GlobalVariableTracker(self._sourceFiles, self.__excludeModules, self.__ignoredModulesForGlobals, self.__ignoredTypes, interfaces, types)
        globalsTracker.setIgnoreRegex(ignoreRegex)
        globalsReferences = globalsTracker.trackGlobalVariables(callGraph)
        
        subroutine = self._findSubroutine(subroutineFullName)
        if subroutine is None:
            raise LookupError("Subroutine not found: " + str(subroutineFullName))
        sourceFile = subroutine.getSourceFile()
        sourceFilePath = sourceFile.getPath()
        if not os.path.isfile(sourceFile.getPath()):
            raise IOError("File not found: " + sourceFilePath);
        
        
        self.addCode(subroutine, typeArgumentReferences, globalsReferences)
        
    def addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        raise NotImplementedError()
    
    def _findSubroutine(self, subroutineFullName):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)
        
        return self._sourceFiles.findSubroutine(subroutineFullName)
    
    def _findModule(self, moduleName):
        assertType(moduleName, 'moduleName', str)
        
        return self._sourceFiles.findModule(moduleName);
        
    def _createFileBackup(self, originalPath):
        print "      Create File Backup of " + originalPath,
        backupPath = originalPath.replace(CodeGenerator.DEFAULT_SUFFIX, self._backupSuffix)
        backupPath = backupPath.replace(CodeGenerator.DEFAULT_SUFFIX.upper(), self._backupSuffix)
        if (backupPath == originalPath):
            backupPath = originalPath + self._backupSuffix
        if not os.path.exists(backupPath):
            shutil.copyfile(originalPath, backupPath)
            print
            return True
        elif not os.path.exists(originalPath):
            shutil.copyfile(backupPath, originalPath)
            print
            return True
        else:
            print " >>> ALREADY EXISTS"
            return False
        
    def _removeFileBackup(self, originalPath):
        print "      Remove File Backup of " + originalPath,
        backupPath = originalPath.replace(CodeGenerator.DEFAULT_SUFFIX, self._backupSuffix)
        backupPath = backupPath.replace(CodeGenerator.DEFAULT_SUFFIX.upper(), self._backupSuffix)
        if (backupPath == originalPath):
            backupPath = originalPath + self._backupSuffix
        if os.path.exists(backupPath):
            os.remove(backupPath)
            print
            return True
        else:
            print " >>> NOT FOUND"
            return False

    def _processTemplate(self, sourceFilePath, lineNumber, part, templateNameSpace):
        print "      Process Template " + os.path.basename(self.__templatePath) + " [" + part + "]" + " on file " + sourceFilePath, 
        source = self._readFile(sourceFilePath)
        codeToAdd = self.__loadTemplate(part, templateNameSpace)
        
        if codeToAdd:
            source = source[:lineNumber] + ["\n"] + [codeToAdd] + ["\n", "\n"] + source[lineNumber:]
            self._writeFile(sourceFilePath, source)
            print
            return True
        else:
            print " >>> EMPTY"
            return False
        
    def __loadTemplate(self, part, templateNameSpace):
        template = Template(file=self.__templatePath, searchList=[templateNameSpace])
        template.part = part
        return self._breakLines(self._indent(str(template).strip()))
    
    def _indent(self, text):
        if not text:
            return text
        
        beginWords = ('PROGRAM ', 'MODULE ', 'SUBROUTINE ', 'FUNCTION ', 'INTERFACE ', 'TYPE ', 'DO ', 'SELECT ')
        beginWordsBack = (' THEN')
        endWords = ('END ', 'ENDIF', 'ENDDO', 'ENDFUNCTION', 'ENDSELECT')
        borderWords = ('CONTAINS', 'ELSE', 'ELSEIF')
        
        originalLines = text.split("\n")
        firstLine = originalLines[0]
        baseIndent = len(firstLine) - len(firstLine.lstrip())
        
        lines = []
        indent = baseIndent
        for line in text.split("\n"):
            line = line.strip()
            lineUpper = line.upper()
            if lineUpper.startswith(endWords) or lineUpper.startswith(borderWords):
                indent = max(baseIndent, indent - CodeGenerator.INDENT_LENGTH)
            if not line.startswith('#'):
                line = (' ' * indent) + line
            lines.append(line)
            if lineUpper.startswith(beginWords) or lineUpper.endswith(beginWordsBack) or lineUpper.startswith(borderWords):
                indent = indent + CodeGenerator.INDENT_LENGTH
                
        return "\n".join(lines)
        
    def _breakLines(self, text):
        lines = []
        for line in text.split("\n"):
            stringMask = self.stringMask(line)
            while len(line) > CodeGenerator.MAX_LINE_LENGTH:
                i = CodeGenerator.MAX_LINE_LENGTH - 2
                while stringMask[i]:
                    i -= 1
                if line[i] != ' ':
                    i -= 1
                    while "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_=>".find(line[i]) >= 0:
                        i -= 1
                lines.append(line[:i + 1].rstrip() + " &")
                indent =  ' ' * (len(line) - len(line.lstrip())) + "&  "
                line = indent + line[i + 1:]
            else:        
                lines.append(line)
        return "\n".join(lines)
    
    def stringMask(self, line):
        mask = []
        inString = False
        quote = ''
        escaped = False
        for c in line:
            if inString:
                if escaped:
                    escaped = False
                elif c == '\\':
                    escaped = True
                elif c == quote and not escaped:
                        inString = False
                mask.append(True)
            else:
                if c == "'" or c == '"':
                    inString = True
                    quote = c
                    escaped = False
                mask.append(inString)
                
        return mask
    
    def _readFile(self, path):
        f = open(path, 'r')
        lines = f.readlines()
        f.close()
        
        return lines
    
    def _writeFile(self, path, lines):
        f = open(path, 'w')
        f.writelines(lines)
        f.close()
