import sys
import os
import shutil
from Cheetah.Template import Template
from utils import assertType, assertTypeAll
from source import SourceFiles, SubroutineFullName
from trackvariable import TrackVariableCallGraphAnalysis
from globals import GlobalVariablesCallGraphAnalysis
from usetraversal import UseTraversal
from supertypes import CallGraphBuilder
import re

class CodeGenerator(object):
    
    MAX_LINE_LENGTH = 132
    INDENT_LENGTH = 2
    DEFAULT_SUFFIX = '.f90'
    
    def __init__(self, sourceFiles, graphBuilder, backupSuffix, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignorePrefix = ''):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(graphBuilder, 'graphBuilder', CallGraphBuilder)
        assertType(backupSuffix, 'backupSuffix', str)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertTypeAll(ignoredModulesForGlobals, 'ignoredModulesForGlobals', str)        
        assertTypeAll(ignoredTypes, 'ignoredTypes', str)        
        
        self._sourceFiles = sourceFiles
        self.__graphBuilder = graphBuilder
        self._backupSuffix = '.' + backupSuffix.lstrip('.')
        self.__excludeModules = excludeModules
        self.__ignoredModulesForGlobals = ignoredModulesForGlobals;
        self.__ignoredTypes = ignoredTypes;
        self.__ignorePrefix = ignorePrefix; 
        
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
        argumentTracker = TrackVariableCallGraphAnalysis(self._sourceFiles, self.__excludeModules, self.__ignoredTypes, interfaces, types)
        argumentTracker.setIgnoreRegex(ignoreRegex)
        typeArgumentReferences = argumentTracker.trackDerivedTypeArguments(callGraph)
        print "    Find References to Global Variables"
        globalsTracker = GlobalVariablesCallGraphAnalysis(self._sourceFiles, self.__excludeModules, self.__ignoredModulesForGlobals, self.__ignoredTypes, interfaces, types)
        globalsTracker.setIgnoreRegex(ignoreRegex)
        globalsReferences = globalsTracker.trackGlobalVariables(callGraph)
        
        sourceFile = self._findSourceFileForSubroutine(subroutineFullName)
        sourceFilePath = sourceFile.getPath()
        if not os.path.isfile(sourceFile.getPath()):
            raise IOError("File not found: " + sourceFilePath);
        
        subroutine = sourceFile.getSubroutine(subroutineFullName)
        
        self._addCode(subroutine, typeArgumentReferences, globalsReferences)
        
    def _addCode(self, subroutine, typeArgumentReferences, globalsReferences):
        raise NotImplementedError()
    
    def _findSourceFileForSubroutine(self, subroutineFullName):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)
        
        return self._findSourceFileForModule(subroutineFullName.getModuleName())
    
    def _findSourceFileForModule(self, moduleName):
        assertType(moduleName, 'moduleName', str)
        
        return self._sourceFiles.findModuleFile(moduleName);

        
    def _extractModulesFromVariableReferences(self, references):
        modules = set()
        for ref in references:
            module = ref.getLevel0Variable().getDeclaredIn()
            if module is not None:
                modules.add(module)
        
        return modules
        
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

    def _processTemplate(self, sourceFilePath, lineNumber, templatePath, templateNameSpace):
        if not os.path.isfile(templatePath):
            print  >> sys.stderr, '*** WARNING [CaptureCodeGenerator]: Template file not found: ' + str(templatePath) + ' ***';
            return
        
        print "      Process Template " + os.path.basename(templatePath) + " on file " + sourceFilePath, 
        source = self._readFile(sourceFilePath)
        codeToAdd = self._breakLines(self._indent((str(Template(file=templatePath, searchList=[templateNameSpace])))))
        
        if codeToAdd:
            source = source[:lineNumber] + ["\n"] + [codeToAdd] + ["\n", "\n"] + source[lineNumber:]
            self._writeFile(sourceFilePath, source)
            print
            return True
        else:
            print " >>> EMPTY"
            return False
    
    def _indent(self, text):
        if not text.strip():
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
            lines.append((' ' * indent) + line)
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
