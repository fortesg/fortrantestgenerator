import os
import sys
import re
from Cheetah.Template import Template
from Cheetah import ImportHooks
from assertions import assertType, assertTypeAll
from source import SourceFiles, SubroutineFullName, Subroutine
from trackvariable import VariableTracker
from globals import GlobalVariableTracker
from usetraversal import UseTraversal
from supertypes import CallGraphBuilder
from printout import printLine, printInline
from templatenamespace import TemplatesNameSpace
from typefinder import TypeCollection
from postprocessor import CodePostProcessor

class CodeGenerator(object):
    
    MAX_LINE_LENGTH = 132
    INDENT_LENGTH = 2
    DEFAULT_SUFFIX = '.f90'
    
    def __init__(self, sourceFiles, templatePath, graphBuilder, postProcessor, excludeModules = [], ignoredModulesForGlobals = [], ignoredTypes = [], ignorePrefix = '', abstractTypes = {}):
        assertType(sourceFiles, 'sourceFiles', SourceFiles)
        assertType(templatePath, 'templatePath', str)
        if not os.path.isfile(templatePath):
            raise IOError("Template file not found: " + templatePath)
        assertType(graphBuilder, 'graphBuilder', CallGraphBuilder)
        assertType(postProcessor, 'postProcessor', CodePostProcessor)
        assertTypeAll(excludeModules, 'excludeModules', str)
        assertTypeAll(ignoredModulesForGlobals, 'ignoredModulesForGlobals', str)        
        assertTypeAll(ignoredTypes, 'ignoredTypes', str)        
        
        self._sourceFiles = sourceFiles
        self.__templatePath = templatePath
        self.__graphBuilder = graphBuilder
        self._postProcessor = postProcessor
        self.__excludeModules = excludeModules
        self.__ignoredModulesForGlobals = ignoredModulesForGlobals
        self.__ignoredTypes = ignoredTypes
        self.__ignorePrefix = ignorePrefix
        self.__abstractTypes = abstractTypes
        
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

        printLine('Build Call Graph', indent = 1)
            
        subroutine = self._findSubroutine(subroutineFullName)
        if subroutine is None:
            raise LookupError("Subroutine not found: " + str(subroutineFullName))

        callgraph = self.__graphBuilder.buildCallGraph(subroutineFullName)
        
        printLine('Find Interfaces and Types', indent = 1)
        useTraversal = UseTraversal(self._sourceFiles, self.__excludeModules, self.__abstractTypes)
        useTraversal.parseModules(callgraph.getRoot())
        interfaces = useTraversal.getInterfaces()
        types = useTraversal.getTypes()

        printLine('Analyse Source Code', indent = 1)
        argumentTracker = VariableTracker(self._sourceFiles, self.__excludeModules, self.__ignoredTypes, interfaces, types, callGraphBuilder = self.__graphBuilder)
        argumentTracker.setIgnoreRegex(ignoreRegex)
        
        printLine('Find References to Type Argument Members', indent = 2)
        typeArgumentReferences = argumentTracker.trackDerivedTypeArguments(callgraph)
        
        subroutine = self._findSubroutine(subroutineFullName)
        if subroutine.isFunction() and subroutine.getResultVariable().hasDerivedType():
            printLine('Find References to Type Result', indent = 2)
            typeResultReferences = argumentTracker.trackDerivedTypeResult(callgraph)
        else:
            typeResultReferences = []
        
        printLine('Find References to Global Variables', indent = 2)
        globalsTracker = GlobalVariableTracker(self._sourceFiles, self.__excludeModules, self.__ignoredModulesForGlobals, self.__ignoredTypes, interfaces, types, callGraphBuilder = self.__graphBuilder)
        globalsTracker.setIgnoreRegex(ignoreRegex)
        globalsReferences = globalsTracker.trackGlobalVariables(callgraph)
        
        sourceFile = subroutine.getSourceFile()
        sourceFilePath = sourceFile.getPath()
        if not os.path.isfile(sourceFile.getPath()):
            raise IOError("File not found: " + sourceFilePath);
        
        self.addCode(subroutineFullName, typeArgumentReferences, typeResultReferences, globalsReferences, callgraph, types)
        
    def addCode(self, subroutineFullName, typeArgumentReferences, typeResultReferences, globalsReferences, callgraph, types):
        raise NotImplementedError()
    
    def _findSubroutine(self, subroutineFullName):
        assertType(subroutineFullName, 'subroutineFullName', SubroutineFullName)
        
        return self._sourceFiles.findSubroutine(subroutineFullName)
    
    def _setTypesToSubroutineVariables(self, subroutine, types):
        assertType(subroutine, 'subroutine', Subroutine)
        assertType(types, 'types', TypeCollection)
        
        for variable in subroutine.getVariables():
            if variable.hasDerivedType() and not variable.isTypeAvailable() and variable.getDerivedTypeName() in types:
                variable.setType(types[variable.getDerivedTypeName()])
    
    def _findModule(self, moduleName):
        assertType(moduleName, 'moduleName', str)
        
        return self._sourceFiles.findModule(moduleName);

    def _processTemplate(self, sourceFilePath, lineNumber, part, templateNameSpace):
        printInline('Process Template ' + os.path.basename(self.__templatePath) + ' [' + part + ']' + ' on file ' + sourceFilePath, indent = 2)
        source = self._readFile(sourceFilePath)
        codeToAdd = self.__loadTemplate(part, templateNameSpace)
        
        if codeToAdd:
            source = source[:lineNumber] + ["\n"] + [codeToAdd] + ["\n", "\n"] + source[lineNumber:]
            self._writeFile(sourceFilePath, source)
            printLine()
            return True
        else:
            printLine(' >>> EMPTY')
            return False
        
    def __loadTemplate(self, part, templateNameSpace):
        template = Template(file=self.__templatePath, searchList=[templateNameSpace])
        template.part = part
        
        rendered = str(template).strip()
        rendered = self._clearLines(rendered)
#         rendered = self._unifyIfs(rendered)
#         rendered = self._merge(rendered)
        rendered = self._indent(rendered)
        rendered = self._breakLines(rendered)
        return rendered
    
    def _clearLines(self, text):
        if not text:
            return text
        
        lines = []
        for line in text.split("\n"):
            line = line.strip()
            if not line == CodePostProcessor.CLEAR_LINE:
                lines.append(line)
                 
        return "\n".join(lines)
    
    def _merge(self, text):
        if not text:
            return text
        
        lines = []
        begins = []
        ends = []
        for i, line in text.split("\n"):
            beginMatch = CodePostProcessor.MERGE_BEGIN_REGEX.match(line)
            if beginMatch is not None:
                identifier = beginMatch.group('identifier')
                if begins[-1] and begins[-1][0] == identifier:
                    begins[-1][0] += "\n" + line
                if not begins[-1] or begins[-1][0] != identifier:
                    begins.append((identifier, i, line))
            else:
                endMatch = CodePostProcessor.MERGE_END_REGEX.match(line)
                if endMatch is not None:
                    identifier = endMatch.group('identifier')
        
        return "\n".join(lines)
    
    def _unifyIfs(self, text):
        if not text:
            return text
        
        ifRegex = re.compile(r'^IF\s+\(.*\)\s+THEN$', re.IGNORECASE)
        endifRegex = re.compile(r'^END\s+IF$', re.IGNORECASE)
        
        lines = []
        ifStack = []
        endIfBuffer = []
        for line in text.split("\n"):
            if ifRegex.match(line) is not None:
                if endIfBuffer:
                    if ifStack[-1] == line:
                        endIfBuffer = []
                    else:
                        lines += endIfBuffer
                        endIfBuffer = []
                        ifStack.pop()
                        ifStack.append(line)
                        lines.append(line)
                else:
                    ifStack.append(line)
                    lines.append(line)
            elif line == 'ELSE':
                ifStack[-1] = line
                lines.append(line)
            elif endifRegex.match(line) is not None:
                if endIfBuffer:
                    lines += endIfBuffer
                    endIfBuffer = []
                    ifStack.pop()
                endIfBuffer.append(line)
            elif not line:
                if endIfBuffer:
                    endIfBuffer.append(line)
                else:
                    lines.append(line)
            else:
                if endIfBuffer:
                    lines += endIfBuffer
                    endIfBuffer = []
                    ifStack.pop()
                lines.append(line)
        
        return "\n".join(lines) 
    
    def _indent(self, text):
        if not text:
            return text
        
        beginWords = ('PROGRAM ', 'MODULE ', 'SUBROUTINE ', 'FUNCTION ', 'INTERFACE ', 'TYPE ', 'DO ', 'SELECT ', 'INTERFACE ')
        beginWordExceptions = ('MODULE PROCEDURE')
        beginWordsBack = (' THEN')
        endWords = ('END ', 'ENDIF', 'ENDDO', 'ENDFUNCTION', 'ENDSELECT')
        doubleEndWords = ('END SELECT', 'ENDSELECT')
        borderWords = ('CONTAINS', 'ELSE', 'ELSEIF')
        
        originalLines = text.split("\n")
        firstLine = originalLines[0]
        baseIndent = len(firstLine) - len(firstLine.lstrip())
        
        lines = []
        indent = baseIndent
        for line in text.split("\n"):
            lineUpper = line.upper()
            if lineUpper.startswith(endWords) or lineUpper.startswith(borderWords):
                indent = max(baseIndent, indent - CodeGenerator.INDENT_LENGTH)
            if lineUpper.startswith(doubleEndWords):
                indent = max(baseIndent, indent - CodeGenerator.INDENT_LENGTH)
            if not line.startswith('#'):
                line = (' ' * indent) + line
            lines.append(line)
            if (lineUpper.startswith(beginWords) or lineUpper.endswith(beginWordsBack) or lineUpper.startswith(borderWords)) and not lineUpper.startswith(beginWordExceptions):
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
