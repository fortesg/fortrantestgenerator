import random
import string
import re

class CodePostProcessor(object):

    MAX_LINE_LENGTH = 132
    INDENT_LENGTH = 2
    CLEAR_LINE = '! ########## CLEAR LINE ##########'
    MERGE_BEGIN_PREFIX = '! #### MERGE BEGIN'
    MERGE_END_PREFIX   = '! #### MERGE END'
    MERGE_KEY = ''.join(random.choice(string.ascii_lowercase + string.digits) for _ in range(5))
    
    def mergeBeginTag(self, identifier):
        return CodePostProcessor.MERGE_BEGIN_PREFIX + ' ' + CodePostProcessor.MERGE_KEY + ' ' + str(identifier)

    def mergeEndTag(self, identifier):
        return CodePostProcessor.MERGE_END_PREFIX + ' ' + CodePostProcessor.MERGE_KEY + ' ' + str(identifier)
    
    def process(self, text):
                
        rendered = self.__clearAndMerge(text)
#         rendered = self.__unifyIfs(rendered)
#         rendered = self.__merge(rendered)
        rendered = self.__indent(rendered)
        rendered = self.__breakLines(rendered)
        
        return rendered
    
    def __clearAndMerge(self, text):
        if not text:
            return text
        
        lines = []
        for line in text.split("\n"):
            line = line.strip()
            if line == CodePostProcessor.CLEAR_LINE:
                continue
            lines.append(line)
    
    def __merge(self, text):
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
    
    def __unifyIfs(self, text):
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
    
    def __indent(self, text):
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
                indent = max(baseIndent, indent - CodePostProcessor.INDENT_LENGTH)
            if lineUpper.startswith(doubleEndWords):
                indent = max(baseIndent, indent - CodePostProcessor.INDENT_LENGTH)
            if not line.startswith('#'):
                line = (' ' * indent) + line
            lines.append(line)
            if (lineUpper.startswith(beginWords) or lineUpper.endswith(beginWordsBack) or lineUpper.startswith(borderWords)) and not lineUpper.startswith(beginWordExceptions):
                indent = indent + CodePostProcessor.INDENT_LENGTH
                
        return "\n".join(lines)
        
    def __breakLines(self, text):
        lines = []
        for line in text.split("\n"):
            stringMask = self.__stringMask(line)
            while len(line) > CodePostProcessor.MAX_LINE_LENGTH:
                i = CodePostProcessor.MAX_LINE_LENGTH - 2
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
    
    def __stringMask(self, line):
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
    
class CodeBlock():
    def __init__(self, begin = None):
        self.begin = []
        if begin is not None:
            self.begin.append(CodeLine(begin))    
        self.content = []    
        self.end = []
        
class CodeLine():
    def __init__(self, line):
        self.line = line