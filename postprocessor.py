import random
import string

class CodePostProcessor(object):

    MAX_LINE_LENGTH = 132
    INDENT_LENGTH = 2
    CLEAR_LINE = '! ########## CLEAR LINE ##########'
    MERGE_KEY_LENGTH = 5
    
    def __init__(self):
        charSet = string.ascii_lowercase + string.digits
        self.mergeSession = ''.join(random.choice(charSet) for _ in range(CodePostProcessor.MERGE_KEY_LENGTH)) 
    
    def mergeBeginTag(self, key):
        return '! #### MERGE BEGIN ' + self.mergeSession + ' ' + str(key) + ' ###'

    def mergeEndTag(self, key):
        return '! #### MERGE END ' + self.mergeSession + ' ' + str(key) + ' ###'
    
    def process(self, text):
        rendered = str(text)
        rendered = self.__clearAndMerge(rendered)
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