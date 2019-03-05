import random
import string
import re
from printout import printDebug

class CodePostProcessor(object):

    MAX_LINE_LENGTH = 132
    INDENT_LENGTH = 2
    CLEAR_LINE = '! ########## CLEAR LINE ##########'
    MERGE_SESSION_ID_LENGTH = 5
    MERGE_REGEX = re.compile(r'! #### MERGE (P?<part>(BEGIN|END)) [a-z0-9]{5} (P?<key>.*) ###')
    
    def __init__(self):
        charSet = string.ascii_lowercase + string.digits
        self.mergeSession = ''.join(random.choice(charSet) for _ in range(CodePostProcessor.MERGE_SESSION_ID_LENGTH)) 
    
    def mergeBeginTag(self, key):
        return '! #### MERGE BEGIN ' + self.mergeSession + ' ' + str(key) + ' ###'

    def mergeEndTag(self, key):
        return '! #### MERGE END ' + self.mergeSession + ' ' + str(key) + ' ###'
    
    def process(self, text):
        if not text:
            return text
        
        lines = str(text).split("\n")
        lines = self.__clearAndMerge(lines)
        lines = self.__indent(lines)
        lines = self.__breakLines(lines)
        return "\n".join(lines)
    
    def __clearAndMerge(self, lines):
        rootBlock = CodeBlock('', None)
        cBlock = rootBlock
        for line in lines:
            line = line.strip()
            if line == CodePostProcessor.CLEAR_LINE:
                continue
            cLine = CodeLine(line)
            match = CodePostProcessor.MERGE_REGEX.match(line)
            if match is not None:
                key = match.group('key')
                if match.group('part') == 'BEGIN':
                    if cBlock.new() and cBlock.key == key:
                        cBlock.begin(cLine)
                    else:
                        if cBlock.closed():
                            cBlock = cBlock.parent
                        newBlock = CodeBlock(key, cBlock)
                        newBlock.begin(cLine)
                        cBlock.content(newBlock)
                        cBlock = newBlock
                else: # END
                    if cBlock.closed() and cBlock.key != key:
                        cBlock = cBlock.parent
                    cBlock.end(cLine)
                    # TODO: What if cBLock.key != key?
            else:
                if cBlock.closed():
                    cBlock = cBlock.parent
                cBlock.content(cLine)
            
        return rootBlock.render()
    
    def __indent(self, lines):
        beginWords = ('PROGRAM ', 'MODULE ', 'SUBROUTINE ', 'FUNCTION ', 'INTERFACE ', 'TYPE ', 'DO ', 'SELECT ', 'INTERFACE ')
        beginWordExceptions = ('MODULE PROCEDURE')
        beginWordsBack = (' THEN')
        endWords = ('END ', 'ENDIF', 'ENDDO', 'ENDFUNCTION', 'ENDSELECT')
        doubleEndWords = ('END SELECT', 'ENDSELECT')
        borderWords = ('CONTAINS', 'ELSE', 'ELSEIF')
        
        firstLine = lines[0]
        baseIndent = len(firstLine) - len(firstLine.lstrip())
        
        rendered = []
        indent = baseIndent
        for line in lines:
            lineUpper = line.upper()
            stringMask = self.__stringMask(line)
            exclPos = line.find('!')
            while exclPos >= 0 and stringMask[exclPos]:
                exclPos = line.find('!', exclPos + 1)
            if exclPos >= 0:
                lineUpper = lineUpper[:exclPos].strip()
                
            if lineUpper.startswith(endWords) or lineUpper.startswith(borderWords):
                indent = max(baseIndent, indent - CodePostProcessor.INDENT_LENGTH)
            if lineUpper.startswith(doubleEndWords):
                indent = max(baseIndent, indent - CodePostProcessor.INDENT_LENGTH)
            if not line.startswith('#'):
                line = (' ' * indent) + line
            rendered.append(line)
            if (lineUpper.startswith(beginWords) or lineUpper.endswith(beginWordsBack) or lineUpper.startswith(borderWords)) and not lineUpper.startswith(beginWordExceptions):
                indent = indent + CodePostProcessor.INDENT_LENGTH
                
        return rendered
        
    def __breakLines(self, lines):
        rendered = []
        for line in lines:
            stringMask = self.__stringMask(line)
            while len(line) > CodePostProcessor.MAX_LINE_LENGTH:
                i = CodePostProcessor.MAX_LINE_LENGTH - 2
                while stringMask[i]:
                    i -= 1
                if line[i] != ' ':
                    i -= 1
                    while "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_=>".find(line[i]) >= 0:
                        i -= 1
                rendered.append(line[:i + 1].rstrip() + " &")
                indent =  ' ' * (len(line) - len(line.lstrip())) + "&  "
                line = indent + line[i + 1:]
            else:        
                rendered.append(line)
        return rendered
    
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
    def __init__(self, key, parent):
        self.key = key
        self.parent = parent
        self.__begin = []
        self.__content = []    
        self.__end = []
    
    def begin(self, element):
        self.__begin.append(element)
    
    def content(self, element):
        self.__content.append(element)
    
    def end(self, element):
        self.__end.append(element)
    
    def root(self):
        return self.parent is None
    
    def new(self):
        return not self.root() and not self.__content and not self.__end
    
    def closed(self):
        return not self.root() and self.__end
        
    def render(self):
        return [code.render() for code in self.__begin + self.__content + self.__end]
        
class CodeLine():
    def __init__(self, line):
        self.__line = line
    
    def empty(self):
        return not self.__line
        
    def render(self):
        return self.__line