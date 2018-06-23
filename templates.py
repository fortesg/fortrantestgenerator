from Cheetah.Template import Template
from enum import Enum
from assertions import assertType
from templatenamespace import TemplatesNameSpace

class FTGTemplate(Template):
    
    class Part(Enum):
        CAPTURE_AFTER_LAST_LINE = '$captureAfterLastLine'
        CAPTURE_BEFORE_CONTAINS = '$captureBeforeContains'
        CAPTURE_AFTER_LAST_SPECIFICATION = '$captureAfterLastSpecification'
        CAPTURE_BEFORE_END = '$captureBeforeEnd'
        EXPORT = '$export'
        REPLAY = '$replay'
        
    def __init__(self, part, namespace):
        assertType(part, 'part', FTGTemplate.Part)
        assertType(namespace, 'namespace', TemplatesNameSpace)
        
        super(FTGTemplate, self).__init__(part.value, searchList=[namespace])
        
    def captureAfterLastLine(self):
        return ''
    
    def captureBeforeContains(self):
        return ''
    
    def captureAfterLastSpecification(self):
        return ''
    
    def captureBeforeEnd(self):
        return ''
    
    def export(self):
        return ''
    
    def replay(self):
        return ''
        
    