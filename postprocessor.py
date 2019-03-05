import random
import string

class CodePostProcessor(object):

    CLEAR_LINE = '! ########## CLEAR LINE ##########'
    MERGE_BEGIN_PREFIX = '! #### MERGE BEGIN'
    MERGE_END_PREFIX   = '! #### MERGE END'
    MERGE_KEY = ''.join(random.choice(string.ascii_lowercase + string.digits) for _ in range(5))
    
    def process(self, text):
        
        
        return text
    
    def mergeBeginTag(self, identifier):
        return CodePostProcessor.MERGE_BEGIN_PREFIX + ' ' + CodePostProcessor.MERGE_KEY + ' ' + str(identifier)

    def mergeEndTag(self, identifier):
        return CodePostProcessor.MERGE_END_PREFIX + ' ' + CodePostProcessor.MERGE_KEY + ' ' + str(identifier)
