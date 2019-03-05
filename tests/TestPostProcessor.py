#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)

from postprocessor import CodePostProcessor

class TestPostProcessor(unittest.TestCase):
    
    def setUp(self):
        self.post = CodePostProcessor()
    
    def testEmpty(self):
        text = ''
        expected = ''
        self.assertEqual(expected, self.post.process(text))
    
    def testClearLine(self):
        text = '''
        a = a + 1
        ! ########## CLEAR LINE ##########
        
        b = a
        '''
        expected = '''
a = a + 1

b = a
'''
        self.assertEqual(expected, self.post.process(text))
    
if __name__ == "__main__":
    unittest.main()