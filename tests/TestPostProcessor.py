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
    
    def testNestingNoMerge(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN abcde 123 ### 
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN abcde 123 ###
            b = a
          END IF ! #### MERGE BEGIN abcde 123 ###
        END IF ! #### MERGE BEGIN abcde 123 ###
        SWITCH TYPE (t) ! #### MERGE BEGIN abcde 123 ###
          TYPE IS (test)
            t%a = a
        END SWITCH ! #### MERGE END abcde 123 ###
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN abcde 123 ###
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN abcde 123 ###
    b = a
  END IF ! #### MERGE BEGIN abcde 123 ###
END IF ! #### MERGE BEGIN abcde 123 ###
SWITCH TYPE (t) ! #### MERGE BEGIN abcde 123 ###
TYPE IS (test)
  t%a = a
END SWITCH ! #### MERGE END abcde 123 ###
'''

        self.assertEqual(expected, self.post.process(text))
    
    def testMergeIf(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN abcde 123 ### 
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN abcde 123 ###
            b = a
          END IF ! #### MERGE BEGIN abcde 123 ###
        END IF ! #### MERGE BEGIN abcde 123 ###
        IF (a > b) THEN ! #### MERGE BEGIN abcde 123 ### 
          IF (a == 1) THEN ! #### MERGE BEGIN abcde 123 ###
            b = a + 45
          END IF ! #### MERGE BEGIN abcde 123 ###
          IF (b == 2) THEN ! #### MERGE BEGIN abcde 123 ###
            a = a * b
          END IF ! #### MERGE BEGIN abcde 123 ###
        END IF ! #### MERGE BEGIN abcde 123 ###
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN abcde 123 ### 
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN abcde 123 ###
    b = a
    b = a + 45
  END IF ! #### MERGE BEGIN abcde 123 ###
  IF (b == 2) THEN ! #### MERGE BEGIN abcde 123 ###
    a = a * b
  END IF ! #### MERGE BEGIN abcde 123 ###
END IF ! #### MERGE BEGIN abcde 123 ###
'''

        self.assertEqual(expected, self.post.process(text))
    
if __name__ == "__main__":
    unittest.main()