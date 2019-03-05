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
        self.assertEqual(expected, self.post.process(text, 'testClearLine'))
    
    def testNestingNoMerge(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
            b = a
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####
        SWITCH TYPE (t) ! #### MERGE BEGIN 789 ####
          TYPE IS (test) ! #### MERGE BEGIN 789 ####
            t%a = a
        END SWITCH ! #### MERGE END 789 ####
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
    b = a
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 123 ####
SWITCH TYPE (t) ! #### MERGE BEGIN 789 ####
TYPE IS (test) ! #### MERGE BEGIN 789 ####
  t%a = a
END SWITCH ! #### MERGE END 789 ####
'''
        actual = self.post.process(text, 'testNestingNoMerge')
        self.assertEqual(expected, actual, '==== expected ====\n@' + expected + '@\n==== actual ====\n@' + actual + '@')
    
    def testMergeIf(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN 123 #### 
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
            b = a
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####
        IF (a > b) THEN ! #### MERGE BEGIN 123 #### 
          IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
            b = a + 45
          END IF ! #### MERGE END 456 ####
          IF (b == 2) THEN ! #### MERGE BEGIN 456 ####
            a = a * b
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
    b = a
    b = a + 45
  END IF ! #### MERGE END 456 ####
  IF (b == 2) THEN ! #### MERGE BEGIN 456 ####
    a = a * b
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 123 ####
'''
        actual = self.post.process(text, 'testMergeIf')
        self.assertEqual(expected, actual, '==== expected ====\n@' + expected + '@\n==== actual ====\n@' + actual + '@')
    
    def testDontMergeIf(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
            b = a
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####


        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          IF (a == 1) THEN ! #### MERGE BEGIN 789 ####
            b = a + 45
          END IF ! #### MERGE END 789 ####
          IF (b == 2) THEN ! #### MERGE BEGIN 456 ####
            a = a * b
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
    b = a
  END IF ! #### MERGE END 456 ####
  IF (a == 1) THEN ! #### MERGE BEGIN 789 ####
    b = a + 45
  END IF ! #### MERGE END 789 ####
  IF (b == 2) THEN ! #### MERGE BEGIN 456 ####
    a = a * b
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 123 ####
'''
        actual = self.post.process(text, 'testDontMergeIf')
        self.assertEqual(expected, actual, '==== expected ====\n@' + expected + '@\n==== actual ====\n@' + actual + '@')
        
    def testMergeSwitch(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
            b = a
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####
        SWITCH TYPE (t) ! #### MERGE BEGIN 789 ####
          TYPE IS (test) ! #### MERGE BEGIN 789 ####
            t%a = a
        END SWITCH ! #### MERGE END 789 ####
        SWITCH TYPE (t) ! #### MERGE BEGIN 789 ####
          TYPE IS (test) ! #### MERGE BEGIN 789 ####
            t%b = b
        END SWITCH ! #### MERGE END 789 ####
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
    b = a
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 123 ####
SWITCH TYPE (t) ! #### MERGE BEGIN 789 ####
TYPE IS (test) ! #### MERGE BEGIN 789 ####
  t%a = a
  t%b = b
END SWITCH ! #### MERGE END 789 ####
'''
        actual = self.post.process(text, 'testMergeSwitch')
        self.assertEqual(expected, actual, '==== expected ====\n@' + expected + '@\n==== actual ====\n@' + actual + '@')
            
    def testNotMatchingKeys(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
            b = a
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 012 ####
        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          IF (a == 1) THEN ! #### MERGE BEGIN 789 ####
            b = a + 45
          END IF ! #### MERGE END 789 ####
          IF (b == 2) THEN ! #### MERGE BEGIN 456 ####
            a = a * b
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
    b = a
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 012 ####
IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  IF (a == 1) THEN ! #### MERGE BEGIN 789 ####
    b = a + 45
  END IF ! #### MERGE END 789 ####
  IF (b == 2) THEN ! #### MERGE BEGIN 456 ####
    a = a * b
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 123 ####
'''
        actual = self.post.process(text, 'testNotMatchingKeys')
        self.assertEqual(expected, actual, '==== expected ====\n@' + expected + '@\n==== actual ====\n@' + actual + '@')
        
    def testMissingBegin(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
            b = a
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####


        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          IF (a == 1) THEN ! #### MERGE BEGIN 789 ####
            b = a + 45
          END IF ! #### MERGE END 789 ####
          IF (b == 2) THEN
            a = a * b
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
    b = a
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 123 ####


IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  IF (a == 1) THEN ! #### MERGE BEGIN 789 ####
    b = a + 45
  END IF ! #### MERGE END 789 ####
  IF (b == 2) THEN
    a = a * b
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 123 ####
'''
        actual = self.post.process(text, 'testMissingBegin')
        self.assertEqual(expected, actual, '==== expected ====\n@' + expected + '@\n==== actual ====\n@' + actual + '@')
                
    def testMissingEnd(self):
        text = '''
        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          a = a + 1
          IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
            b = a
          END IF ! #### MERGE END 456 ####
        END IF ! #### MERGE END 123 ####


        IF (a > b) THEN ! #### MERGE BEGIN 123 ####
          IF (a == 1) THEN ! #### MERGE BEGIN 789 ####
            b = a + 45
          END IF ! #### MERGE END 789 ####
          IF (b == 2) THEN ! #### MERGE BEGIN 789 ####
            a = a * b
          END IF
        END IF ! #### MERGE END 123 ####
        '''
        expected = '''
IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  a = a + 1
  IF (a == 1) THEN ! #### MERGE BEGIN 456 ####
    b = a
  END IF ! #### MERGE END 456 ####
END IF ! #### MERGE END 123 ####


IF (a > b) THEN ! #### MERGE BEGIN 123 ####
  IF (a == 1) THEN ! #### MERGE BEGIN 789 ####
    b = a + 45
  END IF ! #### MERGE END 789 ####
  IF (b == 2) THEN ! #### MERGE BEGIN 789 ####
    a = a * b
  END IF
END IF ! #### MERGE END 123 ####
'''
        actual = self.post.process(text, 'testMissingEnd')
        self.assertEqual(expected, actual, '==== expected ====\n@' + expected + '@\n==== actual ====\n@' + actual + '@')
                
        
if __name__ == "__main__":
    unittest.main()