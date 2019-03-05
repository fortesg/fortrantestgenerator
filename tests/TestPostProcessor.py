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
        self.assertEqual('', self.post.process(text))
    
if __name__ == "__main__":
    unittest.main()