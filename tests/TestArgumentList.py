#!/usr/bin/python

import unittest
import os
import sys
from source import Variable

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)


class ArgumentListTest(unittest.TestCase):
    def setUp(self):
        self.arg0 = Variable.fromDeclarationStatement('INTEGER :: arg0')
                
        
if __name__ == "__main__":
    unittest.main()