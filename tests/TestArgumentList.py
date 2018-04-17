#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)
FCG_DIR = TEST_DIR + '/../../fortrancallgraph'
sys.path.append(FCG_DIR)

from source import Variable
from templatenamespace import ArgumentList

class TestArgumentList(unittest.TestCase):
    def setUp(self):
        self.arg0, self.arg1 = Variable.fromDeclarationStatement('INTEGER :: arg0, arg1(:)')
        self.argList = ArgumentList([self.arg0, self.arg1], [])
        
    def testAll(self):
        self.assertEqual(['arg0', 'arg1'], map(Variable.getName, self.argList))
        
if __name__ == "__main__":
    unittest.main()