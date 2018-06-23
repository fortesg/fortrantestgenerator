#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)
FCG_DIR = TEST_DIR + '/../../fortrancallgraph'
sys.path.append(FCG_DIR)

from templates import FTGTemplate
from templatenamespace import TemplatesNameSpace
from source import Subroutine, SubroutineFullName, Module, SourceFile

class TestTemplate(unittest.TestCase):
    def setUp(self):
        module = Module('mod', [], SourceFile('', isTestDummy=True), 0)
        subroutine = Subroutine(SubroutineFullName('__mod_MOD_subr'), False, [], module)
        self.namespace = TemplatesNameSpace(subroutine, [], [], '') 
        
    def testPlain(self):
        self.assertEqual('', str(FTGTemplate(FTGTemplate.Part.CAPTURE_AFTER_LAST_LINE, self.namespace)))

if __name__ == "__main__":
    unittest.main()