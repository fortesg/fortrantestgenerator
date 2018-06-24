#!/usr/bin/python

import unittest
import os
import sys
from Cheetah.Template import Template
from Cheetah import ImportHooks

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)
FCG_DIR = TEST_DIR + '/../../fortrancallgraph'
sys.path.append(FCG_DIR)

class TestTemplate(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        ImportHooks.install()
    
    def testPlain(self):
        template = Template(file=FTG_DIR + '/FTGTemplate.tmpl')
        template.part = 'captureBeforeContains'
        self.assertEqual('', str(template).strip())

    def testSubclass(self):
        template = Template('#extends FTGTemplate')
        template.part = 'captureAfterLastSpecification'
        self.assertEqual('', str(template).strip())

    def testTestTemplate(self):
        template = Template(file=TEST_DIR + '/TestTemplate.tmpl')
        template.part = 'captureBeforeContains'
        self.assertEqual('CAPTURE BEFORE CONTAINS', str(template).strip())
        template.part = 'captureAfterLastSpecification'
        self.assertEqual('CAPTURE AFTER LAST SPECIFICATION', str(template).strip())
        template.part = 'captureBeforeEnd'
        self.assertEqual('CAPTURE BEFORE END', str(template).strip())
        template.part = 'captureAfterSubroutine'
        self.assertEqual('CAPTURE AFTER SUBROUTINE', str(template).strip())
        template.part = 'export'
        self.assertEqual('EXPORT', str(template).strip())
        template.part = 'replay'
        self.assertEqual('REPLAY', str(template).strip())
        
if __name__ == "__main__":
    unittest.main()