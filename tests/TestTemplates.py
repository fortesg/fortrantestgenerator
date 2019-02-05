#!/usr/bin/python

import unittest
import os
import sys
from Cheetah.Template import Template
from Cheetah import ImportHooks

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)
sys.path.append(FTG_DIR + '/templates')
sys.path.append(FTG_DIR + '/templates/Standalone')
FCG_DIR = TEST_DIR + '/../../fortrancallgraph'
sys.path.append(FCG_DIR)

from templatenamespace import ArgumentList, FunctionResult

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
        
    def testSubSubClass(self):
        template = Template('#extends TestTemplate')
        template.part = 'replay'
        self.assertEqual('REPLAY', str(template).strip())
        

    def testIconStandalone(self):
        class DummySubnamespace(object):
            def __init__(self):
                self.exports = 'EXPORTS'
                self.export = 'EXPORT'
                self.name = 'NAME'
                self.joinNames = 'NAMES'
                self.result = 'RESULT'
                self.moduleName = 'MODULE'
        
        class DummyNamespace(object):
            def __init__(self):
                self.subroutine = DummySubnamespace()
                self.module = DummySubnamespace()
                self.globals = DummySubnamespace()
                self.args = ArgumentList([])
                self.result = None
                self.dataDir = 'DATADIR'
             
            def commaList(self, *elements):
                return ', '.join(map(str, elements))
        
        template = Template(file=FTG_DIR + '/templates/IconStandalone/IconStandalone.tmpl', searchList=[DummyNamespace()])
        template.part = 'captureBeforeContains'
        self.assertTrue(str(template).strip())
        template.part = 'captureAfterLastSpecification'
        self.assertTrue(str(template).strip())
        template.part = 'captureBeforeEnd'
        self.assertTrue(str(template).strip())
        template.part = 'export'
        self.assertTrue(str(template).strip())
        
if __name__ == "__main__":
    unittest.main()