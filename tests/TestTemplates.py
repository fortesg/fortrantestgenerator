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
    
#     def setUp(self):
#         class DummyTemplatesNameSpace(TemplatesNameSpace):
#             def __init__(self):
#                 pass
#         self.namespace = DummyTemplatesNameSpace() 
        
    def testPlain(self):
        self.assertEqual('', str(Template(file=FTG_DIR + '/FTGTemplate.tmpl')).strip())

    def testSubclass(self):
        self.assertEqual('', str(Template('#extends FTGTemplate')).strip())

    def testTestTemplate(self):
        self.assertEqual('', str(Template(file=TEST_DIR + '/TestTemplate.tmpl')).strip())
        
        
if __name__ == "__main__":
    unittest.main()