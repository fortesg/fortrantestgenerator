#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)

from templates import FTGTemplate
from templatenamespace import TemplatesNameSpace

class TestTemplate(unittest.TestCase):
    def setUp(self):
        
        class DummyTemplatesNameSpace(TemplatesNameSpace):
            def __init__(self):
                pass
        self.namespace = DummyTemplatesNameSpace() 
        
    def testPlain(self):
        self.assertEqual('', str(FTGTemplate(FTGTemplate.Part.CAPTURE_AFTER_LAST_LINE, self.namespace)))

if __name__ == "__main__":
    unittest.main()