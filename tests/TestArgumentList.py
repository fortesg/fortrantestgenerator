#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)
FCG_DIR = TEST_DIR + '/../../fortrancallgraph'
sys.path.append(FCG_DIR)

from source import Variable, Subroutine, SubroutineFullName, Module, SourceFile
from templatenamespace import ArgumentList

class TestArgumentList(unittest.TestCase):
    def setUp(self):
        self.arg0, self.arg1 = Variable.fromDeclarationStatement('INTEGER, INTENT(inout), ALLOCATABLE :: arg0, arg1(:)')
        self.arg2, = Variable.fromDeclarationStatement('INTEGER, INTENT(in), POINTER :: arg2')
        self.arg3, = Variable.fromDeclarationStatement('TYPE(test), INTENT(inout), OPTIONAL :: arg3')
        self.arg4, = Variable.fromDeclarationStatement('INTEGER, INTENT(out), OPTIONAL :: arg4')
        self.argList = ArgumentList([self.arg0, self.arg1, self.arg2, self.arg3, self.arg4], [])
        
        sourceFile = SourceFile('', False, True)
        module = Module('testmod', [], sourceFile, 0)
        subroutine = Subroutine(SubroutineFullName.fromParts('testmod', 'testsubr'), False, [], module)
        for arg in self.argList:
            arg.setDeclaredIn(subroutine)
        
    def testAll(self):
        self.assertEqual('arg0, arg1, arg2, arg3, arg4', self.argList.joinNames())
        
    def testInput(self):
        self.assertEqual('arg0, arg1, arg2, arg3', self.argList.input().joinNames())
        
    def testOutput(self):
        self.assertEqual('arg0, arg1, arg3, arg4', self.argList.output().joinNames())
        
    def testOptionals(self):
        self.assertEqual('arg3, arg4', self.argList.optionals().joinNames())
        
    def testRequired(self):
        self.assertEqual('arg0, arg1, arg2', self.argList.requireds().joinNames())
        
    def testBuiltInTypes(self):
        self.assertEqual('arg0, arg1, arg2, arg4', self.argList.builtInTypes().joinNames())
        
    def testDerivedTypes(self):
        self.assertEqual('arg3', self.argList.derivedTypes().joinNames())
        
    def testPointers(self):
        self.assertEqual('arg2', self.argList.pointers().joinNames())
        
    def testAllocatables(self):
        self.assertEqual('arg0, arg1', self.argList.allocatables().joinNames())
        
    def testAllocatablesOrPointers(self):
        self.assertEqual('arg0, arg1, arg2', self.argList.allocatablesOrPointers().joinNames())
        
    def testCombinations(self):
        self.assertEqual('arg3', self.argList.input().optionals().joinNames())
        self.assertEqual('arg3', self.argList.optionals().input().joinNames())
        self.assertEqual('arg0, arg1', self.argList.output().allocatablesOrPointers().joinNames())
        self.assertEqual('arg0, arg1', self.argList.allocatablesOrPointers().output().joinNames())
        self.assertEqual('', self.argList.allocatablesOrPointers().output().optionals().joinNames())
        
    def testSpecs(self):
        expSpec = '''
INTEGER, ALLOCATABLE, INTENT(inout) :: arg0
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(inout) :: arg1
INTEGER, POINTER, INTENT(in) :: arg2
TYPE(test), INTENT(inout), OPTIONAL :: arg3
INTEGER, INTENT(out), OPTIONAL :: arg4
'''.strip()
        self.assertEqual(expSpec, self.argList.specs())
        
        expSpec = '''
INTEGER, ALLOCATABLE, INTENT(in) :: arg0
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(in) :: arg1
INTEGER, POINTER, INTENT(in) :: arg2
TYPE(test), INTENT(in), OPTIONAL :: arg3
INTEGER, INTENT(in), OPTIONAL :: arg4
'''.strip()
        self.assertEqual(expSpec, self.argList.specs(intent='in'))
        
        expSpec = '''
INTEGER, ALLOCATABLE :: arg0
INTEGER, DIMENSION(:), ALLOCATABLE :: arg1
INTEGER, POINTER :: arg2
TYPE(test) :: arg3
INTEGER :: arg4
'''.strip()
        self.assertEqual(expSpec, self.argList.specs(intent=''))
        
        expSpec = '''
INTEGER, ALLOCATABLE, INTENT(inout) :: arg0
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(inout) :: arg1
INTEGER, POINTER, INTENT(in) :: arg2
TYPE(test), ALLOCATABLE, INTENT(inout), OPTIONAL :: arg3
INTEGER, INTENT(out), OPTIONAL :: arg4
'''.strip()
        self.assertEqual(expSpec, self.argList.specs(allocatable=True))
        
        expSpec = '''
INTEGER, INTENT(inout) :: arg0
INTEGER, DIMENSION(:), INTENT(inout) :: arg1
INTEGER, POINTER, INTENT(in) :: arg2
TYPE(test), INTENT(inout), OPTIONAL :: arg3
INTEGER, INTENT(out), OPTIONAL :: arg4
'''.strip()
        self.assertEqual(expSpec, self.argList.specs(allocatable=False))

if __name__ == "__main__":
    unittest.main()