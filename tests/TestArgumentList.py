#!/usr/bin/python

import unittest
import os
import sys

TEST_DIR = os.path.dirname(os.path.realpath(__file__))
FTG_DIR = TEST_DIR + '/..'
sys.path.append(FTG_DIR)
FCG_DIR = TEST_DIR + '/../../fortrancallgraph'
sys.path.append(FCG_DIR)

from source import Variable, Subroutine, SubroutineFullName, Module, SourceFile, VariableReference
from templatenamespace import Argument, ArgumentList

class TestArgumentList(unittest.TestCase):
    def setUp(self):
        self.arg0, self.arg1 = Variable.fromDeclarationStatement('INTEGER, INTENT(inout), ALLOCATABLE :: arg0, arg1(:)')
        self.arg2, = Variable.fromDeclarationStatement('INTEGER, INTENT(in), POINTER :: arg2')
        self.arg3, = Variable.fromDeclarationStatement('TYPE(test), INTENT(inout), OPTIONAL :: arg3')
        self.arg4, = Variable.fromDeclarationStatement('INTEGER, INTENT(out), OPTIONAL :: arg4')
        args = [self.arg0, self.arg1, self.arg2, self.arg3, self.arg4]
        
        sourceFile = SourceFile('', False, True)
        module = Module('testmod', [], sourceFile, 0)
        subroutine = Subroutine(SubroutineFullName.fromParts('testmod', 'testsubr'), False, [], module)
        for arg in args:
            arg.setDeclaredIn(subroutine)
            
        self.ref3a = VariableReference('arg3%member1', subroutine.getName(), 42, self.arg3)
            
        self.argList = ArgumentList(args, [self.ref3a])
        
    def testAll(self):
        self.assertEqual('arg0, arg1, arg2, arg3, arg4', self.argList.joinNames())
        
    def testIntentIn(self):
        self.assertEqual('arg2', self.argList.intentIn().joinNames())
        
    def testIntentOut(self):
        self.assertEqual('arg4', self.argList.intentOut().joinNames())
        
    def testIntentInout(self):
        self.assertEqual('arg0, arg1, arg3', self.argList.intentInout().joinNames())
        
    def testAllIn(self):
        self.assertEqual('arg0, arg1, arg2, arg3', self.argList.allIn().joinNames())
        
    def testAllOut(self):
        self.assertEqual('arg0, arg1, arg3, arg4', self.argList.allOut().joinNames())
        
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
        self.assertEqual('arg3', self.argList.allIn().optionals().joinNames())
        self.assertEqual('arg3', self.argList.optionals().allIn().joinNames())
        self.assertEqual('arg0, arg1', self.argList.allOut().allocatablesOrPointers().joinNames())
        self.assertEqual('arg0, arg1', self.argList.allocatablesOrPointers().allOut().joinNames())
        self.assertEqual('', self.argList.allocatablesOrPointers().allOut().optionals().joinNames())
        
    def testMagicMethods(self):
        #__nonzero__
        self.assertTrue(self.argList)
        self.assertTrue(self.argList.allIn().optionals())
        self.assertFalse(self.argList.allocatablesOrPointers().allOut().optionals())
        #__len__
        self.assertEquals(5, len(self.argList))
        #__getitem__
        self.assertEquals('arg2', self.argList[2].name())
        #__iter__
        self.assertEquals(['arg0', 'arg1', 'arg2', 'arg3', 'arg4'], [arg.name() for arg in self.argList])
        #__reversed__
        self.assertEquals(['arg4', 'arg3', 'arg2', 'arg1', 'arg0'], [arg.name() for arg in reversed(self.argList)])
        #__contains__
        arg2 = Argument(self.arg2, [])
        self.assertIn(arg2, self.argList)
        
    def testUsedVariables(self):
        self.assertEqual(['arg2'], [ref.expression() for ref in self.argList.intentIn().usedVariables()])
        self.assertEqual(['arg0', 'arg1', 'arg2', 'arg3%member1'], [ref.expression() for ref in self.argList.allIn().usedVariables()])
        
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
INTEGER, INTENT(in) :: arg2
TYPE(test), INTENT(inout), OPTIONAL :: arg3
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
        
        expSpec = '''
INTEGER, POINTER, INTENT(inout) :: arg0
INTEGER, DIMENSION(:), POINTER, INTENT(inout) :: arg1
INTEGER, POINTER, INTENT(in) :: arg2
TYPE(test), POINTER, INTENT(inout), OPTIONAL :: arg3
INTEGER, POINTER, INTENT(out), OPTIONAL :: arg4
'''.strip()
        self.assertEqual(expSpec, self.argList.specs(pointer=True))
        
        expSpec = '''
INTEGER, ALLOCATABLE, INTENT(inout) :: arg0
INTEGER, DIMENSION(:), ALLOCATABLE, INTENT(inout) :: arg1
INTEGER, INTENT(in) :: arg2
TYPE(test), INTENT(inout), OPTIONAL :: arg3
INTEGER, INTENT(out), OPTIONAL :: arg4
'''.strip()
        self.assertEqual(expSpec, self.argList.specs(pointer=False))

if __name__ == "__main__":
    unittest.main()