import os

FCG_DIR = os.path.dirname(os.path.realpath(__file__))

TEST_DIR = FCG_DIR + '/../fortrantestgenerator/tests/testprogram'

ASSEMBLER_DIRS = TEST_DIR + '/src' 
SOURCE_DIRS = TEST_DIR + '/src' 

ABSTRACT_TYPE_IMPLEMENTATIONS = {'abstr': 'concr'}