import os

FTG_DIR = os.path.dirname(os.path.realpath(__file__))
FCG_DIR = FTG_DIR + '/../fortrancallgraph'
TEMPLATE = FTG_DIR + '/templates/BaseCompare/BaseCompare.tmpl'

TEST_DIR = FTG_DIR + '/tests/testprogram'

FCG_CONFIG_FILE = TEST_DIR + '/config_fortrancallgraph_test.py'
TEST_SOURCE_DIR = TEST_DIR + '/replay-BaseCompare'
TEST_DATA_BASE_DIR = TEST_DIR + '/data'
