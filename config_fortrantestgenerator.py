import os
import sys


### EDIT HERE ###

FTG_DIR = os.path.dirname(os.path.realpath(__file__))
FCG_DIR = FTG_DIR + '/../fortrancallgraph'

TEMPLATE_DIR = FTG_DIR + '/templates/standalone'
TEST_SOURCE_DIR = ''
TEST_DATA_BASE_DIR = FTG_DIR + '/data'

#################

BACKUP_SUFFIX = 'ftg-backup'
FTG_PREFIX = 'ftg_' # Do not edit as long as you not also edit the templates accordingly.

#################

sys.path.append(FCG_DIR)

#################

### Instead of importing config_fortrancallgraph, you can also configure those variables here, 
### just copy the structure from config_fortrancallgraph.py,

from config_fortrancallgraph import SPECIAL_MODULE_FILES, GRAPH_BUILDER, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, EXCLUDE_MODULES, SOURCE_DIR, SOURCE_FILES  # @UnusedImport
