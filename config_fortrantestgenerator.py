import os
import sys


### EDIT HERE ###

FTG_FOLDER = os.path.dirname(os.path.realpath(__file__))
FCG_FOLDER = FTG_FOLDER + '/../fortrancallgraph'

TEMPLATE_FOLDER = FTG_FOLDER + '/templates/standalone'
TEST_SOURCE_FOLDER = ''
TEST_DATA_BASE_DIR = FTG_FOLDER + '/data'

#################

BACKUP_SUFFIX = 'ftg-backup'
FTG_PREFIX = 'ftg_' # Do not edit as long as you not also edit the templates accordingly.

#################

sys.path.append(FCG_FOLDER)

#################

### Instead of importing config_fortrancallgraph, you can also configure those variables here, 
### just copy the structure from config_fortrancallgraph.py,

from config_fortrancallgraph import SPECIAL_MODULE_FILES, GRAPH_BUILDER, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, EXCLUDE_MODULES, SOURCE_DIR, SOURCE_FILES  # @UnusedImport
