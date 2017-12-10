import os

FTG_DIR = os.path.dirname(os.path.realpath(__file__))

FCG_DIR = FTG_DIR + '/../../fortrancallgraph'
FCG_CONFIG_FILE = 'config_fortrancallgraph.py'

BACKUP_SUFFIX = 'ftg-backup'
FTG_PREFIX = 'ftg_'

TEMPLATE_DIR = FTG_DIR + '/templates/icon_standalone'

ICON_DIR = '/home/christian/workspace/icon'

TEST_SOURCE_DIR = ICON_DIR + '/src/tests'
TEST_DATA_BASE_DIR = ICON_DIR + '/ftg'

SOURCE_DIR = [ICON_DIR + '/build/x86_64-unknown-linux-gnu/src',
              ICON_DIR + '/build/x86_64-unknown-linux-gnu/externals/mtime/src',
              ICON_DIR + '/build/x86_64-unknown-linux-gnu/externals/self/src',
              ICON_DIR + '/build/x86_64-unknown-linux-gnu/externals/tixi/src',
              ICON_DIR + '/build/x86_64-unknown-linux-gnu/externals/yac/src', 
              ICON_DIR + '/build/x86_64-unknown-linux-gnu/support'] 

MODIFY_SOURCE_DIR = [ICON_DIR + '/src',
                     ICON_DIR + '/externals/mtime/src',
                     ICON_DIR + '/externals/self/src',
                     ICON_DIR + '/externals/yac/src',
                     ICON_DIR + '/externals/tixi/src',
                     ICON_DIR + '/externals/jsbach/src',
                     ICON_DIR + '/support'
                    ] 
