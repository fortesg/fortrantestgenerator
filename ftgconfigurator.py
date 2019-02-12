import os
import sys
from printout import printError

CFG_FCG_DIR = 'FCG_DIR'
CFG_TEMPLATE = 'TEMPLATE'
CFG_TEST_SOURCE_DIR = 'TEST_SOURCE_DIR'
CFG_TEST_DATA_BASE_DIR = 'TEST_DATA_BASE_DIR'
CFG_MODIFY_SOURCE_DIRS = 'MODIFY_SOURCE_DIRS'
CFG_MODIFY_SOURCE_DIRS_LEGACY = 'MODIFY_SOURCE_DIR'
CFG_BACKUP_SUFFIX = 'BACKUP_SUFFIX'
CFG_FTG_PREFIX = 'FTG_PREFIX'
CFG_FCG_CONFIG_FILE = 'FCG_CONFIG_FILE'
CFG_FTG_CONFIG_FILE = 'FTG_CONFIG_FILE'
        
def loadFortranTestGeneratorConfiguration(configFile):
    if configFile:
        configFile = configFile.strip('"\'')
    else:
        configFile = 'config_fortrantestgenerator.py'
    originalConfigFile = configFile
    if not os.path.isfile(configFile):
        configFile = os.path.join(os.path.dirname(os.path.realpath(__file__)), originalConfigFile)
    if not os.path.isfile(configFile):
        configFile = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'config', originalConfigFile)
    if not os.path.isfile(configFile):
        printError('Config file not found: ' + originalConfigFile, location='FortranTestGenerator')
        return None
        
    config = {}
    with open(configFile) as f:
        code = compile(f.read(), "somefile.py", 'exec')
        globalNamespace = globals()
        exec(code, globalNamespace, config)
    
    configError = False
    if CFG_FCG_DIR not in config or not config[CFG_FCG_DIR]:
        printError('Missing config variable: ' + CFG_FCG_DIR, location='FortranTestGenerator')
        configError = True
        configError = True
    else:
        fcgDir = config[CFG_FCG_DIR]
        if not os.path.isdir(fcgDir):
            printError('FortranCallGraph directory not found (' + CFG_FCG_DIR + '): ' + fcgDir, location='FortranTestGenerator')
            configError = True
        else:
            fcgBin = os.path.join(fcgDir, 'FortranCallGraph.py')
            if not os.path.isfile(fcgBin):
                printError('FortranCallGraph not found in directory (' + CFG_FCG_DIR + '): ' + fcgDir, location='FortranTestGenerator')
                configError = True

    if CFG_TEMPLATE not in config or not config[CFG_TEMPLATE]:
        printError('Missing config variable: ' + CFG_TEMPLATE, location='FortranTestGenerator')
        configError = True

    if CFG_TEST_SOURCE_DIR not in config or not config[CFG_TEST_SOURCE_DIR]:
        printError(sys.stderr, 'Missing config variable: ' + CFG_TEST_SOURCE_DIR, location='FortranTestGenerator')
        configError = True

    if CFG_MODIFY_SOURCE_DIRS not in config and CFG_MODIFY_SOURCE_DIRS_LEGACY in config:
        config[CFG_MODIFY_SOURCE_DIRS] = config[CFG_MODIFY_SOURCE_DIRS_LEGACY]
    if CFG_MODIFY_SOURCE_DIRS not in config or not config[CFG_MODIFY_SOURCE_DIRS]:
        config[CFG_MODIFY_SOURCE_DIRS] = None
    elif isinstance(config[CFG_MODIFY_SOURCE_DIRS], str):
        config[CFG_MODIFY_SOURCE_DIRS] = [config[CFG_MODIFY_SOURCE_DIRS]]

    if CFG_TEST_DATA_BASE_DIR not in config or not config[CFG_TEST_DATA_BASE_DIR]:
        config[CFG_TEST_DATA_BASE_DIR] = '.'
        
    if CFG_BACKUP_SUFFIX not in config or not config[CFG_BACKUP_SUFFIX]:
        config[CFG_BACKUP_SUFFIX] = 'ftg-backup'

    if CFG_FTG_PREFIX not in config or not config[CFG_FTG_PREFIX]:
        config[CFG_FTG_PREFIX] = 'ftg_'

    if CFG_FCG_CONFIG_FILE not in config:
        config[CFG_FCG_CONFIG_FILE] = None
        
    config[CFG_FTG_CONFIG_FILE] = os.path.abspath(configFile)
    
    if configError:
        return None
    
    return config