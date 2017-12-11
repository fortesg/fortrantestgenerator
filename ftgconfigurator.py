import os
import sys
        
def loadFortranTestGeneratorConfiguration(configFile):
    if configFile:
        configFile = configFile.strip('"\'')
    else:
        configFile = 'config_fortrantestgenerator.py'
    originalConfigFile = configFile
    if not os.path.isfile(configFile):
        configFile = os.path.dirname(os.path.realpath(__file__)) + '/' + originalConfigFile
    if not os.path.isfile(configFile):
        configFile = os.path.dirname(os.path.realpath(__file__)) + '/config/' + originalConfigFile
    if not os.path.isfile(configFile):
        print >> sys.stderr, 'Config file not found: ' + originalConfigFile
        return None
        
    config = {}
    execfile(configFile, globals(), config)
    
    #TODO Constants for keys
    
    configError = False
    if 'FCG_DIR' not in config or not config['FCG_DIR']:
        print >> sys.stderr, 'Missing config variable: FCG_DIR'
        configError = True
        configError = True
    else:
        fcgDir = config['FCG_DIR']
        if not os.path.isdir(fcgDir):
            print >> sys.stderr, 'FortranCallGraph directory not found (FCG_DIR): ' + fcgDir
            configError = True
        else:
            fcgBin = os.path.join(fcgDir, 'FortranCallGraph.py')
            if not os.path.isfile(fcgBin):
                print >> sys.stderr, 'FortranCallGraph not found in directory (FCG_DIR): ' + fcgDir
                configError = True
            else:
                sys.path.append(config['FCG_DIR'])

    if 'TEMPLATE_DIR' not in config or not config['TEMPLATE_DIR']:
        print >> sys.stderr, 'Missing config variable: TEMPLATE_DIR'
        configError = True

    if 'TEST_SOURCE_DIR' not in config or not config['TEST_SOURCE_DIR']:
        print >> sys.stderr, 'Missing config variable: TEST_SOURCE_DIR'
        configError = True

    if 'TEST_DATA_BASE_DIR' not in config or not config['TEST_DATA_BASE_DIR']:
        print >> sys.stderr, 'Missing config variable: TEST_DATA_BASE_DIR'
        configError = True

    if 'MODIFY_SOURCE_DIR' not in config or not config['MODIFY_SOURCE_DIR']:
        config['MODIFY_SOURCE_DIR'] = None
        
    if 'BACKUP_SUFFIX' not in config or not config['BACKUP_SUFFIX']:
        config['BACKUP_SUFFIX'] = 'ftg-backup'

    if 'FTG_PREFIX' not in config or not config['FTG_PREFIX']:
        config['FTG_PREFIX'] = 'ftg_'

    if 'FCG_CONFIG_FILE' not in config or not config['FCG_CONFIG_FILE']:
        config['FCG_CONFIG_FILE'] = None
    
    if configError:
        return None
    
    return config