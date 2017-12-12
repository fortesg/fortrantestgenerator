#!/usr/bin/python

'''
Created on 05.02.2016

@author: Christian Hovy
'''

import argparse;
from ftgconfigurator import loadFortranTestGeneratorConfiguration

def parseArguments(argParser):
    argParser.add_argument('-b', '--restore', action="store_true", help='Restore Backup Files')
    argParser.add_argument('-c', '--capture', action="store_true", help='Generate Capture Code')
    argParser.add_argument('-r', '--replay', action="store_true", help='Generate Replay Code')
    argParser.add_argument('-cf', '--configFile', type=str, help='Import configuration from this file.');
    argParser.add_argument('module', nargs='?', default=None);
    argParser.add_argument('subroutine', nargs='?', default=None);
    return argParser.parse_args();

def main():
    argParser = argparse.ArgumentParser(description='Generate test code.');
    args = parseArguments(argParser);
    if not (args.restore or args.capture or args.replay):
        argParser.error('No action requested, add -b/--restore and/or -c/--capture and/or -r/--replay')

    config = loadFortranTestGeneratorConfiguration(args.configFile)
    if config is None:
        exit(3)
        
    from fcgconfigurator import loadFortranCallGraphConfiguration
    from source import SubroutineFullName, SourceFiles
    from capture import CaptureCodeGenerator
    from replay import ReplayCodeGenerator
    from backup import BackupFileFinder
    from combined import CombinedCodeGenerator
    from assembler import FromAssemblerCallGraphBuilder
    from treecache import CachedAssemblerCallGraphBuilder
    
    config.update(loadFortranCallGraphConfiguration(config['FCG_CONFIG_FILE']))
    config.update(loadFortranTestGeneratorConfiguration(args.configFile)) # Load again to overwrite variables from FCG config file

    BACKUP_SUFFIX = config['BACKUP_SUFFIX']
    FTG_PREFIX = config['FTG_PREFIX']
    TEMPLATE_DIR = config['TEMPLATE_DIR']
    TEST_SOURCE_DIR = config['TEST_SOURCE_DIR']
    TEST_DATA_BASE_DIR = config['TEST_DATA_BASE_DIR']
    EXCLUDE_MODULES = config['EXCLUDE_MODULES']
    IGNORE_GLOBALS_FROM_MODULES = config['IGNORE_GLOBALS_FROM_MODULES']
    IGNORE_DERIVED_TYPES = config['IGNORE_DERIVED_TYPES']
    
    GRAPH_BUILDER = FromAssemblerCallGraphBuilder(config['ASSEMBLER_DIR'], config['SPECIAL_MODULE_FILES'])
    if config['CACHE_DIR']:
        GRAPH_BUILDER = CachedAssemblerCallGraphBuilder(config['CACHE_DIR'], GRAPH_BUILDER)
    SOURCE_FILES = SourceFiles(config['SOURCE_DIR'], config['SPECIAL_MODULE_FILES'], config['SOURCE_FILES_PREPROCESSED'])
    if config['MODIFY_SOURCE_DIR'] is not None:
        MODIFY_SOURCE_FILES = SourceFiles(config['MODIFY_SOURCE_DIR'], config['SPECIAL_MODULE_FILES'])
        BACKUP_FINDER = BackupFileFinder(config['MODIFY_SOURCE_DIR'], BACKUP_SUFFIX) 
    else:
        MODIFY_SOURCE_FILES = SOURCE_FILES
        BACKUP_FINDER = BackupFileFinder(config['SOURCE_DIR'], BACKUP_SUFFIX) 
        

    if args.capture or args.replay:
        moduleName = args.module;
        subroutineName = args.subroutine;
        
        subroutineFullName = None
        
        if not moduleName:
            argParser.error('Missing Module (and Subroutine) name!');
        elif not subroutineName:
            if SubroutineFullName.validFullName(moduleName):
                subroutineFullName = SubroutineFullName(moduleName)
            else: 
                argParser.error('Missing Subroutine name!');
        elif SubroutineFullName.validParts(moduleName, subroutineName):
            subroutineFullName = SubroutineFullName.fromParts(moduleName, subroutineName)
        else:
            argParser.error('Invalid Module and/or Subroutine name!');
            
        if subroutineFullName is not None and not SOURCE_FILES.existsSubroutine(subroutineFullName):
            argParser.error('Subroutine ' + str(subroutineFullName) + ' not found!');
        
    if args.restore:
        print 'Restore Backup Files'
        BACKUP_FINDER.restore()
        SOURCE_FILES.clearCache()
    else: 
        SOURCE_FILES.setSpecialModuleFiles(BACKUP_FINDER.extendSpecialModuleFiles(SOURCE_FILES.getSpecialModuleFiles()))

    if args.capture or args.replay:
        if args.capture and args.replay:
            print 'Generate capture and replay code'
            generator = CombinedCodeGenerator(SOURCE_FILES, TEMPLATE_DIR, TEST_SOURCE_DIR, TEST_DATA_BASE_DIR, GRAPH_BUILDER, BACKUP_SUFFIX, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, FTG_PREFIX)
        elif args.capture:
            print 'Generate capture code'
            generator = CaptureCodeGenerator(SOURCE_FILES, MODIFY_SOURCE_FILES, TEMPLATE_DIR, TEST_DATA_BASE_DIR, GRAPH_BUILDER, BACKUP_SUFFIX, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, FTG_PREFIX)
        else:
            print 'Generate replay code'
            generator = ReplayCodeGenerator(SOURCE_FILES, TEMPLATE_DIR, TEST_SOURCE_DIR, TEST_DATA_BASE_DIR, GRAPH_BUILDER, BACKUP_SUFFIX, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, FTG_PREFIX)
        generator.generate(subroutineFullName)
        
if __name__ == "__main__":
    main()