#!/usr/bin/python

'''
Created on 05.02.2016

@author: Christian Hovy
'''

import sys
import argparse;
from ftgconfigurator import loadFortranTestGeneratorConfiguration, CFG_TEMPLATE, CFG_BACKUP_SUFFIX, CFG_FTG_PREFIX,\
    CFG_TEST_SOURCE_DIR, CFG_TEST_DATA_BASE_DIR, CFG_MODIFY_SOURCE_DIRS, CFG_FCG_CONFIG_FILE, CFG_FTG_CONFIG_FILE, CFG_FCG_DIR
from printout import printLine
from postprocessor import CodePostProcessor

def parseArguments(argParser):
    argParser.add_argument('-a', '--restoreCapture', action="store_true", help='Restore only Capture Backup Files')
    argParser.add_argument('-b', '--restore', action="store_true", help='Restore all Backup Files')
    argParser.add_argument('-c', '--capture', action="store_true", help='Generate Capture Code')
    argParser.add_argument('-e', '--export', action="store_true", help='Generate Export Code')
    argParser.add_argument('-r', '--replay', action="store_true", help='Generate Replay Code')
    argParser.add_argument('-m', '--measure', action="store_true", help='Measure Time')
    argParser.add_argument('-cf', '--configFile', type=str, help='Import configuration from this file.');
    argParser.add_argument('module', nargs='?', default=None, help='Module name');
    argParser.add_argument('subroutine', nargs='?', default=None, help='Subroutine or function name');
    return argParser.parse_args();

def main():
    argParser = argparse.ArgumentParser(description='Generate test code.');
    args = parseArguments(argParser);
    if not (args.restore or args.restoreCapture or args.export or args.capture or args.replay):
        argParser.error('No action requested, add -a/--restoreCapture and/or -b/--restore and/or -c/--capture and/or -e/--export and/or -r/--replay')

    config = loadFortranTestGeneratorConfiguration(args.configFile)
    if config is None:
        exit(3)
        
    backupSuffix = '.' + config[CFG_BACKUP_SUFFIX].lstrip('.')
    ftgPrefix = config[CFG_FTG_PREFIX]
    templatePath = config[CFG_TEMPLATE]
    testSourceDir = config[CFG_TEST_SOURCE_DIR]
    testDataBaseDir = config[CFG_TEST_DATA_BASE_DIR]
    modifySourceDirs = config[CFG_MODIFY_SOURCE_DIRS]
        
    sys.path.append(config[CFG_FCG_DIR])
        
    from fcgconfigurator import loadFortranCallGraphConfiguration, CFG_EXCLUDE_MODULES, CFG_IGNORE_GLOBALS_FROM_MODULES, CFG_IGNORE_DERIVED_TYPES, CFG_ASSEMBLER_DIRS,\
    CFG_SPECIAL_MODULE_FILES, CFG_CACHE_DIR, CFG_SOURCE_DIRS, CFG_SOURCE_FILES_PREPROCESSED, CFG_ABSTRACT_TYPES
    from source import SubroutineFullName, SourceFiles
    from backup import BackupFileFinder
    from combined import CombinedCodeGenerator
    from assembler import GNUx86AssemblerCallGraphBuilder
    from treecache import CachedAssemblerCallGraphBuilder
    
    configFTG = {}
    if config[CFG_FCG_CONFIG_FILE] is not None:
        configFTG = loadFortranCallGraphConfiguration(config[CFG_FCG_CONFIG_FILE], incomplete=True)
    configFTG = loadFortranCallGraphConfiguration(config[CFG_FTG_CONFIG_FILE], baseConfig=configFTG) # Overwrite variables from FCG config file
    if configFTG is None:
        exit(3)
    config.update(configFTG)

    excludeModules = config[CFG_EXCLUDE_MODULES]
    ignoreGlobalsFromModuls = config[CFG_IGNORE_GLOBALS_FROM_MODULES]
    ignoreDerivedTypes = config[CFG_IGNORE_DERIVED_TYPES]
    abstractTypes = config[CFG_ABSTRACT_TYPES]
    sourceDirs = config[CFG_SOURCE_DIRS]

    graphBuilder = GNUx86AssemblerCallGraphBuilder(config[CFG_ASSEMBLER_DIRS], config[CFG_SPECIAL_MODULE_FILES])
    if config[CFG_CACHE_DIR]:
        graphBuilder = CachedAssemblerCallGraphBuilder(config[CFG_CACHE_DIR], graphBuilder)
    sourceFiles = SourceFiles(sourceDirs, config[CFG_SPECIAL_MODULE_FILES], config[CFG_SOURCE_FILES_PREPROCESSED])
    if modifySourceDirs is not None:
        modifySourceFiles = SourceFiles(modifySourceDirs, config[CFG_SPECIAL_MODULE_FILES])
        backupFinder = BackupFileFinder(modifySourceDirs, backupSuffix) 
    else:
        modifySourceFiles = sourceFiles
        backupFinder = BackupFileFinder(sourceDirs, backupSuffix) 

    if args.export or args.capture or args.replay:
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
            
        if subroutineFullName is not None and not sourceFiles.existsSubroutine(subroutineFullName):
            argParser.error('Subroutine ' + str(subroutineFullName) + ' not found!');
        
    if args.restore:
        printLine('Restore all Backup Files')
        backupFinder.restore()
        sourceFiles.clearCache()
    elif args.restoreCapture:
        printLine('Restore Capture Backup Files')
        backupFinder.setBackupSuffixPrefix(BackupFileFinder.CAPTURE_SUFFIX_PREFIX)
        backupFinder.restore()
        sourceFiles.clearCache()
    elif modifySourceDirs is None or modifySourceDirs == sourceDirs: 
        sourceFiles.setSpecialModuleFiles(backupFinder.extendSpecialModuleFiles(sourceFiles.getSpecialModuleFiles()))

    if args.export or args.capture or args.replay:
        printLine('Generate Code')
        postProcessor = CodePostProcessor()
        generator = CombinedCodeGenerator(args.capture, args.replay, sourceFiles, modifySourceFiles, templatePath, testSourceDir, testDataBaseDir, graphBuilder, postProcessor, backupFinder, excludeModules, ignoreGlobalsFromModuls, ignoreDerivedTypes, ftgPrefix, abstractTypes, args.measure)
        generator.generate(subroutineFullName)
        
if __name__ == "__main__":
    main()