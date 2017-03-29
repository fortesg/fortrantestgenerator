#!/usr/bin/python

'''
Created on 05.02.2016

@author: Christian Hovy
'''

import argparse;

from config_fortrantestgenerator import SOURCE_DIR, SOURCE_FILES, TEMPLATE_DIR, BACKUP_SUFFIX, TEST_SOURCE_DIR, TEST_DATA_BASE_DIR, GRAPH_BUILDER, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, EXCLUDE_MODULES, FTG_PREFIX

from source import SubroutineFullName
from capture import CaptureCodeGenerator
from replay import ReplayCodeGenerator
from backup import BackupFileFinder
from combined import CombinedCodeGenerator
import time

def parseArguments(argParser):
    
    argParser.add_argument('-b', '--restore', action="store_true", help='Restore Backup Files')
    argParser.add_argument('-c', '--capture', action="store_true", help='Generate Capture Code')
    argParser.add_argument('-r', '--replay', action="store_true", help='Generate Replay Code')
    argParser.add_argument('module', nargs='?', default=None);
    argParser.add_argument('subroutine', nargs='?', default=None);
    return argParser.parse_args();

def main():
    argParser = argparse.ArgumentParser(description='Generate test code.');
    args = parseArguments(argParser);

    if not (args.restore or args.capture or args.replay):
        argParser.error('No action requested, add -b/--restore and/or -c/--capture and/or -r/--replay')

    backupFinder = BackupFileFinder(SOURCE_DIR, BACKUP_SUFFIX) 

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
        
        if not SOURCE_FILES.existsSubroutine(subroutineFullName):
            argParser.error('Subroutine ' + str(subroutineFullName) + ' not found!');
        
    if args.restore:
        print 'Restore Backup Files'
        backupFinder.restore()
    else: 
        SOURCE_FILES.setSpecialModuleFiles(backupFinder.extendSpecialModuleFiles(SOURCE_FILES.getSpecialModuleFiles()))

    if args.capture or args.replay:
        if args.capture and args.replay:
            print 'Generate capture and replay code'
            generator = CombinedCodeGenerator(SOURCE_FILES, TEMPLATE_DIR, TEST_SOURCE_DIR, TEST_DATA_BASE_DIR, GRAPH_BUILDER, BACKUP_SUFFIX, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, FTG_PREFIX)
        elif args.capture:
            print 'Generate capture code'
            generator = CaptureCodeGenerator(SOURCE_FILES, TEMPLATE_DIR, TEST_DATA_BASE_DIR, GRAPH_BUILDER, BACKUP_SUFFIX, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, FTG_PREFIX)
        else:
            print 'Generate replay code'
            generator = ReplayCodeGenerator(SOURCE_FILES, TEMPLATE_DIR, TEST_SOURCE_DIR, TEST_DATA_BASE_DIR, GRAPH_BUILDER, BACKUP_SUFFIX, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, FTG_PREFIX)
        generator.generate(subroutineFullName)
        
if __name__ == "__main__":
    main()