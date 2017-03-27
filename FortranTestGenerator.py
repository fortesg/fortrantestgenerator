#!/usr/bin/python

'''
Created on 05.02.2016

@author: Christian Hovy
'''

import sys
import argparse;

from config_fortrantestgenerator import SOURCE_FOLDER, SOURCE_FILES, TEMPLATE_FOLDER, BACKUP_SUFFIX, TEST_SOURCE_FOLDER, TEST_DATA_BASE_FOLDER, GRAPH_BUILDER, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, EXCLUDE_MODULES, FTG_PREFIX

from source import SubroutineFullName
from capture import CaptureCodeGenerator
from replay import ReplayCodeGenerator
from backup import BackupFileFinder

def parseArguments():
    argParser = argparse.ArgumentParser(description='Generate test code.');
    captureOrTestCodeArg = argParser.add_mutually_exclusive_group(required=True)
    captureOrTestCodeArg.add_argument('-c', '--capture', action="store_true", help='Generate Capture Code')
    captureOrTestCodeArg.add_argument('-r', '--replay', action="store_true", help='Generate Replay Code')
    captureOrTestCodeArg.add_argument('-b', '--restore', action="store_true", help='Restore Backup Files')
    argParser.add_argument('module', nargs='?', default=None);
    argParser.add_argument('subroutine', nargs='?', default=None);
    return argParser.parse_args();

def main():
    args = parseArguments();

    backupFinder = BackupFileFinder(SOURCE_FOLDER, BACKUP_SUFFIX)

    if args.restore:
        print 'Restore Backup Files'
        backupFinder.restore() 
    else:
        moduleName = args.module;
        subroutineName = args.subroutine;
        
        subroutineFullName = None
        
        if not moduleName:
            print >> sys.stderr, 'Missing Module (and Subroutine) name!';
            exit(1);
        elif not subroutineName:
            if SubroutineFullName.validFullName(moduleName):
                subroutineFullName = SubroutineFullName(moduleName)
            else: 
                print >> sys.stderr, 'Missing Subroutine name!';
                exit(1);
        elif SubroutineFullName.validParts(moduleName, subroutineName):
            subroutineFullName = SubroutineFullName.fromParts(moduleName, subroutineName)
        else:
            print >> sys.stderr, 'Invalid Module and/or Subroutine name!';
            exit(1);
            
        if subroutineFullName is not None and not SOURCE_FILES.existsSubroutine(subroutineFullName):
            print >> sys.stderr, 'ERROR: Subroutine ' + str(subroutineFullName) + ' not found!';
            exit(2);
        
        if not SOURCE_FILES.existsSubroutine(subroutineFullName):
            print >> sys.stderr, 'ERROR: Subroutine ' + str(subroutineFullName) + ' not found!';
            exit(2);
            
        SOURCE_FILES.setSpecialModuleFiles(backupFinder.extendSpecialModuleFiles(SOURCE_FILES.getSpecialModuleFiles()))
        
        if args.capture:
            print 'Generate capture code'
            generator = CaptureCodeGenerator(SOURCE_FILES, TEMPLATE_FOLDER, TEST_DATA_BASE_FOLDER, GRAPH_BUILDER, BACKUP_SUFFIX, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES) 
        else:
            print 'Generate replay code'
            generator = ReplayCodeGenerator(SOURCE_FILES, TEMPLATE_FOLDER, TEST_SOURCE_FOLDER, TEST_DATA_BASE_FOLDER, GRAPH_BUILDER, BACKUP_SUFFIX, EXCLUDE_MODULES, IGNORE_GLOBALS_FROM_MODULES, IGNORE_DERIVED_TYPES, FTG_PREFIX)
            
        generator.generate(subroutineFullName)
        
if __name__ == "__main__":
    main()