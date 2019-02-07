from __future__ import print_function
import sys

def printInline(line, indent = 0):
    print('  ' * indent + str(line), end='')

def printLine(line = '', indent = 0):
    print('  ' * indent + str(line))
    
def printError(line, location = ''):
    msg = '*** ERROR'
    if location:
        msg += ' [' + location + ']'
    msg += ': ' + str(line) + ' ***'
    print(msg, file=sys.stderr)
    
def printErrorAndExit(exitCode, line, location = ''):
    printError(line, location)
    sys.exit(exitCode)
    
def printWarning(line, location = ''):
    msg = '*** WARNING'
    if location:
        msg += ' [' + location + ']'
    msg += ': ' + str(line) + ' ***'
    print(msg, file=sys.stderr)
    
def printDebug(line, location = ''):
    msg = '*** DEBUG'
    if location:
        msg += ' [' + location + ']'
    msg += ': ' + str(line) + ' ***'
    print(msg, file=sys.stderr)