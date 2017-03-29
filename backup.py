import os
from utils import assertType
from difflib import restore
import time
from posix import O_RDONLY

class BackupFileFinder(object):
    
    DEFAULT_SUFFIX = '.f90'
    
    def __init__(self, sourceDir, backupSuffix):
        assertType(sourceDir, 'sourceDir', str)
        if not os.path.isdir(sourceDir):
            raise IOError("Not a directory: " + sourceDir);
        assertType(backupSuffix, 'backupSuffix', str)
        
        self.__sourceDir = sourceDir
        self.__backupSuffix = '.' + backupSuffix.lstrip('.')

    def find(self):
        backupFiles = []
        for root, _, files in os.walk(self.__sourceDir):
            for name in files:
                if name.find(self.__backupSuffix) >= 0:
                    backupFiles.append(os.path.join(root, name))
        return backupFiles
        
    def restore(self):
        for backupFile in self.find():
            restoreFile = self.__getOriginalFile(backupFile)
            print '  Restore ' + restoreFile
            os.rename(backupFile, restoreFile)
            
    def extendSpecialModuleFiles(self, specialModuleFiles):
        assertType(specialModuleFiles, 'specialModuleFiles', dict)
        backupFiles = self.find()
        for module, filE in specialModuleFiles.iteritems():
            backupFile = self.__getBackupFile(filE)
            if backupFile in backupFiles:
                specialModuleFiles[module] = backupFile
                backupFiles.remove(backupFile)
        for backupFile in backupFiles:
            specialModuleFiles[self.__getModuleName(backupFile)] = backupFile[(backupFile.rfind('/') + 1):]
            
        return specialModuleFiles
            
    def __getOriginalFile(self, backupFile):
        return backupFile.replace(self.__backupSuffix, BackupFileFinder.DEFAULT_SUFFIX)
            
    def __getBackupFile(self, originalFile):
        backupFile = originalFile.replace(BackupFileFinder.DEFAULT_SUFFIX, self.__backupSuffix)
        backupFile = backupFile.replace(BackupFileFinder.DEFAULT_SUFFIX.upper(), self.__backupSuffix)
        return backupFile
            
    def __getModuleName(self, filE):
        return filE[(filE.rfind('/') + 1):filE.rfind('.')]