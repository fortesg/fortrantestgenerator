import os
from utils import assertType

class BackupFileFinder(object):
    
    DEFAULT_SUFFIX = 'f90'
    
    def __init__(self, sourceFolder, backupSuffix):
        assertType(sourceFolder, 'sourceFolder', str)
        if not os.path.isdir(sourceFolder):
            raise IOError("Not a directory: " + sourceFolder);
        assertType(backupSuffix, 'backupSuffix', str)
        
        self.__sourceFolder = sourceFolder
        self.__backupSuffix = backupSuffix

    def find(self):
        backupFiles = []
        for root, _, files in os.walk(self.__sourceFolder):
            for name in files:
                if name.find(self.__backupSuffix) >= 0:
                    backupFiles.append(os.path.join(root, name))
        return backupFiles
        
    def restore(self):
        for backupFile in self.find():
            restoreFile = self.__getOriginalFile(backupFile)
            print 'Restore ' + restoreFile
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
        return originalFile.replace(BackupFileFinder.DEFAULT_SUFFIX, self.__backupSuffix)
            
    def __getModuleName(self, filE):
        return filE[(filE.rfind('/') + 1):filE.rfind('.')]