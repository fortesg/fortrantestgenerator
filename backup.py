import os
import shutil
from assertions import assertType, assertTypeAll

class BackupFileFinder(object):
    
    DEFAULT_SUFFIX = '.f90'
    CAPTURE_SUFFIX_PREFIX = '.capture'
    EXPORT_SUFFIX_PREFIX = '.export'
    
    def __init__(self, sourceDirs, backupSuffix):
        assertType(backupSuffix, 'backupSuffix', str)
        
        if isinstance(sourceDirs, str):
            sourceDirs = [sourceDirs]
        assertTypeAll(sourceDirs, 'sourceDirs', str)
        for baseDir in sourceDirs:    
            if not os.path.isdir(baseDir):
                raise IOError("Not a directory: " + baseDir);
        
        self.__sourceDirs = sourceDirs
        self.__backupSuffix = backupSuffix
        self.__suffixPrefix = ''
        
    def getBackupSuffix(self):
        return self.__suffixPrefix + self.__backupSuffix
    
    def setBackupSuffixPrefix(self, prefix):
        self.__suffixPrefix = prefix
        

    def find(self):
        backupFiles = []
        for sourceDir in self.__sourceDirs:
            for root, _, files in os.walk(sourceDir):
                for name in files:
                    if name.find(self.getBackupSuffix()) >= 0:
                        backupFiles.append(os.path.join(root, name))
        #Sorting makes capture backups to be restored before export backups
        # and export backups set to special module files
        return sorted(backupFiles) 
        
    def restore(self):
        for backupFile in self.find():
            restoreFile = self.__getOriginalFile(backupFile)
            print '  Restore ' + backupFile
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
    
    def create(self, originalPath):
        print "      Create File Backup of " + originalPath,
        backupPath = originalPath.replace(BackupFileFinder.DEFAULT_SUFFIX, self.getBackupSuffix())
        backupPath = backupPath.replace(BackupFileFinder.DEFAULT_SUFFIX.upper(), self.getBackupSuffix())
        if (backupPath == originalPath):
            backupPath = originalPath + self.getBackupSuffix()
        if not os.path.exists(backupPath):
            shutil.copyfile(originalPath, backupPath)
            print
            return True
        elif not os.path.exists(originalPath):
            shutil.copyfile(backupPath, originalPath)
            print
            return True
        else:
            print " >>> ALREADY EXISTS"
            return False
        
    def remove(self, originalPath):
        print "      Remove File Backup of " + originalPath,
        backupPath = originalPath.replace(BackupFileFinder.DEFAULT_SUFFIX, self.getBackupSuffix())
        backupPath = backupPath.replace(BackupFileFinder.DEFAULT_SUFFIX.upper(), self.getBackupSuffix())
        if (backupPath == originalPath):
            backupPath = originalPath + self.getBackupSuffix()
        if os.path.exists(backupPath):
            os.remove(backupPath)
            print
            return True
        else:
            print " >>> NOT FOUND"
            return False
            
    def __getOriginalFile(self, backupFile):
        originalFile = backupFile.replace(BackupFileFinder.EXPORT_SUFFIX_PREFIX, '')
        originalFile = originalFile.replace(BackupFileFinder.CAPTURE_SUFFIX_PREFIX, '')
        originalFile = originalFile.replace(self.__backupSuffix, BackupFileFinder.DEFAULT_SUFFIX)
        return originalFile
            
    def __getBackupFile(self, originalFile):
        backupFile = originalFile.replace(BackupFileFinder.DEFAULT_SUFFIX, self.__backupSuffix)
        backupFile = backupFile.replace(BackupFileFinder.DEFAULT_SUFFIX.upper(), self.__backupSuffix)
        return backupFile
            
    def __getModuleName(self, filE):
        return filE[(filE.rfind('/') + 1):filE.rfind('.')]