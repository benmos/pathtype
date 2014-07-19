{-# LANGUAGE CPP, OverloadedStrings #-}
-- | This module provides type-safe access to directory manipulations.
--
--   It is designed to be imported instead of "System.Directory".
--   (It is intended to provide versions of functions from that
--   module which have equivalent functionality but are more
--   typesafe). "System.Path" is a companion module providing
--   a type-safe alternative to "System.FilePath".
--
--   You will typically want to import as follows:
--
--   > import Prelude hiding (FilePath)
--   > import System.Path
--   > import System.Path.Directory
--   > import System.Path.IO
--
--
-- Ben Moseley - (c) 2009
--
module System.Path.Directory
(
  -- * Actions on directories
  createDirectory,
  createDirectoryIfMissing,
  removeDirectory,
  removeDirectoryRecursive,
  renameDirectory,

  getDirectoryContents,
  absDirectoryContents,
  relDirectoryContents,
  filesInDir,
  dirsInDir,

  getCurrentDirectory,
  setCurrentDirectory,

  -- * Pre-defined directories
  getHomeDirectory,
  getAppUserDataDirectory,
  getUserDocumentsDirectory,
  getTemporaryDirectory,

  -- * Actions on files
  removeFile,
  renameFile,
  copyFile,
  canonicalizePath,
  makeRelativeToCurrentDirectory,
  findExecutable,

  -- * Existence tests
  doesFileExist,
  doesDirectoryExist,

  -- * Permissions
  Permissions,
  getPermissions,
  setPermissions,

  -- * Timestamps
  getModificationTime
)

where

import Prelude hiding (FilePath)

import System.Path

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Time
import GHC.Exts(IsString(..))
import System.Directory (Permissions)
import qualified System.Directory as SD
import System.IO hiding (FilePath)
import System.IO.Error
import Test.QuickCheck
import Text.Printf

-- directory < 1.2 used old-time; to present a consistent API we convert it
-- time:Data.Time.UTCTime.
#if !(MIN_VERSION_directory(1,2,0))
import System.Time
#endif

------------------------------------------------------------------------
-- Actions on directories

createDirectory :: AbsRelClass ar => DirPath ar -> IO ()
createDirectory = SD.createDirectory . getPathString

createDirectoryIfMissing :: AbsRelClass ar => Bool -> DirPath ar -> IO ()
createDirectoryIfMissing flag = SD.createDirectoryIfMissing flag . getPathString

removeDirectory :: AbsRelClass ar => DirPath ar -> IO ()
removeDirectory = SD.removeDirectory . getPathString

removeDirectoryRecursive :: AbsRelClass ar => DirPath ar -> IO ()
removeDirectoryRecursive = SD.removeDirectoryRecursive . getPathString

renameDirectory :: (AbsRelClass ar1, AbsRelClass ar2) => DirPath ar1 -> DirPath ar2 -> IO ()
renameDirectory p1 p2 = SD.renameDirectory (getPathString p1) (getPathString p2)

-- | An alias for 'relDirectoryContents'.
getDirectoryContents :: AbsRelClass ar => DirPath ar -> IO ([RelDir], [RelFile])
getDirectoryContents = relDirectoryContents

-- | Retrieve the contents of a directory path (which may be relative) as absolute paths
absDirectoryContents :: AbsRelClass ar => DirPath ar -> IO ([AbsDir], [AbsFile])
absDirectoryContents p = do
  cd <- asAbsDir <$> SD.getCurrentDirectory
  let dir = absRel id (cd </>) p
  (rds, rfs) <- relDirectoryContents dir
  return (map (dir </>) rds, map (dir </>) rfs)

-- | Returns paths relative /to/ the supplied (abs or relative) directory path.
--   eg (for current working directory of @\/somewhere\/cwd\/@):
--
-- > show (relDirectoryContents "d/e/f/") == (["subDir1A","subDir1B"],
-- >                                                      ["file1A","file1B"])
--
relDirectoryContents :: AbsRelClass ar => DirPath ar -> IO ([RelDir], [RelFile])
relDirectoryContents dir = do
  filenames <- filter (not . flip elem [".",".."]) <$> SD.getDirectoryContents (getPathString dir)
  dirFlags  <- mapM (doesDirectoryExist . (dir </>) . asRelPath) filenames
  let fileinfo = zip filenames dirFlags
      (dirs, files) = partition snd fileinfo
  return (map (combine currentDir . asRelDir . fst) dirs,
          map (combine currentDir . asRelFile . fst) files)

-- | A convenient alternative to 'relDirectoryContents' if you only want files.
filesInDir :: AbsRelClass ar => DirPath ar -> IO [RelFile]
filesInDir dir = snd <$> relDirectoryContents dir

-- | A convenient alternative to 'relDirectoryContents' if you only want directories.
dirsInDir :: AbsRelClass ar => DirPath ar -> IO [RelDir]
dirsInDir dir = fst <$> relDirectoryContents dir


getCurrentDirectory :: IO AbsDir
getCurrentDirectory = asAbsDir <$> SD.getCurrentDirectory

setCurrentDirectory :: AbsRelClass ar => DirPath ar -> IO ()
setCurrentDirectory = SD.setCurrentDirectory . getPathString


------------------------------------------------------------------------
-- Pre-defined directories

getHomeDirectory :: IO AbsDir
getHomeDirectory = asAbsDir <$> SD.getHomeDirectory

getAppUserDataDirectory :: String -> IO AbsDir
getAppUserDataDirectory user = asAbsDir <$> SD.getAppUserDataDirectory user

getUserDocumentsDirectory :: IO AbsDir
getUserDocumentsDirectory = asAbsDir <$> SD.getUserDocumentsDirectory

getTemporaryDirectory :: IO AbsDir
getTemporaryDirectory = asAbsDir <$> SD.getTemporaryDirectory


------------------------------------------------------------------------
-- Actions on files

removeFile :: AbsRelClass ar => FilePath ar -> IO ()
removeFile = SD.removeFile . getPathString

renameFile :: (AbsRelClass ar1, AbsRelClass ar2) => FilePath ar1 -> FilePath ar2 -> IO ()
renameFile p1 p2 = SD.renameFile (getPathString p1) (getPathString p2)

copyFile :: (AbsRelClass ar1, AbsRelClass ar2) => FilePath ar1 -> FilePath ar2 -> IO ()
copyFile p1 p2 = SD.copyFile (getPathString p1) (getPathString p2)

canonicalizePath :: AbsRelClass ar => Path ar fd -> IO (AbsPath fd)
canonicalizePath p = asPath <$> SD.canonicalizePath (getPathString p)

makeRelativeToCurrentDirectory :: AbsRelClass ar => Path ar fd -> IO (RelPath fd)
makeRelativeToCurrentDirectory p = asPath <$> SD.makeRelativeToCurrentDirectory (getPathString p)

findExecutable :: String -> IO (Maybe AbsFile)
findExecutable s = fmap asPath <$> SD.findExecutable s


------------------------------------------------------------------------
-- Existence tests

doesFileExist :: AbsRelClass ar => FilePath ar -> IO Bool
doesFileExist = SD.doesFileExist . getPathString

doesDirectoryExist :: AbsRelClass ar => DirPath ar -> IO Bool
doesDirectoryExist = SD.doesDirectoryExist . getPathString


------------------------------------------------------------------------
-- Permissions

getPermissions :: AbsRelClass ar => Path ar fd -> IO Permissions
getPermissions p = SD.getPermissions (getPathString p)

setPermissions :: AbsRelClass ar => Path ar fd -> Permissions -> IO ()
setPermissions p perms = SD.setPermissions (getPathString p) perms


------------------------------------------------------------------------
-- Timestamps

getModificationTime :: AbsRelClass ar => Path ar fd -> IO UTCTime
#if MIN_VERSION_directory(1,2,0)
getModificationTime p = SD.getModificationTime (getPathString p)
#else
-- directory < 1.2 used the old-time package, convert to UTCTime
getModificationTime p = convertTime <$> SD.getModificationTime (getPathString p)
  where
    convertTime clock = UTCTime day (fromIntegral sec)
      where
        calendar = toUTCTime clock
        day = addDays (toInteger (ctYDay calendar)) yearStart
        yearStart = fromGregorian (toInteger (ctYear calendar)) 1 1
        hour = ctHour calendar
        min = hour * 60 + ctMin calendar
        sec = min * 60 + ctSec calendar
#endif



------------------------------------------------------------------------
-- QuickCheck

testall = do
  putStrLn "Running QuickCheck tests..."
  putStrLn "Tests completed."

vectorOf :: Gen a -> Int -> Gen [a]
vectorOf gen n = sequence [ gen | i <- [1..n] ]

-- test :: Testable a => a -> IO ()
-- test = quickCheck


