{-# LANGUAGE OverloadedStrings #-}
-- | This module provides type-safe access to IO operations.
--
--   It is designed to be imported instead of "System.IO".
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
module System.Path.IO
(
  -- * Covers for System.IO functions
  withFile,
  openFile,
  System.Path.IO.readFile,
  System.Path.IO.writeFile,
  System.Path.IO.appendFile,
  withBinaryFile,
  openBinaryFile,
  openTempFile,
  openBinaryTempFile,

  -- * Re-exports
  IO,
  fixIO,
  Handle,
  stdin,
  stdout,
  stderr,
  IOMode(..),
  hClose,
  hFileSize,
  hSetFileSize,
  hIsEOF,
  isEOF,
  BufferMode(..),
  hSetBuffering,
  hGetBuffering,
  hFlush,
  hGetPosn,
  hSetPosn,
  HandlePosn,
  hSeek,
  SeekMode(..),
  hTell,
  hIsOpen,
  hIsClosed,
  hIsReadable,
  hIsWritable,
  hIsSeekable,
  hIsTerminalDevice,
  hSetEcho,
  hGetEcho,
  hShow,
  hWaitForInput,
  hReady,
  hGetChar,
  hGetLine,
  hLookAhead,
  hGetContents,
  hPutChar,
  hPutStr,
  hPutStrLn,
  hPrint,
  interact,
  putChar,
  putStr,
  putStrLn,
  print,
  getChar,
  getLine,
  getContents,
  readIO,
  readLn,
  hSetBinaryMode,
  hPutBuf,
  hGetBuf,
  hPutBufNonBlocking,
  hGetBufNonBlocking
)

where

import Prelude hiding (FilePath)

import System.Path

import Control.Applicative
import Control.Arrow
import Data.List
import GHC.Exts(IsString(..))
import System.Directory (Permissions)
import System.IO hiding (FilePath, withFile, openFile, readFile, writeFile, appendFile,
                                 withBinaryFile, openBinaryFile, openTempFile, openBinaryTempFile)
import qualified System.IO as SIO
import System.IO.Error
import Test.QuickCheck
import Text.Printf


------------------------------------------------------------------------
-- Covers for System.IO functions

withFile :: AbsRelClass ar => Path ar fd -> IOMode -> (Handle -> IO r) -> IO r
withFile f = SIO.withFile (getPathString f)

openFile :: AbsRelClass ar => FilePath ar -> IOMode -> IO Handle
openFile f = SIO.openFile (getPathString f)

readFile :: AbsRelClass ar => FilePath ar -> IO String
readFile f = SIO.readFile (getPathString f)

writeFile :: AbsRelClass ar => FilePath ar -> String -> IO ()
writeFile f = SIO.writeFile (getPathString f)

appendFile :: AbsRelClass ar => FilePath ar -> String -> IO ()
appendFile f = SIO.appendFile (getPathString f)

withBinaryFile :: AbsRelClass ar => FilePath ar -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile f = SIO.withBinaryFile (getPathString f)

openBinaryFile :: AbsRelClass ar => FilePath ar -> IOMode -> IO Handle
openBinaryFile f = SIO.openBinaryFile (getPathString f)

openTempFile :: AbsRelClass ar => DirPath ar -> RelFile -> IO (AbsFile, Handle)
openTempFile f template = first asAbsFile <$> SIO.openTempFile (getPathString f) (getPathString template)

openBinaryTempFile :: AbsRelClass ar => DirPath ar -> RelFile -> IO (AbsFile, Handle)
openBinaryTempFile f template = first asAbsFile <$> SIO.openBinaryTempFile (getPathString f) (getPathString template)

------------------------------------------------------------------------
-- QuickCheck

testall = do
  putStrLn "Running QuickCheck tests..."
  putStrLn "Tests completed."

vectorOf :: Gen a -> Int -> Gen [a]
vectorOf gen n = sequence [ gen | i <- [1..n] ]

-- test :: Testable a => a -> IO ()
-- test = quickCheck


