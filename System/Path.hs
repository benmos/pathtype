{-# LANGUAGE CPP #-}
-- | This module provides type-safe access to filepath manipulations.
--
--   It is designed to be imported instead of "System.FilePath".
--   (It is intended to provide versions of functions from that
--   module which have equivalent functionality but are more
--   typesafe). "System.Path.Directory" is a companion module
--   providing a type-safe alternative to "System.Directory".
--
--   The heart of this module is the @'Path' ar fd@ abstract type which
--   represents file and directory paths. The idea is that there are
--   two phantom type parameters - the first should be 'Abs' or 'Rel',
--   and the second 'File' or 'Dir'. A number of type synonyms are
--   provided for common types:
--
--   > type AbsFile     = Path Abs File
--   > type RelFile     = Path Rel File
--   > type AbsDir      = Path Abs Dir
--   > type RelDir      = Path Rel Dir
--   >
--   > type AbsPath  fd = Path Abs fd
--   > type RelPath  fd = Path Rel fd
--   > type FilePath ar = Path ar File
--   > type DirPath  ar = Path ar Dir
--
--   The type of the 'combine' (aka '</>') function gives the idea:
--
--   > (</>) :: DirPath ar -> RelPath fd -> Path ar fd
--
--   Together this enables us to give more meaningful types to
--   a lot of the functions, and (hopefully) catch a bunch more
--   errors at compile time.
--
--   Overloaded string literals are supported, so with the @OverloadedStrings@
--   extension enabled, you can:
--
--   > f :: FilePath ar
--   > f = "tmp" </> "someFile" <.> "ext"
--
--   If you don't want to use @OverloadedStrings@, you can use the construction fns:
--
--   > f :: FilePath ar
--   > f = asDirPath "tmp" </> asFilePath "someFile" <.> "ext"
--
--   or...
--
--   > f :: FilePath ar
--   > f = asPath "tmp" </> asPath "someFile" <.> "ext"
--
--   or just...
--
--   > f :: FilePath ar
--   > f = asPath "tmp/someFile.ext"
--
--   One point to note is that whether one of these is interpreted as
--   an absolute or a relative path depends on the type at which it is
--   used:
--
--   > *System.Path> f :: AbsFile
--   > /tmp/someFile.ext
--   > *System.Path> f :: RelFile
--   > tmp/someFile.ext
--
--   You will typically want to import as follows:
--
--   > import Prelude hiding (FilePath)
--   > import System.Path
--   > import System.Path.Directory
--   > import System.Path.IO
--
--   The basic API (and properties satisfied) are heavily influenced
--   by Neil Mitchell's "System.FilePath" module.
--
--
-- Ben Moseley - (c) 2009-2010
--

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module System.Path(module System.Path.Windows) where
import System.Path.Windows
#else
module System.Path(module System.Path.Posix) where
import System.Path.Posix
#endif


