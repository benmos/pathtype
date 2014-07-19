
-- LANGUAGE pragmas need to go in System.Path.[Windows|Posix]
module System.Path.MODULE_NAME
(
  -- * The main filepath (& dirpath) abstract type
  Path, -- kept abstract

  -- * Phantom Types
  Abs,
  Rel,
  File,
  Dir,

  -- * Type Synonyms
  AbsFile,
  RelFile,
  AbsDir,
  RelDir,
  AbsPath,
  RelPath,
  FilePath,
  DirPath,

  -- * Classes
  AbsRelClass(..),
  FileDirClass(..),

  -- * Path to String conversion
  getPathString,

  -- * Constants
  rootDir,
  currentDir,

  -- * Unchecked Construction Functions
  asPath,
  asRelFile,
  asRelDir,
  asAbsFile,
  asAbsDir,
  asRelPath,
  asAbsPath,
  asFilePath,
  asDirPath,

  -- * Checked Construction Functions
  mkPathAbsOrRel,
  mkPathFileOrDir,
  mkAbsPath,
  mkAbsPathFromCwd,

  -- * Basic Manipulation Functions
  (</>),
  (<.>),
  addExtension,
  combine,
  dropExtension,
  dropExtensions,
  dropFileName,
  replaceExtension,
  replaceBaseName,
  replaceDirectory,
  replaceFileName,
  splitExtension,
  splitExtensions,
  splitFileName,
  takeBaseName,
  takeDirectory,
  takeExtension,
  takeExtensions,
  takeFileName,

  -- * Auxillary Manipulation Functions
  equalFilePath,
  joinPath,
  normalise,
  splitPath,
  makeRelative,
  makeAbsolute,
  makeAbsoluteFromCwd,
  genericMakeAbsolute,
  genericMakeAbsoluteFromCwd,
  pathMap,

  -- * Path Predicates
  isAbsolute,
  isAbsoluteString,
  isRelative,
  isRelativeString,
  hasAnExtension,
  hasExtension,

  -- * Separators
  addTrailingPathSeparator,
  dropTrailingPathSeparator,
  extSeparator,
  hasTrailingPathSeparator,
  pathSeparator,
  pathSeparators,
  searchPathSeparator,
  isExtSeparator,
  isPathSeparator,
  isSearchPathSeparator,

  -- * Generic Manipulation Functions
  genericAddExtension,
  genericDropExtension,
  genericDropExtensions,
  genericSplitExtension,
  genericSplitExtensions,
  genericTakeExtension,
  genericTakeExtensions
)

where

import Prelude hiding (FilePath)

import Control.Applicative
import Control.Arrow
import Data.List
import GHC.Exts( IsString(..) )
import qualified System.Directory as SD

import System.IO hiding (FilePath)
import System.IO.Error
import Text.Printf
import Test.QuickCheck


------------------------------------------------------------------------
-- Types

data Abs
data Rel
data File
data Dir

-- | This is the main filepath abstract datatype
data Path ar fd = PathRoot -- ^ Invariant - this should always have type :: DirPath ar
                | FileDir !(DirPath ar) !PathComponent -- The fact that we recurse binding fd to Dir
                                                       -- makes this a "nested datatype"
                  deriving (Eq, Ord)

-- Possible GADT version...
--
-- data Path ar fd where
--   AbsRoot :: Path Abs Dir
--   RelRoot :: Path Rel Dir
--   File    :: Path ar Dir -> PathComponent -> Path ar File
--   Dir     :: Path ar Dir -> PathComponent -> Path ar Dir
--
-- ... doesn't presently seem to add much value over non-GADT.

newtype PathComponent = PathComponent { unPathComponent :: String } deriving (Eq,Ord)
instance Show PathComponent where showsPrec _ (PathComponent s) = showString s

type AbsFile = Path Abs File
type RelFile = Path Rel File
type AbsDir  = Path Abs Dir
type RelDir  = Path Rel Dir

type AbsPath  fd = Path Abs fd
type RelPath  fd = Path Rel fd
type FilePath ar = Path ar File
type DirPath  ar = Path ar Dir

-- I don't think this basic type of fold is appropriate for a nested datatype
-- pathFold :: a -> (a -> String -> a) -> Path ar fd -> a
-- pathFold pr f PathRoot = pr
-- pathFold pr f (FileDir d pc) = f (pathFold pr f d) (unPathComponent pc)

-- | Map over the components of the path.
--
-- >> pathMap (map toLower) "/tmp/Reports/SpreadSheets" == "/tmp/reports/spreadsheets"
pathMap :: (String -> String) -> Path ar fd -> Path ar fd
pathMap f PathRoot = PathRoot
pathMap f (FileDir d pc) = FileDir (pathMap f d) (pcMap f pc)

-- Private fn
pcMap :: (String -> String) -> PathComponent -> PathComponent
pcMap f (PathComponent s) = PathComponent (f s)


------------------------------------------------------------------------
-- Type classes and machinery for switching on Abs/Rel and File/Dir

-- | This class provides a way to prevent other modules
--   from making further 'AbsRelClass' or 'FileDirClass'
--   instances
class Private p
instance Private Abs
instance Private Rel
instance Private File
instance Private Dir

-- | This class allows selective behaviour for absolute and
--   relative paths and is mostly for internal use.
class Private ar => AbsRelClass ar where
    absRel :: (AbsPath fd -> a) -> (RelPath fd -> a) -> Path ar fd -> a

instance AbsRelClass Abs where absRel f g = f
instance AbsRelClass Rel where absRel f g = g

-- | This class allows selective behaviour for file and
--   directory paths and is mostly for internal use.
class Private fd => FileDirClass fd where
    fileDir :: (FilePath ar -> a) -> (DirPath ar -> a) -> Path ar fd -> a

instance FileDirClass File where fileDir f g = f
instance FileDirClass Dir  where fileDir f g = g


-- | Currently not exported
pathAbsRel :: AbsRelClass ar => Path ar fd -> Either (AbsPath fd) (RelPath fd)
pathAbsRel = absRel Left Right

-- | Currently not exported
pathFileDir :: FileDirClass fd => Path ar fd -> Either (FilePath ar) (DirPath ar)
pathFileDir = fileDir Left Right

------------------------------------------------------------------------
-- Read & Show instances

instance AbsRelClass ar => Show (Path ar fd) where
    showsPrec d x@PathRoot                = absRel (const $ showString pathSeparators)
                                                   (const $ showString ".") x
    -- we need the clause below so that we don't duplicate the pathSeparator after an abs
    -- root and we don't want to display a "./" prefix on relative paths
    showsPrec d x@(FileDir p@PathRoot pc) = absRel (const $ showString pathSeparators)
                                                   (const id)
                                                   p .
                                            showsPrec d pc
    showsPrec d x@(FileDir p pc)          = showsPrec d p . showString pathSeparators .
                                            showsPrec d pc

-- This instance consumes all remaining input. Would it be better to, say,
-- give up at newlines or some set of non-allowable chars?
instance AbsRelClass ar => Read (Path ar fd) where
    readsPrec _ s = [(asPath s,"")]

-- | Convert the 'Path' into a plain 'String'. This is simply an
--   alias for 'show'.
getPathString :: AbsRelClass ar => Path ar fd -> String
getPathString = show

prop_asPath_getPathString :: AbsFile -> Property
prop_asPath_getPathString p = property $ p == asPath (getPathString p)


------------------------------------------------------------------------
-- Windows / Posix

isPosix :: Bool
isPosix = not isWindows

isWindows :: Bool
isWindows = IS_WINDOWS

------------------------------------------------------------------------
-- Constants

rootDir :: AbsDir
rootDir = PathRoot

currentDir :: RelDir
currentDir = PathRoot


------------------------------------------------------------------------
-- Unchecked Construction Functions
-- NB - these construction functions are pure and do no checking!!

-- | Use a 'String' as a 'Path' whose type is determined
--   by its context.
--
-- >> asPath "/tmp" == "/tmp"
-- >> asPath "file.txt" == "file.txt"
-- >> isAbsolute (asPath "/tmp" :: AbsDir) == True
-- >> isAbsolute (asPath "/tmp" :: RelDir) == False
-- >> getPathString (asPath "/tmp" :: AbsDir) == "/tmp"
-- >> getPathString (asPath "/tmp" :: RelDir) == "tmp"
asPath :: String -> Path ar fd
asPath = mkPathFromComponents . mkPathComponents

-- | Use a 'String' as a 'RelFile'. No checking is done.
--
-- >> getPathString (asRelFile "file.txt") == "file.txt"
-- >> getPathString (asRelFile "/file.txt") == "file.txt"
-- >> getPathString (asRelFile "tmp") == "tmp"
-- >> getPathString (asRelFile "/tmp") == "tmp"
asRelFile :: String -> RelFile
asRelFile = asPath

-- | Use a 'String' as a 'RelDir'. No checking is done.
--
-- >> getPathString (asRelDir "file.txt") == "file.txt"
-- >> getPathString (asRelDir "/file.txt") == "file.txt"
-- >> getPathString (asRelDir "tmp") == "tmp"
-- >> getPathString (asRelDir "/tmp") == "tmp"
asRelDir :: String -> RelDir
asRelDir = asPath

-- | Use a 'String' as an 'AbsFile'. No checking is done.
--
-- >> getPathString (asAbsFile "file.txt") == "/file.txt"
-- >> getPathString (asAbsFile "/file.txt") == "/file.txt"
-- >> getPathString (asAbsFile "tmp") == "/tmp"
-- >> getPathString (asAbsFile "/tmp") == "/tmp"
asAbsFile :: String -> AbsFile
asAbsFile = asPath

-- | Use a 'String' as an 'AbsDir'. No checking is done.
--
-- >> getPathString (asAbsDir "file.txt") == "/file.txt"
-- >> getPathString (asAbsDir "/file.txt") == "/file.txt"
-- >> getPathString (asAbsDir "tmp") == "/tmp"
-- >> getPathString (asAbsDir "/tmp") == "/tmp"
asAbsDir :: String -> AbsDir
asAbsDir = asPath

-- | Use a 'String' as a 'RelPath fd'. No checking is done.
asRelPath :: String -> RelPath fd
asRelPath = asPath

-- | Use a 'String' as an 'AbsPath fd'. No checking is done.
asAbsPath :: String -> AbsPath fd
asAbsPath = asPath

-- | Use a 'String' as a 'FilePath ar'. No checking is done.
asFilePath :: String -> FilePath ar
asFilePath = asPath

-- | Use a 'String' as a 'DirPath ar'. No checking is done.
asDirPath :: String -> DirPath ar
asDirPath = asPath

-- | Allow use of OverloadedStrings if desired
instance IsString (Path ar fd) where fromString = asPath

------------------------------------------------------------------------
-- Checked Construction Functions

-- | Examines the supplied string and constructs an absolute or
-- relative path as appropriate.
--
-- >> either id (const "fred") (mkPathAbsOrRel "/tmp") == "/tmp"
-- >> either id (const "fred") (mkPathAbsOrRel  "tmp") == "fred"
mkPathAbsOrRel :: String -> Either (AbsPath fd) (RelPath fd)
mkPathAbsOrRel s | isAbsoluteString s = Left (asPath s)
                 | otherwise = Right (asPath s)

-- | Searches for a file or directory with the supplied path string
--   and returns a 'File' or 'Dir' path as appropriate. If neither exists
--   at the supplied path, 'Nothing' is returned.
mkPathFileOrDir :: AbsRelClass ar => String -> IO (Maybe (Either (FilePath ar) (DirPath ar)))
mkPathFileOrDir s = do
  isfile <- doesFileExist `onPathString` s
  isdir <- doesDirectoryExist `onPathString` s
  case (isfile, isdir) of
    (False, False) -> return Nothing
    (True,  False) -> return $ Just $ Left $ asPath s
    (False, True ) -> return $ Just $ Right $ asPath s
    (True,  True ) -> ioError $ userError "mkPathFileOrDir - internal inconsistency - file&dir"
  where
    -- We duplicate these from System.Path.Directory to avoid a module cycle
    doesFileExist      :: AbsRelClass ar => FilePath ar -> IO Bool
    doesFileExist      = SD.doesFileExist . getPathString
    doesDirectoryExist :: AbsRelClass ar => DirPath ar -> IO Bool
    doesDirectoryExist = SD.doesDirectoryExist . getPathString

-- | Convert a 'String' into an 'AbsPath' by interpreting it as
--   relative to the supplied directory if necessary.
--
-- >> mkAbsPath "/tmp" "foo.txt" == "/tmp/foo.txt"
-- >> mkAbsPath "/tmp" "/etc/foo.txt" == "/etc/foo.txt"
mkAbsPath :: AbsDir -> String -> AbsPath fd
mkAbsPath d = (id ||| makeAbsolute d) . mkPathAbsOrRel

-- | Convert a 'String' into an 'AbsPath' by interpreting it as
--   relative to the cwd if necessary.
mkAbsPathFromCwd :: String -> IO (AbsPath fd)
mkAbsPathFromCwd = (return ||| makeAbsoluteFromCwd) . mkPathAbsOrRel


-- | Lift a function which can operate on either Abs or Rel Path to one which
--   operates on Strings.
--   At present this fn is the only reason we have Rank-2 types, it's also not
--   doing anything useful at present. We /may/ want to expose it later though
--   so leave it for now...
onPathString :: (forall ar . AbsRelClass ar => Path ar fd -> a) -> String -> a
onPathString f = (f ||| f) . mkPathAbsOrRel


------------------------------------------------------------------------
-- Internal Functions for PathComponent manipulation

mkPathFromComponents :: [PathComponent] -> Path ar fd
mkPathFromComponents []  = PathRoot
mkPathFromComponents pcs | (p:ps) <- reverse pcs = FileDir (foldr (flip FileDir) PathRoot ps) p

mkPathComponents :: String -> [PathComponent]
mkPathComponents xs =
    case break isPathSeparator (dropWhile isPathSeparator xs) of
      ("","")  -> []
      (s,rest) -> PathComponent s : mkPathComponents rest

pathComponents :: Path ar fd -> [PathComponent]
pathComponents PathRoot = []
pathComponents (FileDir p pc) = pathComponents p ++ [pc]

prop_mkPathFromComponents_pathComponents :: AbsFile -> Property
prop_mkPathFromComponents_pathComponents p = property $
                                               mkPathFromComponents (pathComponents p) == p



------------------------------------------------------------------------
-- Basic Manipulation Functions

-- | Join an (absolute or relative) directory path with a relative
--   (file or directory) path to form a new path.
(</>) :: DirPath ar -> RelPath fd -> Path ar fd
PathRoot         </> PathRoot       = PathRoot
(FileDir dp dpc) </> PathRoot       = FileDir dp dpc
d                </> (FileDir p pc) = FileDir (d </> p) pc

-- | We only allow files (and not directories) to have extensions added
--   by this function. This is because it's the vastly common case and
--   an attempt to add one to a directory will - more often than not -
--   represent an error.
--   We don't however want to prevent the corresponding operation on
--   directories, and so we provide a function that is more flexible:
--   'genericAddExtension'.
(<.>) :: FilePath ar -> String -> FilePath ar
(<.>) = genericAddExtension

-- | Add an extension, even if there is already one there.
--   E.g. @addExtension \"foo.txt\" \"bat\" -> \"foo.txt.bat\"@.
--
-- >> addExtension "file.txt" "bib" == "file.txt.bib"
-- >> addExtension "file." ".bib" == "file..bib"
-- >> addExtension "file" ".bib" == "file.bib"
-- >> takeFileName (addExtension "" "ext") == ".ext"
addExtension :: FilePath ar -> String -> FilePath ar
addExtension = (<.>)

-- | Join an (absolute or relative) directory path with a relative
--   (file or directory) path to form a new path.
combine :: DirPath ar -> RelPath fd -> Path ar fd
combine = (</>)


-- | Remove last extension, and the \".\" preceding it.
--
-- >> dropExtension x == fst (splitExtension x)
dropExtension :: FilePath ar -> FilePath ar
dropExtension = fst . splitExtension

-- | Drop all extensions
--
-- >> not $ hasAnExtension (dropExtensions x)
dropExtensions :: FilePath ar -> FilePath ar
dropExtensions = fst . splitExtensions

dropFileName :: Path ar fd -> DirPath ar
dropFileName = fst . splitFileName


-- | Set the extension of a file, overwriting one if already present.
--
-- >> replaceExtension "file.txt" ".bob" == "file.bob"
-- >> replaceExtension "file.txt" "bob" == "file.bob"
-- >> replaceExtension "file" ".bob" == "file.bob"
-- >> replaceExtension "file.txt" "" == "file"
-- >> replaceExtension "file.fred.bob" "txt" == "file.fred.txt"
replaceExtension :: FilePath ar -> String -> FilePath ar
replaceExtension p ext = dropExtension p <.> ext

replaceBaseName :: Path ar fd -> String -> Path ar fd
replaceBaseName p bn = takeDirectory p </> (asPath bn `genericAddExtension` genericTakeExtension p)

replaceDirectory :: Path ar1 fd -> DirPath ar2 -> Path ar2 fd
replaceDirectory p d = d </> takeFileName p

replaceFileName :: Path ar fd -> String -> Path ar fd
replaceFileName p fn = takeDirectory p </> asPath fn


-- | Split on the extension. 'addExtension' is the inverse.
--
-- >> uncurry (<.>) (splitExtension x) == x
-- >> uncurry addExtension (splitExtension x) == x
-- >> splitExtension "file.txt" == ("file",".txt")
-- >> splitExtension "file" == ("file","")
-- >> splitExtension "file/file.txt" == ("file/file",".txt")
-- >> splitExtension "file.txt/boris" == ("file.txt/boris","")
-- >> splitExtension "file.txt/boris.ext" == ("file.txt/boris",".ext")
-- >> splitExtension "file/path.txt.bob.fred" == ("file/path.txt.bob",".fred")
splitExtension :: FilePath ar -> (FilePath ar, String)
splitExtension = genericSplitExtension

-- | Split on all extensions
--
-- >> splitExtensions "file.tar.gz" == ("file",".tar.gz")
splitExtensions :: FilePath ar -> (FilePath ar, String)
splitExtensions = genericSplitExtensions

prop_splitCombine :: AbsFile -> Property
prop_splitCombine p = property $ p == p2 <.> ext
                      where
                        (p2, ext) = splitExtension p

splitFileName :: Path ar fd -> (DirPath ar, RelPath fd)
splitFileName (FileDir p pc) = (p, mkPathFromComponents [pc])

prop_split_combine :: AbsFile -> Property
prop_split_combine p = property $ uncurry combine (splitFileName p) == p


-- | Get the basename of a file
--
-- >> takeBaseName "/tmp/somedir/myfile.txt" == "myfile"
-- >> takeBaseName "./myfile.txt" == "myfile"
-- >> takeBaseName "myfile.txt" == "myfile"
takeBaseName :: Path ar fd -> RelPath fd
takeBaseName = takeFileName . genericDropExtension

takeDirectory :: Path ar fd -> DirPath ar
takeDirectory = fst . splitFileName

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- >> takeExtension x == snd (splitExtension x)
-- >> takeExtension (addExtension x "ext") == ".ext"
-- >> takeExtension (replaceExtension x "ext") == ".ext"
takeExtension :: FilePath ar -> String
takeExtension = snd . splitExtension

-- | Get all extensions
--
-- >> takeExtensions "file.tar.gz" == ".tar.gz"
takeExtensions :: FilePath ar -> String
takeExtensions = snd . splitExtensions

-- | Get the filename component of a file path (ie stripping all parent dirs)
--
-- >> takeFileName "/tmp/somedir/myfile.txt" == "myfile.txt"
-- >> takeFileName "./myfile.txt" == "myfile.txt"
-- >> takeFileName "myfile.txt" == "myfile.txt"
takeFileName :: Path ar fd -> RelPath fd
takeFileName PathRoot = PathRoot -- becomes a relative root
takeFileName (FileDir _ pc) = FileDir PathRoot pc

prop_takeFileName_end :: AbsFile -> Property
prop_takeFileName_end p = property $ show (takeFileName p) `isSuffixOf` show p


------------------------------------------------------------------------
-- Auxillary Manipulation Functions

-- | Check whether two strings are equal as file paths.
--
-- >> equalFilePath "/tmp/" "/tmp" == True
-- >> equalFilePath "/tmp"  "tmp"  == False
equalFilePath :: String -> String -> Bool
equalFilePath s1 s2 | a1 <- isAbsoluteString s1,
                      a2 <- isAbsoluteString s2 = a1 == a2 && asPath s1 == asPath s2

-- | Constructs a 'Path' from a list of components.
--
-- >> joinPath ["/tmp","someDir","file.txt"] == "/tmp/someDir/file.txt"
-- >> (joinPath ["/tmp","someDir","file.txt"] :: RelFile) == "tmp/someDir/file.txt"
joinPath :: [String] -> Path ar fd
joinPath = asPath . intercalate [pathSeparator]

-- | Currently just transforms:
--
-- >> normalise "/tmp/fred/./jim/./file" == "/tmp/fred/jim/file"
normalise :: Path ar fd -> Path ar fd
normalise = mkPathFromComponents . filter (/=(PathComponent ".")) . pathComponents

-- | Deconstructs a path into its components.
--
-- >> splitPath ("/tmp/someDir/myfile.txt" :: AbsDir)  == (["tmp","someDir","myfile.txt"],Nothing)
-- >> splitPath ("/tmp/someDir/myfile.txt" :: AbsFile) == (["tmp","someDir"],Just "myfile.txt")
-- >> splitPath (asAbsFile "/tmp/someDir/myfile.txt")  == (["tmp","someDir"],Just "myfile.txt")
splitPath :: FileDirClass fd => Path ar fd -> ([RelDir],Maybe RelFile)
splitPath PathRoot = ([],Nothing)
splitPath p@(FileDir d pc) =
    fileDir (\_->(map (FileDir PathRoot) . pathComponents $ d,  Just (FileDir PathRoot pc)))
            (\_->(map (FileDir PathRoot) . pathComponents $ p,  Nothing))
            p

-- | This function can be used to construct a relative path by removing
--   the supplied 'AbsDir' from the front. It is a runtime 'error' if the
--   supplied 'AbsPath' doesn't start with the 'AbsDir'.
--
-- >> makeRelative "/tmp/somedir" "/tmp/somedir/anotherdir/file.txt" == "anotherdir/file.txt"
makeRelative :: AbsDir -> AbsPath fd -> RelPath fd
makeRelative relTo orig = maybe err mkPathFromComponents $ stripPrefix relToPC origPC
  where
    err     = error $ printf "System.Path can't make %s relative to %s" (show origPC) (show relToPC)
    relToPC = pathComponents relTo
    origPC  = pathComponents orig

-- | Joins an absolute directory with a relative path to construct a
--   new absolute path.
--
-- >> makeAbsolute "/tmp" "file.txt"      == "/tmp/file.txt"
-- >> makeAbsolute "/tmp" "adir/file.txt" == "/tmp/adir/file.txt"
makeAbsolute :: AbsDir -> RelPath fd -> AbsPath fd
makeAbsolute = genericMakeAbsolute

-- | Converts a relative path into an absolute one by
--   prepending the current working directory.
makeAbsoluteFromCwd :: RelPath fd -> IO (AbsPath fd)
makeAbsoluteFromCwd = genericMakeAbsoluteFromCwd

-- | As for 'makeAbsolute', but for use when the path may already be
--   absolute (in which case it is left unchanged).
--
-- >> genericMakeAbsolute "/tmp" (asRelFile "file.txt")       == "/tmp/file.txt"
-- >> genericMakeAbsolute "/tmp" (asRelFile "adir/file.txt")  == "/tmp/adir/file.txt"
-- >> genericMakeAbsolute "/tmp" (asAbsFile "adir/file.txt")  == "/adir/file.txt"
-- >> genericMakeAbsolute "/tmp" (asAbsFile "/adir/file.txt") == "/adir/file.txt"
genericMakeAbsolute :: AbsRelClass ar => AbsDir -> Path ar fd -> AbsPath fd
genericMakeAbsolute base p = absRel id (base </>) p

-- | As for 'makeAbsoluteFromCwd', but for use when the path may already be
--   absolute (in which case it is left unchanged).
genericMakeAbsoluteFromCwd :: AbsRelClass ar => Path ar fd -> IO (AbsPath fd)
genericMakeAbsoluteFromCwd p = do
  cwdString <- SD.getCurrentDirectory -- we don't use System.Path.Directory impl here to avoid module cycle
  return $ genericMakeAbsolute (asAbsDir cwdString) p

prop_makeAbsoluteFromDir_endSame :: AbsDir -> RelFile -> Property
prop_makeAbsoluteFromDir_endSame base p = property $ show p `isSuffixOf` show (makeAbsolute base p)

prop_makeAbsoluteFromDir_startSame :: AbsDir -> RelFile -> Property
prop_makeAbsoluteFromDir_startSame base p = property $ show base `isPrefixOf` show (makeAbsolute base p)

-- prop_makeAbsoluteFromDir_startSameAbs :: AbsDir -> AbsFile -> Property
-- prop_makeAbsoluteFromDir_startSameAbs base p = property $ show base `isPrefixOf` show (makeAbsolute base p)


------------------------------------------------------------------------
-- NYI - Not Yet Implemented

{-
splitSearchPath  :: String   -> [String]
getSearchPath    :: IO [String]
splitDrive       :: String   -> (String, String)
joinDrive        :: String   -> String -> String
takeDrive        :: String   -> String
hasDrive         :: String   -> Bool
dropDrive        :: String   -> String
isDrive          :: String   -> Bool
isValid          :: String   -> Bool
makeValid        :: String   -> String
-}


------------------------------------------------------------------------
-- Path Predicates

-- | Test whether a @'Path' ar fd@ is absolute.
--
-- >> isAbsolute (asAbsFile "fred")  == True
-- >> isAbsolute (asRelFile "fred")  == False
-- >> isAbsolute (asAbsFile "/fred") == True
-- >> isAbsolute (asRelFile "/fred") == False
isAbsolute :: AbsRelClass ar => Path ar fd -> Bool
isAbsolute = absRel (const True) (const False)

-- | Test whether the 'String' would correspond to an absolute path
--   if interpreted as a 'Path'.
isAbsoluteString :: String -> Bool
isAbsoluteString [] = False -- Treat the empty string as relative because it doesn't start with 'pathSeparators'
isAbsoluteString (x:_) = any (== x) pathSeparators -- Absolute if first char is a path separator

-- | Invariant - this should return True iff arg is of type @'Path' Rel _@
--
-- > isRelative = not . isAbsolute
isRelative :: AbsRelClass ar => Path ar fd -> Bool
isRelative = not . isAbsolute

-- | Test whether the 'String' would correspond to a relative path
--   if interpreted as a 'Path'.
--
-- > isRelativeString = not . isAbsoluteString
isRelativeString :: String -> Bool
isRelativeString = not . isAbsoluteString


-- | Does the given filename have an extension?
--
-- >> null (takeExtension x) == not (hasAnExtension x)
hasAnExtension :: FilePath ar -> Bool
hasAnExtension = not . null . snd . splitExtension

-- | Does the given filename have the given extension?
--
-- >> hasExtension ".hs" "MyCode.hs" == True
-- >> hasExtension ".hs" "MyCode.hs.bak" == False
-- >> hasExtension ".hs" "MyCode.bak.hs" == True
hasExtension :: String -> FilePath ar -> Bool
hasExtension ext = (==ext) . snd . splitExtension


------------------------------------------------------------------------
-- Separators

-- | This is largely for 'System.FilePath' compatability
addTrailingPathSeparator :: String -> String
addTrailingPathSeparator = (++[pathSeparator])

-- | This is largely for 'System.FilePath' compatability
dropTrailingPathSeparator :: String -> String
dropTrailingPathSeparator = init

-- | File extension character
--
-- >> extSeparator == '.'
extSeparator :: Char
extSeparator = '.'

-- | This is largely for 'System.FilePath' compatability
hasTrailingPathSeparator :: String -> Bool
hasTrailingPathSeparator = isPathSeparator . last

-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- >> isPathSeparator pathSeparator
pathSeparator :: Char
pathSeparator | isWindows = '\\'
              | otherwise = '/'

-- | The list of all possible separators.
--
-- >> pathSeparator `elem` pathSeparators
pathSeparators :: [Char]
pathSeparators = return pathSeparator

-- | The character that is used to separate the entries in the $PATH environment variable.
--
searchPathSeparator :: Char
searchPathSeparator = ':'

-- | Is the character an extension character?
--
-- >> isExtSeparator a == (a == extSeparator)
isExtSeparator :: Char -> Bool
isExtSeparator = (== extSeparator)

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- >> isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: Char -> Bool
isPathSeparator = (== pathSeparator)

-- | Is the character a file separator?
--
-- >> isSearchPathSeparator a == (a == searchPathSeparator)
isSearchPathSeparator :: Char -> Bool
isSearchPathSeparator = (== searchPathSeparator)


------------------------------------------------------------------------
-- Generic Manipulation Functions

-- These functions support manipulation of extensions on directories
-- as well as files. They have looser types than the corresponding
-- 'Basic Manipulation Functions', but it is expected that the basic
-- functions will be used more frequently as they provide more checks.

-- | This is a more flexible variant of 'addExtension' / '<.>' which can
--   work with files or directories
--
-- >> genericAddExtension "/" "x" == "/.x"
genericAddExtension :: Path ar fd -> String -> Path ar fd
genericAddExtension p "" = p
genericAddExtension (FileDir p (PathComponent pc)) ext = FileDir p (PathComponent (pc ++ suffix))
                                         where suffix | "." `isPrefixOf` ext = ext
                                                      | otherwise = "." ++ ext
genericAddExtension PathRoot ext = FileDir PathRoot (PathComponent suffix)
                                         where suffix | "." `isPrefixOf` ext = ext
                                                      | otherwise = "." ++ ext

genericDropExtension :: Path ar fd -> Path ar fd
genericDropExtension = fst . genericSplitExtension

genericDropExtensions :: Path ar fd -> Path ar fd
genericDropExtensions = fst . genericSplitExtensions

genericSplitExtension :: Path ar fd -> (Path ar fd, String)
genericSplitExtension (FileDir p (PathComponent s)) = (FileDir p (PathComponent s1), s2)
    where (s1,s2) = fixTrailingDot $ rbreak isExtSeparator s
          fixTrailingDot ("",r2) = (r2,"")
          fixTrailingDot (r1,r2) | [extSeparator] `isSuffixOf` r1 = (init r1, extSeparator:r2)
                                 | otherwise = (r1,r2)
          swap (x,y) = (y,x)
          rbreak p = (reverse *** reverse) . swap . break p . reverse
genericSplitExtension p = (p,"")

genericSplitExtensions :: Path ar fd -> (Path ar fd, String)
genericSplitExtensions (FileDir p (PathComponent s)) = (FileDir p (PathComponent s1), s2)
    where (s1,s2) = break isExtSeparator s
genericSplitExtensions p = (p,"")

genericTakeExtension :: Path ar fd -> String
genericTakeExtension = snd . genericSplitExtension

genericTakeExtensions :: Path ar fd -> String
genericTakeExtensions = snd . genericSplitExtension


------------------------------------------------------------------------
-- QuickCheck

testall = do
  putStrLn "Running QuickCheck tests..."
  quickCheck prop_mkPathFromComponents_pathComponents
  quickCheck prop_makeAbsoluteFromDir_endSame
  quickCheck prop_makeAbsoluteFromDir_startSame
  quickCheck prop_split_combine
  quickCheck prop_takeFileName_end
  quickCheck prop_splitCombine
  putStrLn "Tests completed."

-- test :: Testable a => a -> IO ()
-- test = quickCheck

qcFileComponent :: Gen PathComponent
qcFileComponent = PathComponent <$> frequency [
                    (1, return "someFile"),
                    (1, return "fileWith.ext"),
                    (1, return "file.with.multiple.exts"),
                    (1, return "file with spcs")
                  ]

qcDirComponent :: Gen PathComponent
qcDirComponent = PathComponent <$> frequency [
                    (1, return "someDir"),
                    (1, return "aDir"),
                    (1, return "aFolder"),
                    (1, return "a folder"),
                    (1, return "directory")
                  ]

qcFilePath :: Gen (FilePath ar)
qcFilePath = do
  (NonNegative numDirs) <- arbitrarySizedIntegral
  pcs <- vectorOf numDirs qcDirComponent
  pc <- qcFileComponent
  return $ mkPathFromComponents (pcs ++ [pc])


qcDirPath :: Gen (DirPath ar)
qcDirPath = do
  (NonNegative numDirs) <- arbitrarySizedIntegral
  pcs <- vectorOf numDirs qcDirComponent
  pc <- qcDirComponent
  return $ mkPathFromComponents (pcs ++ [pc])

-- qcPath :: (AbsRelClass ar, FileDirClass fd) => Gen (Path ar fd)
-- qcPath = absRel

instance Arbitrary PathComponent where
    arbitrary = oneof [qcFileComponent, qcDirComponent]

instance Arbitrary (Path ar File) where
    arbitrary = qcFilePath

instance Arbitrary (Path ar Dir)  where
    arbitrary = qcDirPath


