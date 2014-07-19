{-# LANGUAGE OverloadedStrings #-}
module TestModule ()
where

import System.Path
import System.Path.Directory
import System.Random
import Data.Char

main = do
  g <- newStdGen
  let (a,_g') = random g
      -- TODO - integrate with QuickCheck
      x = rootDir </> "tmp" </> "someFile" <.> "ext"

  let numResults = zip [1..] $ results a x
      fails = [n|(n,False) <-numResults]

  putStr $ show fails -- This can then be parsed by the invoking Test module

results a x = [
  pathMap (map toLower) "/tmp/Reports/SpreadSheets" == "/tmp/reports/spreadsheets",
  asPath "/tmp" == "/tmp",
  asPath "file.txt" == "file.txt",
  isAbsolute (asPath "/tmp" :: AbsDir) == True,
  isAbsolute (asPath "/tmp" :: RelDir) == False,
  getPathString (asPath "/tmp" :: AbsDir) == "/tmp",
  getPathString (asPath "/tmp" :: RelDir) == "tmp",
  getPathString (asRelFile "file.txt") == "file.txt",
  getPathString (asRelFile "/file.txt") == "file.txt",
  getPathString (asRelFile "tmp") == "tmp",
  getPathString (asRelFile "/tmp") == "tmp",
  getPathString (asRelDir "file.txt") == "file.txt",
  getPathString (asRelDir "/file.txt") == "file.txt",
  getPathString (asRelDir "tmp") == "tmp",
  getPathString (asRelDir "/tmp") == "tmp",
  getPathString (asAbsFile "file.txt") == "/file.txt",
  getPathString (asAbsFile "/file.txt") == "/file.txt",
  getPathString (asAbsFile "tmp") == "/tmp",
  getPathString (asAbsFile "/tmp") == "/tmp",
  getPathString (asAbsDir "file.txt") == "/file.txt",
  getPathString (asAbsDir "/file.txt") == "/file.txt",
  getPathString (asAbsDir "tmp") == "/tmp",
  getPathString (asAbsDir "/tmp") == "/tmp",
  either id (const "fred") (mkPathAbsOrRel "/tmp") == "/tmp",
  either id (const "fred") (mkPathAbsOrRel  "tmp") == "fred",
  mkAbsPath "/tmp" "foo.txt" == "/tmp/foo.txt",
  mkAbsPath "/tmp" "/etc/foo.txt" == "/etc/foo.txt",
  addExtension "file.txt" "bib" == "file.txt.bib",
  addExtension "file." ".bib" == "file..bib",
  addExtension "file" ".bib" == "file.bib",
  takeFileName (addExtension "" "ext") == ".ext",
  dropExtension x == fst (splitExtension x),
  not $ hasAnExtension (dropExtensions x),
  replaceExtension "file.txt" ".bob" == "file.bob",
  replaceExtension "file.txt" "bob" == "file.bob",
  replaceExtension "file" ".bob" == "file.bob",
  replaceExtension "file.txt" "" == "file",
  replaceExtension "file.fred.bob" "txt" == "file.fred.txt",
  uncurry (<.>) (splitExtension x) == x,
  uncurry addExtension (splitExtension x) == x,
  splitExtension "file.txt" == ("file",".txt"),
  splitExtension "file" == ("file",""),
  splitExtension "file/file.txt" == ("file/file",".txt"),
  splitExtension "file.txt/boris" == ("file.txt/boris",""),
  splitExtension "file.txt/boris.ext" == ("file.txt/boris",".ext"),
  splitExtension "file/path.txt.bob.fred" == ("file/path.txt.bob",".fred"),
  splitExtensions "file.tar.gz" == ("file",".tar.gz"),
  takeBaseName "/tmp/somedir/myfile.txt" == "myfile",
  takeBaseName "./myfile.txt" == "myfile",
  takeBaseName "myfile.txt" == "myfile",
  takeExtension x == snd (splitExtension x),
  takeExtension (addExtension x "ext") == ".ext",
  takeExtension (replaceExtension x "ext") == ".ext",
  takeExtensions "file.tar.gz" == ".tar.gz",
  takeFileName "/tmp/somedir/myfile.txt" == "myfile.txt",
  takeFileName "./myfile.txt" == "myfile.txt",
  takeFileName "myfile.txt" == "myfile.txt",
  equalFilePath "/tmp/" "/tmp" == True,
  equalFilePath "/tmp"  "tmp"  == False,
  joinPath ["/tmp","someDir","file.txt"] == "/tmp/someDir/file.txt",
  (joinPath ["/tmp","someDir","file.txt"] :: RelFile) == "tmp/someDir/file.txt",
  normalise "/tmp/fred/./jim/./file" == "/tmp/fred/jim/file",
  splitPath ("/tmp/someDir/myfile.txt" :: AbsDir)  == (["tmp","someDir","myfile.txt"],Nothing),
  splitPath ("/tmp/someDir/myfile.txt" :: AbsFile) == (["tmp","someDir"],Just "myfile.txt"),
  splitPath (asAbsFile "/tmp/someDir/myfile.txt")  == (["tmp","someDir"],Just "myfile.txt"),
  makeRelative "/tmp/somedir" "/tmp/somedir/anotherdir/file.txt" == "anotherdir/file.txt",
  makeAbsolute "/tmp" "file.txt"      == "/tmp/file.txt",
  makeAbsolute "/tmp" "adir/file.txt" == "/tmp/adir/file.txt",
  genericMakeAbsolute "/tmp" (asRelFile "file.txt")       == "/tmp/file.txt",
  genericMakeAbsolute "/tmp" (asRelFile "adir/file.txt")  == "/tmp/adir/file.txt",
  genericMakeAbsolute "/tmp" (asAbsFile "adir/file.txt")  == "/adir/file.txt",
  genericMakeAbsolute "/tmp" (asAbsFile "/adir/file.txt") == "/adir/file.txt",
  isAbsolute (asAbsFile "fred")  == True,
  isAbsolute (asRelFile "fred")  == False,
  isAbsolute (asAbsFile "/fred") == True,
  isAbsolute (asRelFile "/fred") == False,
  null (takeExtension x) == not (hasAnExtension x),
  hasExtension ".hs" "MyCode.hs" == True,
  hasExtension ".hs" "MyCode.hs.bak" == False,
  hasExtension ".hs" "MyCode.bak.hs" == True,
  extSeparator == '.',
  isPathSeparator pathSeparator,
  pathSeparator `elem` pathSeparators,
  isExtSeparator a == (a == extSeparator),
  isPathSeparator a == (a `elem` pathSeparators),
  isSearchPathSeparator a == (a == searchPathSeparator),
  genericAddExtension "/" "x" == "/.x",
 True -- Just so we can have commas at end of all preceding lines
 ]

