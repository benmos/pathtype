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
 <TESTS_GO_HERE>
 True -- Just so we can have commas at end of all preceding lines
 ]

