module Test where

import Control.Applicative
import Control.Monad
import Data.List

import System.Path
import System.Process
import System.Exit
import Text.Printf

srcfiles   = ["System/Path/Internal.hs","System/Path/Directory.hs","System/Path/IO.hs"]
template   = "TestTemplate.hs"
testModule = "TestModule.hs"
tok        = "<TESTS_GO_HERE>"
testPrefix = "-- >> "

main = do
  sourceLines   <- concat <$> mapM (fmap lines . readFile) srcfiles
  templateLines <- lines <$> readFile template
  let testLines = [drop (length testPrefix) l | l <- sourceLines, testPrefix `isPrefixOf` l]
      (templateHead,_:templateTail) = break (tok `isInfixOf`) templateLines
      outLines = (\t -> "  "++t++",") <$> testLines
      numTestLines = zip [1..] testLines

  writeFile testModule $ unlines $ templateHead ++ outLines ++ templateTail

  let args = ["-optP-include", "-optPdist/build/autogen/cabal_macros.h", "-e","TestModule.main",testModule]
      ghc = "ghc"
      stdinput = ""

  printf "Running %d tests...\n" (length testLines)
  x@(_ec, failedTestsStr, err) <- readProcessWithExitCode ghc args stdinput
  when (not $ null err) $ putStrLn err >> exitFailure

  let failedTests :: [Int]
      failedTests = read failedTestsStr
      numFailures = length failedTests

  when (not $ null failedTests) $ do
    putStrLn "Failures:"
    putStrLn $ unlines [s | (n,s) <- numTestLines, n `elem` failedTests]
    exitFailure

  putStrLn "Passed."

