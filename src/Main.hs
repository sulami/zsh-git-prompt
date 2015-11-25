module Main where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))
import System.IO.Unsafe (unsafeInterleaveIO)

import Utils (stringsFromStatus, Hash(MkHash))
import Data.Maybe (fromMaybe)

{- Git commands -}

successOrNothing :: (ExitCode, a, b) -> Maybe a
successOrNothing (exitCode, output, _)
  | exitCode == ExitSuccess = Just output
  | otherwise               = Nothing

safeRun :: String -> [String] -> IO (Maybe String)
safeRun command arguments = successOrNothing
                            <$> readProcessWithExitCode command arguments ""

gitstatus :: IO (Maybe String)
gitstatus = safeRun "git" ["status", "--porcelain", "--branch"]

gitrevparse :: IO (Maybe Hash)
gitrevparse = do
  result <- safeRun "git" ["rev-parse", "--short", "HEAD"]
  return $ MkHash . init <$> result

{- main -}

main :: IO ()
main = do
  mstatus <- gitstatus
  mhash <- unsafeInterleaveIO gitrevparse -- defer the execution until we know we need the hash
  let result = do status <- mstatus
                  strings <- stringsFromStatus mhash status
                  return $ unwords strings
  putStr $ fromMaybe "" result
