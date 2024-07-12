module Main where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Data.Foldable (for_)
import Flow

import Digits.Read
import Digits.Parse
import PolicyNumbers.Validate
import PolicyNumbers.Analyze
import PolicyNumber
import SevenSegmentDigit

main :: IO ()
main = do
  operation <- getOpChoice opNames "for an option below"
  doOp operation

opNames :: [String]
opNames = ["parse", "validate", "analyze", "analyze and correct"]

doOp :: String -> IO ()
doOp "parse" = do
  doFileOp "parse" "These are the policy numbers in" (parse .> show)
doOp "validate" = do
  putStrLn <| "\nPlease enter a policy number to validate:\n"
  pn <- getLine
  if pn `elem` ["e", "exit", "q", "quit"]
    then exitSuccess
  else do
    let result = validate (fromString pn)
    putStrLn <| "\n" ++ reportResult result ++ "\n"
      where reportResult True = "Hooray, that's a VALID policy number 🎉"
            reportResult False = "Nope, that's an INVALID policy number (ノಠ益ಠ)ノ彡┻━┻"
doOp "analyze" = do
  doFileOp "analyze" "Here is an analysis of" (parse .> analyze)
doOp "analyze and correct" = do
  doFileOp "analyze and correct" "Here is an analysis and correction of" analyzeAndCorrect
doOp _ = do
  error "Bad input was accepted. Check `opNames` in Main.hs."

doFileOp :: String -> String -> ([SevenSegmentDigit] -> String) -> IO ()
doFileOp opName opDescription action = do
  (fileName, fileContents) <- getFileChoice opName
  putStrLn <| "\n" ++ opDescription ++ " " ++ fileName ++ ":\n"
  let results = readDigits fileContents |> map action
  putStrLn <| intercalate "\n" results ++ "\n"

getFileChoice :: String -> IO (String, String)
getFileChoice opName = do
  ocrFileNames <- listDirectory "fixtures/ocr"
  fileName <- getOpChoice ocrFileNames ("to " ++ opName ++ " the corresponding sample file")
  fileContents <- readFile ("fixtures/ocr" </> fileName)
  return (fileName, fileContents)

getOpChoice :: [String] -> String -> IO String
getOpChoice options description = do
  putStrLn <| "\nPlease enter a number " ++ description ++ ":\n"
  for_ (zip [(1::Int)..] options) <| \(n, fileName) -> do
    putStrLn <| show n ++ ". " ++ fileName
  putStr "\n> "
  hFlush stdout
  choice <- getLine

  if choice `elem` ["e", "exit", "q", "quit"]
    then exitSuccess
  else if read choice `elem` [1 .. length options]
    then do
      return (options !! (read choice - 1))
  else do
    putStrLn "\nOops, that's not an option."
    getOpChoice options description

