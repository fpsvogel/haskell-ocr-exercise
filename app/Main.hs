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

main :: IO ()
main = do
  operation <- getOperationChoice operationNames "for an option below"
  doOperation operation

operationNames :: [String]
operationNames = ["parse", "validate"]

doOperation :: String -> IO ()
doOperation "parse" = do
  (fileName, fileContents) <- getFileChoice "parse"
  putStrLn <| "\nThese are the policy numbers in " ++ fileName ++ ":\n"
  let results = map parseDigits <| readDigits fileContents
  putStrLn <| intercalate "\n" results ++ "\n"
doOperation "validate" = do
  putStrLn <| "\nPlease enter a policy number to validate:\n"
  pn <- getLine
  let result = validatePolicyNumber pn
  putStrLn <| "\n" ++ reportResult result ++ "\n"
    where reportResult True = "Hooray, that's a valid policy number 🎉"
          reportResult False = "Oops, that's an invalid policy number (ノಠ益ಠ)ノ彡┻━┻"
doOperation _ = do
  error "Bad input was accepted. Check `operationNames` in Main.hs."

getFileChoice :: String -> IO (String, String)
getFileChoice operationName = do
  ocrFileNames <- listDirectory "fixtures/ocr"
  fileName <- getOperationChoice ocrFileNames ("to " ++ operationName ++ " the corresponding sample file")
  fileContents <- readFile ("fixtures/ocr" </> fileName)
  return (fileName, fileContents)

getOperationChoice :: [String] -> String -> IO String
getOperationChoice options description = do
  putStrLn <| "\nPlease enter a number " ++ description ++ ":\n"
  for_ (zip [(1::Int)..] options) <| \(n, fileName) -> do
    putStrLn <| show n ++ ". " ++ fileName
  putStr "\n> "
  hFlush stdout
  userChoice <- getLine

  if userChoice `elem` ["e", "exit", "q", "quit"]
    then exitSuccess
  else if read userChoice `elem` [1 .. length options]
    then do
      return (options !! (read userChoice - 1))
  else do
    putStrLn "\nOops, that's not an option."
    getOperationChoice options description

