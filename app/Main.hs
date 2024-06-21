module Main where

main :: IO ()
main = do
  putStr "placeholder"

-- main :: IO ()
-- main = do
--   putStrLn "Welcome, adventurer, to the Rusty Scabbard Inn. What's your name?"
--   name <- getLine
--   putStrLn ("Hello, " ++ name ++ ", what would you like to do this evening?")
--   putStrLn possibleActionsLn
--   choice <- getLine
--   putStrLn ("Thank you for your input! You chose " ++ choice ++ ". Good night.")

-- possibleActionsLn :: String
-- possibleActionsLn = unlines $ addNumbering possibleActions

-- addNumbering :: [String] -> [String]
-- addNumbering = zipWith (\n str -> show (n :: Int) ++ ". " ++ str) [1..]

-- possibleActions :: [String]
-- possibleActions = ["Eat", "Drink", "Sleep", "Brawl", "Gamble", "Beg"]
