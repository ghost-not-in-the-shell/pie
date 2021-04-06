{-# LANGUAGE LambdaCase #-}
module Main where
import System.Environment
import System.Exit
import Text.ParserCombinators.Parsec
import Parser
-- import Elab

main ∷ IO ()
main = undefined
  {-
  do
  getArgs >>= \case
    ["type"] → do
      file ← getContents
      raw  ← case parse prog "<stdin>" file of
        Left e → do
          putStrLn $ show e
          exitSuccess
        Right t → return t
      case runElab₀ (infer raw) of
        Left e → putStrLn e
        Right _ → putStrLn "Success"
    _ → putStrLn "Hello World!"
-}
