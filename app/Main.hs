module Main where

import System.Environment
import Control.Monad
import Parse
import Elaborate
import Interpret
import Helper.Combinators
import Helper.SExpr
import Data.List

main :: IO ()
main = do
  args <- getArgs
  when (length args == 2) $ do
    prog <- readFile (Prelude.head args)
    case parser prog of
      (Left e) -> print e
      (Right t) -> putStrLn ("tape: |" ++ intercalate "|" tape ++ " ...")
        where
          tape = run (elaborator t) (read (args !! 1))