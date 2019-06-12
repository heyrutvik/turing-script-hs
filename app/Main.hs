module Main where

import System.Environment
import Control.Monad
import Parse
import Elaborate
import Helper.Combinators
import Helper.SExpr

main :: IO ()
main = do
  args <- getArgs
  when (not $ null args) $ do
    prog <- readFile (head args)
    case parser prog of
      (Left e) -> print e
      (Right t) -> print (sexpr t) >> print (sexpr (elaborator t))