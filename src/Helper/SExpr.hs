module Helper.SExpr where

import Core
import Helper.Combinators
import Control.Monad

-- to s-expression
class SExpr a where
  sexpr :: a -> String

instance SExpr Kind where
  sexpr (Print s) = "P" ++ sexpr s
  sexpr Erase = "E"

instance SExpr Step where
  sexpr (Effect k) = sexpr k
  sexpr (Move d) = show d
  sexpr OnTheFly = "<@>"

instance SExpr Atom where
  sexpr (String s) = s
  sexpr Blank = "none"
  sexpr NotBlank = "any"
  sexpr Elaborated = "$$"

instance SExpr Term where
  sexpr (Ident s) = s
  sexpr (Symbol a) = sexpr a
  sexpr (Exec s) = sexpr s
  sexpr (Rule m s ss f) = "(" ++ sexpr m ++ " " ++ sexpr s ++ " [" ++ join (fmap sexpr ss) ++ "] " ++ sexpr f ++ ")"
  sexpr (Seq fst snd) = sexpr fst ++ " " ++ sexpr snd
  sexpr (Table s) = "(table " ++ sexpr s ++ ")"
  sexpr (Machine n i) = "(machine " ++ sexpr n ++ " " ++ sexpr i ++ ")"