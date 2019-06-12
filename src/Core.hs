module Core (
  Term(..),
  Atom(..),
  Step(..),
  Kind(..),
  Direction(..)
) where

data Direction = R | L | N deriving Show -- right | left | none

data Kind = Print Term | Erase deriving Show

data Step = Effect Kind
          | Move Direction
          | OnTheFly -- read from tape at run time
          deriving Show

data Atom = String String -- (exist till final elaboration)
          | Blank -- blank square (exist till final elaboration)
          | NotBlank -- not-blank square
          | Elaborated -- place holder for expanded rule in elaborate phase
          deriving (Show, Eq, Ord)

data Term = Ident String
          | Symbol Atom
          | Exec Step
          | Rule Term Term [Term] Term -- m-config, symbol, exec, f-config
          | Seq Term Term -- rule, seq
          | Table Term -- sequence of rules
          | Machine Term Term -- name, table
          deriving Show