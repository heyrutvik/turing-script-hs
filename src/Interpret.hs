module Interpret where

import Core
import Control.Monad.State.Lazy
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List.Index

data ISymbol = IString String | IBlank | IOnTheFly deriving Show
data Operation = PR ISymbol | PL ISymbol | PN ISymbol deriving Show

data Config = Config String String deriving (Show, Eq, Ord) -- m-config and symbol
data Behaviour = Behaviour Operation String deriving Show -- operations and f-config
type Context = Map Config Behaviour -- program's config/behaviour context

data CompleteConfig = CompleteConfig
  { tape :: [String]   -- symbol tape
  , head :: Int        -- tape index
  , config :: String } -- m-config
    deriving Show

run :: Term -> Int -> [String]
run t steps = evalState (interpreter steps cx) (initialCompleteConfig cx)
  where
    cx :: Context
    cx = context t

interpreter :: Int -> Context -> State CompleteConfig [String]
interpreter steps cx = sequence_ (modify <$> (\_ -> update cx) <$> [0..steps]) >> gets tape

update :: Context -> CompleteConfig -> CompleteConfig
update cx CompleteConfig { tape = t, Interpret.head = h, config = mc} = let
    sym = ((if length t == h then insertAt h " " t else t) !! h)
  in
    case Map.lookup (Config mc sym) cx of
    (Just (Behaviour (PR is) fc)) -> CompleteConfig { tape = insertOrUpdate h t h (isymbolToString sym is) t, Interpret.head = h + 1, config = fc }
    (Just (Behaviour (PL is) fc)) -> CompleteConfig { tape = insertOrUpdate h t h (isymbolToString sym is) t, Interpret.head = h - 1, config = fc }
    (Just (Behaviour (PN is) fc)) -> CompleteConfig { tape = setAt h (isymbolToString sym is) t, Interpret.head = h, config = fc }
    _ -> error "machine stuck"

initialCompleteConfig :: Context -> CompleteConfig
initialCompleteConfig cx = CompleteConfig { tape = [sym], Interpret.head = 0, config = mc }
  where
    Config mc sym = (fst . Prelude.head . Map.toList) cx

insertOrUpdate :: Int -> [a] -> (Int -> a -> [a] -> [a])
insertOrUpdate i as = if length as == i then insertAt else setAt

-- TODO improve `context` implementation
context :: Term -> Context
context t = context' t Map.empty
  where
    context' :: Term -> Context -> Context
    context' t acc = case t of
      Rule (Ident mc) (Symbol a) [Exec (Effect (Print (Symbol (String sym)))), Exec (Move R)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PR $ IString sym) fc) acc
      Rule (Ident mc) (Symbol a) [Exec (Effect (Print (Symbol (String sym)))), Exec (Move L)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PL $ IString sym) fc) acc
      Rule (Ident mc) (Symbol a) [Exec (Effect (Print (Symbol (String sym)))), Exec (Move N)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PN $ IString sym) fc) acc
      Rule (Ident mc) (Symbol a) [Exec (Effect (Print (Symbol Blank))), Exec (Move R)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PR IBlank) fc) acc
      Rule (Ident mc) (Symbol a) [Exec (Effect (Print (Symbol Blank))), Exec (Move L)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PL IBlank) fc) acc
      Rule (Ident mc) (Symbol a) [Exec (Effect (Print (Symbol Blank))), Exec (Move N)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PN IBlank) fc) acc
      Rule (Ident mc) (Symbol a) [Exec (Effect Erase), Exec (Move R)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PR IBlank) fc) acc
      Rule (Ident mc) (Symbol a) [Exec (Effect Erase), Exec (Move L)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PL IBlank) fc) acc
      Rule (Ident mc) (Symbol a) [Exec (Effect Erase), Exec (Move N)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PN IBlank) fc) acc
      Rule (Ident mc) (Symbol a) [Exec OnTheFly, Exec (Move R)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PR IOnTheFly) fc) acc
      Rule (Ident mc) (Symbol a) [Exec OnTheFly, Exec (Move L)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PL IOnTheFly) fc) acc
      Rule (Ident mc) (Symbol a) [Exec OnTheFly, Exec (Move N)] (Ident fc) -> Map.insert (Config mc (atomToString a)) (Behaviour (PN IOnTheFly) fc) acc
      Seq f s -> Map.union (context' f acc) (context' s acc)
      Table rs -> context' rs acc
      Machine n ta -> context' ta acc
      _ -> Map.empty

atomToString :: Atom -> String
atomToString (String s) = s
atomToString _ = " "

isymbolToString :: String -> ISymbol -> String
isymbolToString _ (IString s) = s
isymbolToString _ IBlank = " "
isymbolToString z IOnTheFly = z
