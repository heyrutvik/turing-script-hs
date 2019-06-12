module Helper.Combinators where

import Core
import Control.Monad
import Data.Set as Set
import qualified Data.Set as Set
import Data.String.Utils
import Data.Map (Map)
import qualified Data.Map as Map

mkSeq :: [Term] -> Term
mkSeq [t] = t
mkSeq (h : t) = Seq h (mkSeq t)

flatSeq :: Term -> Term -> Term
flatSeq s1 s2 = case s1 of
  Rule _ _ _ _ -> Seq s1 s2
  Seq f s -> Seq f (flatSeq s s2)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = Prelude.take n l : group n (Prelude.drop n l)
  | otherwise = error "Negative or zero n"

symbols :: Term -> [Atom]
symbols t = Set.toList (symbols' t Set.empty)
  where
    symbols' :: Term -> Set Atom -> Set Atom
    symbols' t acc = case t of
      Symbol a@(String s) -> Set.insert a acc -- only `String s :: Atom`
      Rule _ s _ _ -> symbols' s acc
      Seq r s -> Set.union (symbols' r acc) (symbols' s acc)
      Table rs -> symbols' rs acc
      Machine n ta -> symbols' ta acc
      _ -> Set.empty

configs :: Term -> [String]
configs t = Set.toList (configs' t Set.empty)
  where
    configs' :: Term -> Set String -> Set String
    configs' t acc = case t of
      Ident s -> Set.insert s acc
      Rule (Ident mc) _ _ (Ident fc) -> Set.union (Set.fromList [mc, fc]) acc
      Seq r s -> Set.union (configs' r acc) (configs' s acc)
      Table rs -> configs' rs acc
      Machine n ta -> configs' ta acc
      _ -> Set.empty

freshName :: [String] -> String -> ([String], String)
freshName dict s = if s `elem` dict then freshName dict (s ++ "*") else (s : dict, s)

lastName :: [String] -> String -> String
lastName dict s = head $ Prelude.filter (startswith s) dict