module Elaborate (elaborator) where

import Core
import Helper.Combinators
import Control.Monad
import Data.List.Split
import Data.List.Index

elaborator :: Term -> Term
elaborator t = (expandRule symbolDict configDict . evenExec) t
  where
    symbolDict = symbols t
    configDict = configs t

evenExec :: Term -> Term
evenExec t = case t of
  (Rule mc sym es fc) -> Rule mc sym (expand es) fc
  (Seq fst snd) -> Seq (evenExec fst) (evenExec snd)
  (Table s) -> Table (evenExec s)
  (Machine n t) -> Machine n (evenExec t)
  where
    expand :: [Term] -> [Term]
    expand = join . reverse . group 2 . expand' []
    expand' :: [Term] -> [Term] -> [Term]
    expand' acc (Exec e1 : Exec e2 : rest) = case (e1, e2) of
      (Effect _, Effect _) -> expand' (Exec e2 : Exec (Move N) : acc) rest
      (Effect _, Move _) -> expand' (Exec e1 : Exec e2 : acc) rest
      (Move _, Effect _) -> expand' (Exec OnTheFly : Exec e1 : Exec e2 : Exec (Move N) : acc) rest
      (Move _, Move _) -> expand' (Exec OnTheFly : Exec e1 : Exec OnTheFly : Exec e2 : acc) rest
    expand' acc [Exec e1]= case e1 of
      Effect _ -> Exec e1 : Exec (Move N) : acc
      Move _ -> Exec OnTheFly : Exec e1 : acc
    expand' acc _ = acc

expandRule :: [Atom] -> [String] -> Term -> Term
expandRule symbolDict configDict = specialSymbol symbolDict . splitRule configDict

splitRule :: [String] -> Term -> Term
splitRule configDict t = snd $ splitRule' configDict t
  where
    splitRule' :: [String] -> Term -> ([String], Term)
    splitRule' configDict t = case t of
      Machine n is -> let (cd1, t1) = splitRule' configDict is in (cd1, Machine n t1)
      Table s -> let (cd1, t1) = splitRule' configDict s in (cd1, Table t1)
      Seq r s ->
        let
          (cd1, t1) = splitRule' configDict r
          (cd2, t2) = splitRule' cd1 s
        in
          (cd2, flatSeq t1 t2)
      Rule mc s [e1, e2] fc -> (configDict, t)
      Rule (Ident mc) s es (Ident fc) ->
        let
          (cd1, nmc) = freshName configDict mc
        in
          ifoldl (expand max) (cd1, Rule (Ident mc) s (head chunk) (Ident nmc)) (tail chunk)
        where
          chunk :: [[Term]]
          chunk = chunksOf 2 es
          max :: Int
          max = length (tail chunk) - 1
          expand :: Int -> ([String], Term) -> Int -> [Term] -> ([String], Term)
          expand n (cd, z) i as
            | i == n = (cd, Seq z (Rule (Ident $ lastName cd mc) (Symbol Elaborated) as (Ident fc)))
            | otherwise = let (cd1, nmc1) = freshName cd mc in (cd1, Seq z (Rule (Ident $ lastName cd mc) (Symbol Elaborated) as (Ident nmc1)))
      _ -> (configDict ,t)

specialSymbol :: [Atom] -> Term -> Term
specialSymbol symbolDict t = case t of
  Machine m ta -> Machine m (specialSymbol symbolDict ta)
  Table is -> Table (specialSymbol symbolDict is)
  Seq f s -> flatSeq (specialSymbol symbolDict f) (specialSymbol symbolDict s)
  Rule mc (Symbol NotBlank) es fc -> mkSeq (map (\s -> Rule mc (Symbol s) es fc) symbolDict)
  Rule mc (Symbol Elaborated) es fc -> mkSeq (map (\s -> Rule mc (Symbol s) es fc) (Blank : symbolDict))
  _ -> t