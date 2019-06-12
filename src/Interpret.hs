module Interpret where

import Core
import Control.Monad.State.Lazy
import Data.Map (Map)
import qualified Data.Map as Map

data Config = Config String Atom deriving (Eq, Ord) -- m-config and symbol
data Behaviour = Behaviour Step String -- operations and f-config
type Context = Map Config Behaviour -- program's config/behaviour context

data CompleteConfig = CompleteConfig
  { tape :: [Atom]     -- symbol tape
  , head :: Int        -- tape index
  , config :: String } -- m-config

update :: Context -> CompleteConfig -> CompleteConfig
update cx cc = undefined

run :: Term -> Int -> State CompleteConfig [Atom]
run t steps = undefined
  where
    cx :: Context
    cx = undefined