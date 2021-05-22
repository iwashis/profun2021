module AppState where

import Philosopher
import Control.Monad

type AppState = [Philosopher]

update :: Philosopher -> AppState -> AppState
update ph = map (\x-> if x == ph then ph else x)

constant = 5

defaultAppState :: AppState
defaultAppState = [ Philosopher "A" Hungry
                  , Philosopher "B" Hungry
                  , Philosopher "C" Hungry
                  , Philosopher "D" Hungry
                  , Philosopher "E" Hungry
                  ]

defaultTableIO :: TableIO
defaultTableIO = replicateM constant $ newFork Utensil



