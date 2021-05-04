module AppState where

import Automaton
import Comonad.Tape


data AppState = AppState { width    :: Int
                         , height   :: Int
                         , autom    :: Maybe Automaton
                         , evol     :: Maybe Evolution
                         , allAutom :: [ Automaton ]
                         }


