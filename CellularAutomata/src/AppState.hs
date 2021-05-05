{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module AppState where

import Automaton

data AppState = AppState { automaton   :: Automaton
                         , evolution   :: Evolution
                         , allAutomata      :: [ Automaton ]
                         }



defAppState  = calcEvolution (AppState automaton90 nullEvolution Automaton.allAutomata)

setAutomaton :: Automaton -> AppState -> AppState
setAutomaton autom AppState {automaton, ..} =
  AppState {automaton = autom, ..}


setEvolution :: Evolution -> AppState -> AppState
setEvolution evol AppState {evolution, ..} =
  AppState {evolution = evol, ..}


calcEvolution :: AppState -> AppState
calcEvolution AppState { evolution, automaton, ..} =
  AppState { evolution = calculateEvolution automaton, ..}


nullifyEvolution :: AppState -> AppState
nullifyEvolution AppState { evolution, ..} =
  AppState { evolution = nullEvolution, ..}


setAllAutomata :: [Automaton]-> AppState -> AppState
setAllAutomata automata AppState {allAutomata, ..} =
  AppState {allAutomata = automata, ..}
