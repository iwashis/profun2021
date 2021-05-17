{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module AppState where

import Automaton

-- Główny typ danych reprezentujący stan naszej aplikacji
data AppState = AppState { automaton   :: Automaton
                         , evolution   :: Evolution
                         , allAutomata :: [ Automaton ]
                         }

-- domyślny stan aplikacji (z którego nasza aplikacja startuje. Patrz UI.hs)
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

nextAutomaton :: AppState -> AppState
nextAutomaton appState@AppState {automaton, allAutomata, ..} =
  case maybeNewAutomaton of
    Nothing -> appState
    Just newAutomaton -> calcEvolution $ setAutomaton newAutomaton appState
    where
      autoPairs = zip allAutomata (tail allAutomata ++ [head allAutomata])
      maybeNewAutomaton = lookup automaton autoPairs
