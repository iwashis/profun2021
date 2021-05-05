module UI
    ( displayAll
    ) where
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Automaton
import AppState


drawRule :: Rule -> Rule -> Widget ()
drawRule selectedRule rule = withBorderStyle borderStyle
  $ borderWithLabel (str $ name rule)
  $ hCenter
  $ padAll 1
  $ str $ show $ ruleFunction rule
    where
      borderStyle = if rule == selectedRule then unicodeBold  else unicode

maxEvolutionWidth  = 100
maxEvolutionHeight = 120

drawEvolution :: Int -> Int -> Evolution -> Widget ()
drawEvolution width height evol = withBorderStyle unicode
  $ borderWithLabel (str "Evolution")
  $ hCenter
  $ padAll 1
  $ str $ unlines $ showEvolution evol (\x-> if x then '+' else ' ') width height


drawRules :: AppState -> Widget ()
drawRules appState = foldl1 (<+>) $ drawRule selectedRule <$> rules
  where
    rules = rule <$> AppState.allAutomata appState
    autom = automaton appState
    selectedRule = rule autom

drawAppState :: AppState -> Widget ()
drawAppState appState = vLimit 40 (drawEvolution maxEvolutionWidth maxEvolutionHeight evol)
  <=> drawRules appState
    where
      evol = AppState.evolution appState


displayAll :: IO ()
displayAll = simpleMain $ drawAppState defAppState
