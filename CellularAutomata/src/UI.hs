module UI
    ( displayAll
    ) where
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty as V
import Automaton
import AppState

-- funkcja pomocnicza używana w drawAppState
-- służąca do wyświetlania ramek z zasadami automatów
drawRules :: AppState -> Widget ()
drawRules appState = foldl1 (<+>) $ drawRule <$> rules
  where
    rules = [ (rule aut, name aut, aut == AppState.automaton appState) | aut <- AppState.allAutomata appState]
    autom = automaton appState
    selectedRule = rule autom

    -- funkcja pomocnicza uzywana w drawRules (a potem w drawAppState)
    -- służąca do rysowania ramki z nazwą zasady i jej kodem
    drawRule :: (Rule, String, Bool) -> Widget ()
    drawRule (rule, automatonName , isSelected) =
      withBorderStyle borderStyle
      $ borderWithLabel (str automatonName )
      $ hCenter
      $ padAll 1
      $ str $ show rule
      where
        borderStyle =  if isSelected then unicodeBold  else unicode



maxEvolutionWidth  = 100
maxEvolutionHeight = 120

-- funkcja pomocnicza rysująca ramkę z tytułem "Evolution"
-- wypełnioną ewolucją wybranego automatu.
drawEvolution :: Int -> Int -> Evolution -> Widget ()
drawEvolution width height evol = withBorderStyle unicode
  $ borderWithLabel (str "Evolution")
  $ hCenter
  $ padAll 1
  $ str $ unlines $ showEvolution evol (\x-> if x then '+' else ' ') width height

-- funkcja główna do wyświetlania stanu aplikacji
drawAppState :: AppState -> [Widget ()]
drawAppState appState = return $
  vLimit 40 (drawEvolution maxEvolutionWidth maxEvolutionHeight evol)
  <=> drawRules appState
    where
      evol = AppState.evolution appState

-- obsługa zdarzeń (w tym przypadku wciśnięć przycisków klawiatury)
handleEvent appState (VtyEvent (V.EvKey V.KRight [])) = continue $ nextAutomaton appState
handleEvent appState (VtyEvent (V.EvKey V.KEsc [])) = halt appState -- zatrzymanie aplikacji po wcisnięciu Esc
handleEvent appState _ = continue appState -- jeśli stanie się cokolwiek innego niż dwa zdarzenia powyżej,
                                           -- to nie robimy nic

-- atrybuty rysowania. Możemy je zostawić puste na potrzeby naszej aplikacji
-- sugeruję sprawę zbadać dokładniej na innych przykładach dostępnych w sieci
theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App AppState () ()
app = App { appDraw = drawAppState
          , appChooseCursor =  neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

displayAll = do
  defaultMain app defAppState
  return ()
