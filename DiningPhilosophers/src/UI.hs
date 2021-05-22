module UI
    ( someFunc
    ) where
import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.BChan (newBChan, writeBChan)
import Graphics.Vty as V
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import AppState
import Philosopher

newtype Tick = Tick Philosopher

-- rysowanie pojedynczego filozofa
drawPhilosopher :: Philosopher -> Widget ()
drawPhilosopher (Philosopher name state) = hLimit 20
  $ vLimit 20
  $ withBorderStyle unicodeRounded
  $ borderWithLabel ( str name )
  $ padAll 1
  $ str $ show state

-- rysowanie stanu aplikacji (czyli listy filozofów)
drawAppState :: AppState -> Widget ()
drawAppState appState =
  if length appState `mod` 2 == 1 then drawOdd appState else drawEven appState
  where
    drawOdd :: AppState -> Widget ()
    drawOdd ap  = hCenter (drawPhilosopher (head ap)) <=> hCenter (drawEven $ tail ap)

    drawEven :: AppState -> Widget ()
    drawEven [] = emptyWidget
    drawEven (p:ps) =
      drawPhilosopher p <+> drawPhilosopher (last ps)
      <=> drawEven (init ps)


-- obsługa zdarzeń (w tym przypadku wciśnięć przycisków klawiatury)
handleEvent appState (AppEvent (Tick philo)) =  continue $ AppState.update philo appState
handleEvent appState (VtyEvent (V.EvKey V.KEsc [])) = halt appState -- zatrzymanie aplikacji po wcisnięciu Esc
handleEvent appState _ = continue appState -- jeśli stanie się cokolwiek innego niż dwa zdarzenia powyżej,
                                           -- to nie robimy nic

-- atrybuty rysowania. Możemy je zostawić puste na potrzeby naszej aplikacji
-- sugeruję sprawę zbadać dokładniej na innych przykładach dostępnych w sieci
theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App AppState Tick ()
app = App { appDraw = return . drawAppState
          , appChooseCursor =  neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

someFunc = do
  chan <- newBChan 10 -- stworzenie kanału na którym bedzie się wysyłać sygnały
                      -- powodujące zmiane stanu aplikacji
  table <- defaultTableIO -- tu jest nasz stół składający się z mutowalnych zmiennych Fork
  let forkpairs = zip table (tail $ cycle table)           -- pary sąsiadujących ze sobą widelcy
      runPhilosophers = runPhilosopher <$> defaultAppState -- lista funkcji typu (Fork,Fork)->Chan-> IO()
                                                           -- Patrz niżej.
      runPhilosophersWithForks = [ f forkPair chan | (f,forkPair) <- zip runPhilosophers forkpairs]
                                                           -- lista elementów typu IO ()
                                                           -- te akcje uruchomimy w osobnych wątkach
  mapM_ forkIO runPhilosophersWithForks -- uruchomienie akcji z listy powyżej w osobnych wątkach.

  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app defaultAppState -- uruchomienie aplikacji
  where
    runPhilosopher philo (leftfork, rightfork) chan = do -- akcja która wysyła sygnały zmiany stanu aplikacji
      threadDelay  1000000
      forever $ do
        writeBChan chan (Tick $ toHungry philo)
        pickUpForks philo (leftfork, rightfork)
        writeBChan chan (Tick $ toEating philo)
        doing philo
        putDownForks philo (leftfork, rightfork)
        writeBChan chan (Tick $ toThinking philo)
        doing philo
