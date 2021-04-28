module UI
    ( displayAll
    ) where


import Control.Concurrent (threadDelay)
import Control.Monad (forM_, replicateM_)
import System.IO (hFlush, stdout)
import Text.Printf(printf)
import Data.Colour.SRGB (sRGB24)
import System.Console.ANSI
import System.Environment

import Comonad.Tape
import Automata

-- troche funkcji pomocnicznych do obsługi terminala.
resetScreen :: IO () -- nie trzeba chyba tłumaczyć do czego ta funkcja służy
resetScreen = do 
  setSGR [Reset]   
--  setSGR [SetColor Background Dull Black] -- kolor tła 
  setSGR [SetColor Foreground Vivid Red]  -- ustawienie koloru pierwszego planu
  clearScreen >> setCursorPosition 0 0

-- pauza 0.05 sekundowa. Zmieniając warość liczbową poniżej,
-- zmieniamy długość pauzy (jednostki = milisekundy). 
pause :: IO ()
pause = do
  hFlush stdout
  threadDelay 50000


displayEvolution :: [Tape Char] -> IO ()
displayEvolution list = do
  Just (termHeight, termWidth) <- getTerminalSize -- Pobranie wielkości terminala.    
  let frameWidth = (termWidth `div` 2) - 2        -- Ustawienie szerokości ramki
                                                  -- wykorzystywanej w type FramedTape.
  let framedTapeList = frame frameWidth <$> take (termHeight-4) list --lista taśm do wyświetlenia.
  -- bierzemy już tylko tyle taśm, ile równona się wysokość okna terminala - 4,
  -- bo po co więcej ?
  resetScreen
  forM_ framedTapeList (\x -> (putStrLn . show)  x >> pause)


allRules = [ rule30, rule90 ] -- wszystkie zasady jakie były zdefiniowane

-- funkcja wyświetlająca ewolucje wszystkich zasad zapisanych w allRules
displayAll = 
  forM_ allRules displayRule
  where 
    displayRule rule = 
      displayEvolution $ 
        fmap (\x -> if x then '+' else ' ') <$> 
          applyRule initialTape rule
