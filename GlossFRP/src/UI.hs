module UI
    ( someFunc
    ) where
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Yampa (playYampa)
import Ball
import FRP.Yampa
import Control.Concurrent


someFunc :: IO ()
someFunc =
  playYampa (InWindow "Bouncy bouncy ..." (round windowWidth, round windowHeight) (200, 200)) white 100 play
    where
      play :: SF a Picture -- SF () Ball
      play = arr (const ()) >>> mainSignal >>> arr drawPicture

      drawPicture :: Ball -> Picture
      drawPicture ((px,py), _ ) = translate (-((windowWidth/2)- px )) (-((windowHeight/2)- py )) $ circleSolid 6
        {-
  reactimate (return ())
              (\_-> threadDelay 100 >> return (0.01, Nothing) )
              ( \_ x -> print x  >> return False )
              mainSignal
              -}
