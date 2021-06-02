{-# LANGUAGE Arrows #-}
module Ball where

import FRP.Yampa
import Data.VectorSpace

-- SF a b == (Time -> a) -> (Time -> b)
--
windowWidth :: Float
windowWidth = 600.0
windowHeight :: Float
windowHeight = 600.0



type Position = (Float, Float)
type Velocity = (Float, Float)
type Ball     = (Position, Velocity)

ballSignal :: Ball -> SF () Ball --- Ball -> ( Time -> Ball )
ballSignal (x0 ,v0 ) =  constant (0,-9.81) >>> integral >>> arr (^+^ v0) >>> ( (integral >>> arr (^+^ x0)) &&& identity )


data EventType = HitBottom Ball | HitLeft Ball | HitRight Ball | HitTop Ball

generateEvent :: Ball -> (Ball, Event EventType)
generateEvent ball@((px,py), _ )  | px < 0  = (ball, Event $ HitLeft ball)
                                  | py < 0  = (ball, Event $ HitBottom ball)
                                  | px > windowWidth = (ball, Event $ HitRight ball)
                                  | py > windowHeight= (ball, Event $ HitTop ball)
                                  | otherwise = (ball, NoEvent)

initPos = (400,400)
initVel = (10, 20)

signal :: Ball -> SF () Ball
signal ball  = switch (ballSignal ball >>> arr generateEvent) g
  where
    g :: EventType -> SF () Ball
    g (HitLeft ((px,py),(vx,vy) )) = signal ((0,py), (-vx, vy) )
    g (HitBottom ((px,py),(vx,vy) )) = signal ((px,0), (vx, -vy) )
    g (HitRight ((px,py),(vx,vy) )) = signal ((windowWidth,py), (-vx, vy) )
    g (HitTop ((px,py),(vx,vy) )) = signal ((px,windowHeight), (vx, -vy) )


mainSignal = signal (initPos, initVel)



















































mirrorImageReflection:: (Float,Float) -> (Float,Float) -> (Float,Float)
mirrorImageReflection (a,b) (p,q) = (  ( (b^2-a^2)*p-2*a*b*q ) /(a^2+b^2) , ( (a^2-b^2)*q -2*a*b*p ) /(a^2+b^2) )  -- (a,b) jest wektorem normalnym, (p,q)--punktem do translacji


