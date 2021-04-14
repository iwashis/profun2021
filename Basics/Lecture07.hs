module Lecture07 where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Applicative
import Control.Monad

import Control.Monad.State.Strict
--- Zdefiniujmy instancję transformaty monadycznej dla monady Maybe --
--
newtype MaybeT1 m a = MaybeT1 { runMaybeT1 :: m (Maybe a) }

-- x :: MaybeT1 m a
-- runMaybeT1 x :: m (Maybe a)
-- y :: m (Maybe a)
-- MaybeT1 y :: MaybeT1 m a

instance MonadTrans MaybeT1 where
  lift x =  MaybeT1 $ fmap Just x -- lift :: m a -> MaybeT1 m a (wczesniej: m (Maybe a)) 
  -- Just :: a -> Maybe a; x |-> Just x
-- fmap f x = f <$> x
instance (Functor m) => Functor (MaybeT1 m) where
  fmap f x = MaybeT1 $ fmap f <$> runMaybeT1 x  
    -- x :: MaybeT1 m a === m(Maybe a)
    -- f : a -> b
    -- wynik :: MaybeT1 m b === m (Maybe b)

instance (Monad m) => Monad (MaybeT1 m) where
  xs >>= f = MaybeT1 $ runMaybeT1 xs >>= runMaybeT1 . maybeF
    where
      maybeF (Just x) = f x
      maybeF Nothing  = MaybeT1 $ return Nothing
    -- maybeF :: Maybe a -> m(Maybe b)
    -- xs :: MaybeT1 m a === m (Maybe a)
    -- f :: a-> MaybeT1 m b
    -- runMaybeT1 . f :: a -> m (Maybe b)
  return   = MaybeT1 . return . return  
  -- dla x :: a to return x :: MaybeT1 m a === m(Maybe a) 

instance (Monad m) => Applicative (MaybeT1 m) where
  pure    = return
  x <*> y = ap x y



-- Praca domowa.
-- Dokończyć definicję instancji transformaty StateT:

newtype StateT1 s m a = StateT1 { runStateT1 :: s -> m (a,s) }

instance MonadTrans (StateT1 s) where
  lift  = error "todo"

instance (Monad m) => Functor (StateT1 s m) where
  fmap  = error "todo"    

instance (Monad m) => Monad (StateT1 s m) where
  (>>=)  = error "todo"
  return = error "todo"

instance (Monad m) => Applicative (StateT1 s m) where
  pure    = return
  x <*> y = ap x y



--- Przykład 1.
--- Weźmy kod z poprzedniego wykładu.

type Cost = State Int

addCost :: (Double -> Double) -> Double -> Cost Double
addCost f r = do
 modify (\st -> if r < 0 then st - 3 else st+2)
 return $ f r

listOfFuncs = [ \x->x^2
              , \x->x^3-x^2-5
              ]

composeWithEffects addEffects list = foldr (>=>) return (map addEffects list)

composedCost = composeWithEffects addCost listOfFuncs
-- runState (composedCost 10) 0
--

type ExtraCost = StateT Double Cost
-- Zachodzą następujące izomorfizmy między typami:
-- ExtraCost a == Double -> Cost (a, Double) == Double -> Int -> ((a, Double), Int)
-- == Double -> Int -> (a, Double, Int) ==
-- == (Double, Int) -> (a, (Double, Int)) == State (Double,Int) a 

addExtraCost :: (Double -> Double) -> Double -> ExtraCost Double
addExtraCost f r = do 
  modify (*r)
  lift $ addCost f r

composedExtraCost = composeWithEffects addExtraCost listOfFuncs
--runState (runStateT (composedExtraCost 10) 0) 0


--- Przykład 2. 
--- Transformata MaybeT i monada IO


question1 :: IO String
question1 = do 
  putStrLn "Pytanie 1: Jak masz na imię?"
  getLine

question2 :: String -> IO (String,String) -- można przerobić
question2 name = do
  putStrLn $ "Witaj " 
            ++ name 
            ++ "! Miło Cię widzieć. Pytanie 2: Jak wabi się Twój pies?" -- ++ " Wpisz END jeśli nie masz psa"
  dog <- getLine
  return (name, dog)

summary :: (String, String) -> IO ()
summary (name,dog) = 
  putStrLn $ "Podsumowanie: Nazywasz się " 
           ++ name 
           ++ " a Twój pies wabi się " 
           ++ dog 

questionaire = question1 >>= question2 >>= summary



type MaybeIO = MaybeT IO -- IO (Maybe a)

question2' :: String -> MaybeIO (String,String) -- można przerobić
question2' yourName = do
  lift . putStrLn $ "Witaj " 
                  ++ yourName 
                  ++ "! Miło Cię widzieć. Pytanie 2: Jak wabi się Twój pies?" 
                  ++ " Wpisz BRAK jeśli nie masz psa"
  dogsName <- lift getLine
  if dogsName /= "BRAK" then 
    lift $ return (yourName, dogsName) 
  else 
    MaybeT (return Nothing)


questionaire' = lift question1 >>= question2' >>= lift . summary





