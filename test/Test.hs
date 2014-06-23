{-# language
   DeriveGeneric
 , BangPatterns
 #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Test where

import System.IO
import Control.Exception

import GHC.Generics

import Control.DeepSeq
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.MemoTrie.Generic
import Data.Time.Clock
import Text.Printf

import Test.QuickCheck

propMemoId :: (HasTrie a, Eq a) => (a -> a) -> [a] -> Bool
propMemoId f as =
  let f' = memo f
      r1 = fmap f as
      r2 = fmap f' as
      r3 = reverse $ fmap f' (reverse as)
  in  all id $ zipWith3 (\x y z -> x == y && y == z) r1 r2 r3

data Creature = Human { pName :: String
                      , pAge  :: Int
                      , pHeight :: Int
                      }
              | Android { pId :: Int
                        , pModel :: String
                        , pDreams :: [Creature]
                        }
              | Animal { pKind :: String
                       , pLegs :: Int
                       }
  deriving (Show,Generic)

instance HasTrie Creature

android :: Creature
android = Android { pId = 5
                  , pModel = "Sonny"
                  , pDreams = [animal]
                  }

human :: Creature
human = Human { pName = "Rick Deckard"
              , pAge = 33
              , pHeight = 6
              }

animal :: Creature
animal = Animal { pKind = "Sheep"
                , pLegs = 4
                }

compute :: Creature -> Int
compute c = foldl' (\i v -> i + v) 0 [0..n]
  where
    bigN = (10::Int)^(8::Int)
    n = bigN + (l^(4::Int) `mod` bigN)
    l = length (show c)

properties :: IO ()
properties = do
  quickCheck (propMemoId ((*2) :: Int -> Int))
  quickCheck (propMemoId ((*2) :: Integer -> Integer))
  quickCheck (propMemoId ((*2) :: Float -> Float))
  quickCheck (propMemoId ((*2) :: Double -> Double))
  quickCheck (propMemoId ((*2) *** (*3) :: (Double,Int) -> (Double,Int)))
  quickCheck (propMemoId (toLower :: Char -> Char))

secElapsed :: NFData a => IO a -> IO (Double,a)
secElapsed act = do
  t1 <- getCurrentTime
  a <- act >>= (evaluate . force)
  t2 <- getCurrentTime
  let secs :: Double
      secs = fromRational . toRational $
             diffUTCTime t2 t1
      secs' = fromIntegral (round (secs * 100.0) :: Int) / 100.0
  return (secs',a)

runLongComputation :: NFData b => String -> [a] -> (a -> b) -> Int -> IO [[b]]
runLongComputation desc values f m = do
  let act () = do
        putStr "."
        hFlush stdout
        forM values $ (evaluate . force) . f

  putStrLn $ printf "%s (%d points):" desc m
  (secs,vals) <- secElapsed $ do
    forM [1..(m::Int)] $ const $ act ()
  putStrLn ""
  putStrLn $ printf "Sec elapsed: %f" secs

  return vals

bench :: IO ()
bench = do
  let values = [android,human,animal]
      m = 6

  vals1 <- runLongComputation "Slow"     values       compute  m
  vals2 <- runLongComputation "Memoized" values (memo compute) m

  when (vals1 /= vals2) $ do
    putStrLn $ printf "Computation result mismatch:\n\
                      \Expected\n%s\n\
                      \Got\n%s"
                      (show vals1)
                      (show vals2)

main :: IO ()
main = do
  properties
  bench
