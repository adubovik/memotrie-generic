{-# language
   DeriveGeneric
 #-}

module Test where

import System.Environment

import GHC.Generics

import Control.Monad
import Data.List
import Data.MemoTrie.Generic

data Creature = Human { pName :: String
                      , pAge  :: Int
                      , pHeight :: Int
                      }
              | Android { pId :: Int
                        , pModel :: String
                        -- FIXME: recursive datatypes
                        -- aren't supported yet
                        -- , pDreams :: [Creature]
                        }
              | Animal { pKind :: String
                       , pLegs :: Int
                       }
  deriving (Show,Generic)

android :: Creature
android = Android { pId = 5
                  , pModel = "Sonny"
                  -- , pDreams = [animal]
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

main :: IO ()
main = do
  args <- getArgs
  let computeFunction = case args of
        [] -> compute
        _  -> memo compute
  let act () = mapM_ (putStrLn . show . computeFunction)
               [android,human,animal]
  forM_ [0..(100::Int)] $ \_ ->
    act ()
