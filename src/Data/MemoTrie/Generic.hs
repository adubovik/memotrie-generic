{-# language
   TypeOperators
 , TypeFamilies
 , DefaultSignatures
 , FlexibleContexts
 #-}

module Data.MemoTrie.Generic
 ( memo
 ) where

import Data.MemoTrie(HasTrie(..))
import GHC.Generics

memo :: (Generic a, GHasTrie (Rep a)) => (a -> b) -> (a -> b)
memo f = (gUntrie $ gTrie (f . to)) . from

class GHasTrie (f :: * -> *) where
  data (f :=>: b) :: *
  gTrie :: (f p -> b) -> (f :=>: b)
  gUntrie :: (f :=>: b) -> (f p -> b)

instance GHasTrie V1 where
  data (V1 :=>: b) = V1Trie
  gTrie _ = undefined
  gUntrie _ = undefined

instance GHasTrie U1 where
  data (U1 :=>: b) = U1Trie b
  gTrie f = U1Trie (f U1)
  gUntrie (U1Trie b) = \U1 -> b

instance (GHasTrie f, GHasTrie g) => GHasTrie (f :+: g) where
  data ((f :+: g) :=>: b) = SumTrie (f :=>: b) (g :=>: b)
  gTrie f = SumTrie (gTrie (f . L1)) (gTrie (f . R1))
  gUntrie (SumTrie f g) =
    \s -> case s of
      L1 a -> gUntrie f a
      R1 b -> gUntrie g b

instance (GHasTrie f, GHasTrie g) => GHasTrie (f :*: g) where
  data ((f :*: g) :=>: b) = ProdTrie (f :=>: (g :=>: b))
  gTrie fg = ProdTrie (gTrie $ \f -> gTrie $ \g -> fg (f :*: g))
  gUntrie (ProdTrie t) = \(f :*: g) -> gUntrie (gUntrie t f) g

instance HasTrie c => GHasTrie (K1 i c) where
  data (K1 i c :=>: b) = K1Trie (c :->: b)
  gTrie f = K1Trie (trie (f . K1))
  gUntrie (K1Trie t) = untrie t . unK1

instance GHasTrie f => GHasTrie (M1 i t f) where
  data (M1 i t f :=>: b) = M1Trie (f :=>: b)
  gTrie f = M1Trie (gTrie (f . M1))
  gUntrie (M1Trie t) = gUntrie t . unM1



