{-# language
   TypeOperators
 , TypeFamilies
 , DefaultSignatures
 , FlexibleContexts
 , UndecidableInstances
 , CPP
 #-}

module Data.MemoTrie.Generic
 ( memo
 , mup
 , HasTrie(..)
 ) where

import Data.Bits
import Data.Word
import Data.Int

import GHC.Generics

newtype MemoWrap a = MemoWrap a

class HasTrie a where
  type (a :->: b) :: *
  type (a :->: b) = MemoWrap (Rep a :=>: b)

  trie :: (a -> b) -> (a :->: b)
  untrie :: (a :->: b) -> (a -> b)

  default trie :: (Generic a, GHasTrie (Rep a)) =>
                  (a -> b) -> (MemoWrap (Rep a :=>: b))
  trie f = MemoWrap $ gTrie (f . to)

  default untrie :: (Generic a, GHasTrie (Rep a)) =>
                    (MemoWrap (Rep a :=>: b)) -> (a -> b)
  untrie (MemoWrap t) = gUntrie t . from

-- | Trie-based function memoizer
memo :: HasTrie a => (a -> b) -> (a -> b)
memo = untrie . trie

-- | Lift a memoizer to work with one more argument.
mup :: HasTrie t => (b -> c) -> (t -> b) -> (t -> c)
mup mem f = memo (mem . f)

---- HasTrie instances for primitive types
-- (straight from Data.MemoTrie@MemoTrie)

#define IntInstance(IntType,WordType,TrieType) \
newtype TrieType a = TrieType a; \
instance HasTrie IntType where \
  type IntType :->: a = TrieType (WordType :->: a); \
  untrie (TrieType t) = untrie t . (fromIntegral :: IntType -> WordType); \
  trie f = TrieType (trie (f . (fromIntegral :: WordType -> IntType)));

IntInstance(Int,Word,IntTrie)
IntInstance(Int8,Word8,Int8Trie)
IntInstance(Int16,Word16,Int16Trie)
IntInstance(Int32,Word32,Int32Trie)
IntInstance(Int64,Word64,Int64Trie)

#define WordInstance(Type,TrieType)\
newtype TrieType a = TrieType a; \
instance HasTrie Type where \
  type Type :->: a = TrieType ([Bool] :->: a);\
  trie f = TrieType (trie (f . unbits));\
  untrie (TrieType t) = untrie t . bits;\

WordInstance(Word,WordTrie)
WordInstance(Word8,Word8Trie)
WordInstance(Word16,Word16Trie)
WordInstance(Word32,Word32Trie)
WordInstance(Word64,Word64Trie)

-- | Extract bits in little-endian order
bits :: (Num t, Bits t) => t -> [Bool]
bits 0 = []
bits x = testBit x 0 : bits (shiftR x 1)

-- | Convert boolean to 0 (False) or 1 (True)
unbit :: Num t => Bool -> t
unbit False = 0
unbit True  = 1

-- | Bit list to value
unbits :: (Num t, Bits t) => [Bool] -> t
unbits [] = 0
unbits (x:xs) = unbit x .|. shiftL (unbits xs) 1

newtype CharTrie a = CharTrie a

instance HasTrie Char where
  type Char :->: a = CharTrie (Int :->: a)
  untrie (CharTrie t) n = untrie t (fromEnum n)
  trie f = CharTrie (trie (f . toEnum))

---- Generic counterpart of HasTrie - GHasTrie

class GHasTrie (f :: * -> *) where
  data (f :=>: b) :: *
  gTrie :: (f p -> b) -> (f :=>: b)
  gUntrie :: (f :=>: b) -> (f p -> b)

instance GHasTrie V1 where
  data (V1 :=>: b) = V1Trie
  gTrie _ = V1Trie
  gUntrie V1Trie = undefined

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

---- Deriving HasTrie instances via GHC.Generics machinery

#define H(x) HasTrie (x)

instance H()
instance (H(a),H(b))                          => H((a,b))
instance (H(a),H(b),H(c))                     => H((a,b,c))
instance (H(a),H(b),H(c),H(d))                => H((a,b,c,d))
instance (H(a),H(b),H(c),H(d),H(e))           => H((a,b,c,d,e))
instance (H(a),H(b),H(c),H(d),H(e),H(f))      => H((a,b,c,d,e,f))
instance (H(a),H(b),H(c),H(d),H(e),H(f),H(g)) => H((a,b,c,d,e,f,g))

instance H(Bool)

instance H(a) => H([a])
instance (H(a),H(b)) => H(Either a b)
instance H(a) => H(Maybe a)



