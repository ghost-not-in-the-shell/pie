module Var where

type Name = String

newtype Ix = Ix Int deriving (Eq, Show)
newtype Lv = Lv Int deriving (Eq, Show)

type Len = Int

ix2lv ∷ Len → Ix → Lv
ix2lv n (Ix i) = Lv $ n - i - 1

lv2ix ∷ Len → Lv → Ix
lv2ix n (Lv l) = Ix $ n - l - 1
