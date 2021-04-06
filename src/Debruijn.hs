module Debruijn where

newtype Ix = Ix Int deriving (Eq, Show) via Int
newtype Lv = Lv Int deriving (Eq, Show) via Int
newtype Id = Id Int deriving (Eq, Show) via Int

ix2lv ∷ Int → Ix → Lv
ix2lv n (Ix x) = Lv $ n - x - 1

lv2ix ∷ Int → Lv → Ix
lv2ix n (Lv x) = Ix $ n - x - 1
