-- | This module shows the different difficulties used in Easy Notes
module Difficulty where

data Difficulty = Easy | Medium | Hard
    deriving (Eq,Read,Show,Enum)

-- | Puts all modes into a list of strings. 
-- The first data type of Difficulty /must/ be 'Easy'
getAllModes :: [String]
getAllModes = map show $ enumFrom Easy