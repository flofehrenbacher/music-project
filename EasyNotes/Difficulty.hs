module Difficulty where

data Difficulty = Easy | Medium | Hard
    deriving (Eq,Read,Show,Enum)

-- | puts all modi into a list of strings
-- the first data type of Difficulty MUST be Easy
getAllModi :: [String]
getAllModi = map show $ enumFrom Easy