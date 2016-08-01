module Modus where

data Modus = Easy | Medium | Hard
    deriving (Eq,Read,Show,Enum)

getAllModi :: [String]
getAllModi = map show $ enumFrom Easy
