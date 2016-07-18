module Modi where

type Difficulty = Int

easy :: Difficulty
easy = 0

medium :: Difficulty
medium = 1

hard :: Difficulty
hard = 2

modi :: [(String,Difficulty)]
modi  = [("hard",hard),
         ("medium", medium),
         ("easy", easy)]
