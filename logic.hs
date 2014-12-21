module Logic where

import Data.List

type Field = [State]

data State = Black | White | Empty
	deriving (Show, Eq)

count :: Int
count = 8

startField :: IO Field
startField = return $ replicate (count * count) Empty

getState :: Field -> Int -> Int -> State
getState field i j = field !! (i * count + j)

setState :: Field -> State -> Int -> Int -> Field
setState field state i j = take (i * count + j) field ++ [state] ++ drop (i * count + j + 1) field

toInt :: State -> Int
toInt Black = -1
toInt White = 1
toInt Empty = 0