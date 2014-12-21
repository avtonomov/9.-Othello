module Types where

type Field = [State]

type Position = (Int, Int)

data State = Black | White | Empty
	deriving (Show, Eq)

count :: Int
count = 8