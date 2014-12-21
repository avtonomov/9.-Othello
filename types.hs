module Types where

type Field = [State]

type Position = (Int, Int)

data State = Black | White | Empty
	deriving (Show, Eq)
	
data Direction = Up | Down | Left1 | Right1 | UpLeft | UpRight | DownLeft | DownRight
	deriving (Show, Eq)
	
type FoundPositions = [(Position, Direction)]

count :: Int
count = 8