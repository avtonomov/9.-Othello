module Logic where

import Data.List
import System.IO.Unsafe

type Field = [State]

type Position = (Int, Int)

data Direction = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight
	deriving (Show)

data State = Black | White | Empty
	deriving (Show, Eq)

count :: Int
count = 8

startField :: Field
startField = replicate (count * count) Empty

startFieldIO :: IO Field
startFieldIO = return $ replicate (count * count) Empty

fieldToIO :: Field -> IO Field
fieldToIO = return

--знаю, что unsafe, но что поделать...
fieldFromIO :: IO Field -> Field
fieldFromIO = unsafePerformIO

getState :: Field -> Position -> State
getState field pos = field !! ((fst pos - 1) * count + (snd pos - 1))

setState :: Field -> State -> Position -> Field
setState field state pos = take ((fst pos - 1) * count + (snd pos - 1)) field ++ [state] ++ drop ((fst pos - 1) * count + snd pos) field

countBlack :: Field -> Int
countBlack field = foldl (\s x -> if x == Black then s+1 else s ) 0 field 

countWhite :: Field -> Int
countWhite field = foldl (\s x -> if x == White then s+1 else s ) 0 field 

countEmpty :: Field -> Int
countEmpty field = foldl (\s x -> if x == Empty then s+1 else s ) 0 field 

toInt :: State -> Int
toInt Black = -1
toInt White = 1
toInt Empty = 0

isValidPosition :: Position -> Bool
isValidPosition pos = if fst pos > 0 && fst pos <= count && snd pos > 0 && snd pos <= count then True else False

printField :: Field -> IO ()
printField f = putStr $ getStrField f 1
	where
		getStrField field i
			| i < count = show (take count field) ++ "\n" ++ getStrField (drop count field) (i+1)
			| otherwise = show (take count field) ++ "\n"
			
f2 = setState startField White (4,4)
f3 = setState f2 Black (4,3)
f4 = setState f3 Black (3,4)
f5 = setState f4 Black (3,5)
f6 = setState f5 Black (3,6)
f7 = setState f6 White (3,7)

test = f7
--getState (fieldFromIO startFieldIO) (0,0) 
