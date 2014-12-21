module Logic where

import Data.List

type Field = [State]

type Position = (Int, Int)

data Direction = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight
	deriving (Show)

data State = Black | White | Empty
	deriving (Show, Eq)

count :: Int
count = 8

startField :: IO Field
startField = return $ replicate (count * count) Empty

getState :: IO Field -> Position -> IO State
getState field pos = field !! (fst pos * count + snd pos)

setState :: Field -> State -> Position -> Field
setState field state pos = take (fst pos * count + snd pos) field ++ [state] ++ drop (fst pos * count + snd pos + 1) field

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

printField :: Field -> IO ()
printField f = putStr $ getStrField f 1
	where
		getStrField field i
			| i < count = show (take count field) ++ "\n" ++ getStrField (drop count field) (i+1)
			| otherwise = show (take count field) ++ "\n"

--проверяем возможность хода на позиции i j
-- state - цвет ходящего
--checkMove :: Field -> State -> Int -> Int

checkUp :: Field -> State -> Bool -> Position -> (Bool, Position)
checkUp field state flag pos
	| fst pos < 2 = (False, (15,10)) -- если находимся на верхней клетке или на второй сверху
	| getState field (fst pos - 1, snd pos) == Empty = (False, (20,10)) -- если клетка сверху пуста
	| getState field (fst pos - 1, snd pos) == state = if (flag == False) then (False, (10,10)) else (True, (fst pos - 1, snd pos)) -- если клетка сверху нашего цвета
	| otherwise = checkUp field state True ((fst pos - 1, snd pos)) -- если клетка сверху не нашего цвета
{-
f1 = setState startField White (3,3)
f2 = setState f1 White (4,4)
f3 = setState f2 Black (4,3)
f4 = setState f3 Black (3,4)

test = printField f4
-}