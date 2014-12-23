module Logic where

import Data.List
import System.IO.Unsafe
import Checking
import Types

startField :: Field
startField = replicate (count * count) Empty

startFieldIO :: IO Field
startFieldIO = return $ replicate (count * count) Empty

fieldToIO :: Field -> IO Field
fieldToIO = return

--знаю, что unsafe, но что поделать...
fieldFromIO :: IO Field -> Field
fieldFromIO = unsafePerformIO

toInt :: State -> Int
toInt Black = -1
toInt White = 1
toInt Empty = 0

countBlack :: Field -> Int
countBlack field = foldl (\s x -> if x == Black then s+1 else s ) 0 field 

countWhite :: Field -> Int
countWhite field = foldl (\s x -> if x == White then s+1 else s ) 0 field 

countEmpty :: Field -> Int
countEmpty field = foldl (\s x -> if x == Empty then s+1 else s ) 0 field 

printField :: Field -> IO ()
printField f = putStr $ getStrField f 1
	where
		getStrField field i
			| i < count = show (take count field) ++ "\n" ++ getStrField (drop count field) (i+1)
			| otherwise = show (take count field) ++ "\n"

-- поле - цвет ходящего - координаты хода
moving :: Field -> State -> Position -> Field
moving field state pos = if (getState field pos == Empty) then move field state pos (checkPosition field state pos) else field

f1 = setState startField Black (4,5)
f2 = setState f1 White (4,4)
f3 = setState f2 Black (5,4)
f4 = setState f3 White (5,5)

startGame = fieldToIO f4

test = f4

intToPair k
	| (k `mod` count) == 0 = ((k `div` count), count)
	| otherwise = ((k `div` count) + 1, (k `mod` count))

nextStep :: IO Field -> State -> Int -> IO Field
nextStep field state k = fieldToIO $ moving (fieldFromIO field) state $ intToPair k

cmpField :: Field -> Field -> Bool
cmpField [] [] = True
cmpField (x:f1) (y:f2)
	| x == y = cmpField f1 f2
	| otherwise = False

isMoved :: IO Field -> State -> Int -> Bool
isMoved field state k
	| cmpField (fieldFromIO field) (fieldFromIO $ nextStep field state k) = False
	| otherwise = True

isEndGame :: IO Field -> Bool
isEndGame field = (isEnd (fieldFromIO field) White 0) && (isEnd (fieldFromIO field) Black 0)
	where
		isEnd field state k
			| k == count*count - 1 = True
			| otherwise = if (length (checkPosition field state (intToPair k)) > 0) then False else isEnd field state (k+1)
			
winner :: IO Field -> State
winner field 
	| isEndGame field == True = if (countBlack (fieldFromIO field) > countWhite (fieldFromIO field)) then Black else White
	| otherwise = Empty
