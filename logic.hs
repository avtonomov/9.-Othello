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
			


moving :: Field -> State -> Position -> Field
moving field state pos = move field state pos (checkPosition field state pos)

f1 = setState startField Black (4,4)
f2 = setState f1 White (3,4)
f3 = setState f2 Black (4,5)
f4 = setState f3 White (3,6)
test = f4
--getState (fieldFromIO startFieldIO) (0,0) 


