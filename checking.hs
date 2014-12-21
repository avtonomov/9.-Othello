module Checking where

import Types

getState :: Field -> Position -> State
getState field pos = field !! ((fst pos - 1) * count + (snd pos - 1))

setState :: Field -> State -> Position -> Field
setState field state pos = take ((fst pos - 1) * count + (snd pos - 1)) field ++ [state] ++ drop ((fst pos - 1) * count + snd pos) field

isValidPosition :: Position -> Bool
isValidPosition pos = if fst pos > 0 && fst pos <= count && snd pos > 0 && snd pos <= count then True else False

checkUp :: Field -> State -> Position -> Position
checkUp field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (15,10)
			| fst pos == 1 = (15,10) -- если находимся на верхней клетке
			| getState field (fst pos - 1, snd pos) == Empty = (20,10) -- если клетка сверху пуста
			| getState field (fst pos - 1, snd pos) == state = if (flag == False) then (10,10) else (fst pos - 1, snd pos) -- если клетка сверху нашего цвета
			| otherwise = check field state True (fst pos - 1, snd pos) -- если клетка сверху не нашего цвета

checkDown :: Field -> State -> Position -> Position
checkDown field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (15,10)
			| fst pos == count = (15,10) -- если находимся на нижней клетке
			| getState field (fst pos + 1, snd pos) == Empty = (20,10) -- если клетка снизу пуста
			| getState field (fst pos + 1, snd pos) == state = if (flag == False) then (10,10) else (fst pos + 1, snd pos) -- если клетка снизу нашего цвета
			| otherwise = check field state True (fst pos + 1, snd pos) -- если клетка снизу не нашего цвета

checkRight :: Field -> State -> Position -> Position
checkRight field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (15,10)
			| snd pos == count = (15,10) -- если находимся на правой клетке
			| getState field (fst pos, snd pos + 1) == Empty = (20,10) -- если клетка справа пуста
			| getState field (fst pos, snd pos + 1) == state = if (flag == False) then (10,10) else (fst pos, snd pos + 1) -- если клетка справа нашего цвета
			| otherwise = check field state True (fst pos, snd pos + 1) -- если клетка справа не нашего цвета

checkLeft :: Field -> State -> Position -> Position
checkLeft field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (15,10)
			| snd pos == 1 = (15,10) -- если находимся на левой клетке
			| getState field (fst pos, snd pos - 1) == Empty = (20,10) -- если клетка слева пуста
			| getState field (fst pos, snd pos - 1) == state = if (flag == False) then (10,10) else (fst pos, snd pos - 1) -- если клетка слева нашего цвета
			| otherwise = check field state True (fst pos, snd pos - 1) -- если клетка слева не нашего цвета

checkUpRight :: Field -> State -> Position -> Position
checkUpRight field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (15,10)
			| fst pos == 1 = (15,10) -- если находимся на верхней клетке
			| snd pos == count = (15,10) -- если находимся на правой клетке
			| getState field (fst pos - 1, snd pos + 1) == Empty = (20,10) -- если клетка справа сверху пуста
			| getState field (fst pos - 1, snd pos + 1) == state = if (flag == False) then (10,10) else (fst pos - 1, snd pos + 1) -- если клетка справа сверху нашего цвета
			| otherwise = check field state True (fst pos - 1, snd pos + 1) -- если клетка справа сверху не нашего цвета

checkUpLeft :: Field -> State -> Position -> Position
checkUpLeft field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (10,10)
			| fst pos == 1 = (10,10) -- если находимся на верхней клетке
			| snd pos == 1 = (10,10) -- если находимся на левой клетке
			| getState field (fst pos - 1, snd pos - 1) == Empty = (20,10) -- если клетка слева сверху пуста
			| getState field (fst pos - 1, snd pos - 1) == state = if (flag == False) then (10,10) else (fst pos - 1, snd pos - 1) -- если клетка слева слева нашего цвета
			| otherwise = check field state True (fst pos - 1, snd pos - 1) -- если клетка слева слева не нашего цвета

checkDownRight :: Field -> State -> Position -> Position		
checkDownRight field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (10,10)
			| fst pos == count = (10,10) -- если находимся на нижней клетке
			| snd pos == count = (10,10) -- если находимся на правой клетке
			| getState field (fst pos + 1, snd pos + 1) == Empty = (10,10) -- если клетка справа снизу пуста
			| getState field (fst pos + 1, snd pos + 1) == state = if (flag == False) then (10,10) else (fst pos + 1, snd pos + 1) -- если клетка справа снизу нашего цвета
			| otherwise = check field state True (fst pos + 1, snd pos + 1) -- если клетка справа снизу не нашего цвета


checkDownLeft :: Field -> State -> Position -> Position		
checkDownLeft field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (10,10)
			| fst pos == count = (10,10)-- если находимся на нижней клетке
			| snd pos == 1 = (10,10) -- если находимся на левой клетке
			| getState field (fst pos + 1, snd pos - 1) == Empty = (20,10) -- если клетка слева снизу пуста
			| getState field (fst pos + 1, snd pos - 1) == state = if (flag == False) then (10,10) else (fst pos + 1, snd pos - 1) -- если клетка слева снизу нашего цвета
			| otherwise = check field state True (fst pos + 1, snd pos - 1) -- если клетка слева снизу не нашего цвета
			
			
checkPosition :: Field -> State -> Position -> FoundPositions
checkPosition field state pos = check field state pos 1
	where
		check field state pos i
			| i == 1 = if (isValidPosition (checkUp field state pos) == True) then ((checkUp field state pos), Up):check field state pos (i+1) else check field state pos (i+1)
			| i == 2 = if (isValidPosition (checkDown field state pos) == True) then ((checkDown field state pos), Down):check field state pos (i+1) else check field state pos (i+1)
			| i == 3 = if (isValidPosition (checkLeft field state pos) == True) then ((checkLeft field state pos), Left1):check field state pos (i+1) else check field state pos (i+1)
			| i == 4 = if (isValidPosition (checkRight field state pos) == True) then ((checkRight field state pos), Right1):check field state pos (i+1) else check field state pos (i+1)
			| i == 5 = if (isValidPosition (checkUpRight field state pos) == True) then ((checkUpRight field state pos), UpRight):check field state pos (i+1) else check field state pos (i+1)
			| i == 6 = if (isValidPosition (checkUpLeft field state pos) == True) then ((checkUpLeft field state pos), UpLeft):check field state pos (i+1) else check field state pos (i+1)
			| i == 7 = if (isValidPosition (checkDownRight field state pos) == True) then ((checkDownRight field state pos), DownRight):check field state pos (i+1) else check field state pos (i+1)
			| i == 8 = if (isValidPosition (checkDownLeft field state pos) == True) then ((checkDownLeft field state pos), DownLeft):check field state pos (i+1) else check field state pos (i+1)
			| otherwise = []

directionMove :: Field -> State -> Position -> (Position, Direction) -> Field
directionMove field state pos finish_pos
	| snd finish_pos == Up = if (pos /= fst finish_pos) then directionMove (setState field state (fst pos - 1, snd pos)) state (fst pos - 1, snd pos) finish_pos else field
	| snd finish_pos == Down = if (pos /= fst finish_pos) then directionMove (setState field state (fst pos + 1, snd pos)) state (fst pos + 1, snd pos) finish_pos else field
	| snd finish_pos == Right1 = if (pos /= fst finish_pos) then directionMove (setState field state (fst pos, snd pos + 1)) state (fst pos, snd pos + 1) finish_pos else field
	| snd finish_pos == Left1 = if (pos /= fst finish_pos) then directionMove (setState field state (fst pos, snd pos - 1)) state (fst pos, snd pos - 1) finish_pos else field
	| snd finish_pos == UpRight = if (pos /= fst finish_pos) then directionMove (setState field state (fst pos - 1, snd pos + 1)) state (fst pos - 1, snd pos + 1) finish_pos else field
	| snd finish_pos == UpLeft = if (pos /= fst finish_pos) then directionMove (setState field state (fst pos - 1, snd pos - 1)) state (fst pos - 1, snd pos - 1) finish_pos else field
	| snd finish_pos == DownLeft = if (pos /= fst finish_pos) then directionMove (setState field state (fst pos + 1, snd pos + 1)) state (fst pos + 1, snd pos + 1) finish_pos else field
	| snd finish_pos == DownRight = if (pos /= fst finish_pos) then directionMove (setState field state (fst pos + 1, snd pos + 1)) state (fst pos + 1, snd pos + 1) finish_pos else field
	| otherwise = field


move :: Field -> State -> Position -> FoundPositions -> Field
move field _ _ [] = field
move field state pos (x:xs) = move (directionMove (setState field state pos) state pos x) state pos xs


moving :: Field -> State -> Position -> Field
moving field state pos = move field state pos (checkPosition field state pos)


