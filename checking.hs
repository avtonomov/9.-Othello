import Logic

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
			| getState field (fst pos - 1, snd pos - 1) == state = if (flag == False) then (10,10) else (fst pos - 1, snd pos - 1) -- если клетка слева сверху нашего цвета
			| otherwise = check field state True (fst pos - 1, snd pos - 1) -- если клетка слева сверху не нашего цвета

checkDownRight :: Field -> State -> Position -> Position		
checkDownRight field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (10,10)
			| fst pos == count = (10,10) -- если находимся на нижней клетке
			| snd pos == count = (10,10) -- если находимся на правой клетке
			| getState field (fst pos + 1, snd pos + 1) == Empty = (10,10) -- если клетка справа сверху пуста
			| getState field (fst pos + 1, snd pos + 1) == state = if (flag == False) then (10,10) else (fst pos + 1, snd pos + 1) -- если клетка справа сверху нашего цвета
			| otherwise = check field state True (fst pos + 1, snd pos + 1) -- если клетка сверху не нашего цвета


checkDownLeft :: Field -> State -> Position -> Position		
checkDownLeft field state pos = check field state False pos
	where
		check field state flag pos
			| isValidPosition pos == False = (10,10)
			| fst pos == count = (10,10)-- если находимся на нижней клетке
			| snd pos == 1 = (10,10) -- если находимся на левой клетке
			| getState field (fst pos + 1, snd pos + 1) == Empty = (20,10) -- если клетка слева снизу пуста
			| getState field (fst pos + 1, snd pos + 1) == state = if (flag == False) then (10,10) else (fst pos + 1, snd pos + 1) -- если клетка слева снизу нашего цвета
			| otherwise = check field state True (fst pos + 1, snd pos + 1) -- если клетка слева снизу не нашего цвета