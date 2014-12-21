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
moving field state pos = move field state pos (checkPosition field state pos)

f1 = setState startField Black (4,5)
f2 = setState f1 White (4,4)
f3 = setState f2 Black (5,4)
f4 = setState f3 White (5,5)

start = f4

writeGameToFileIO :: Field -> FilePath -> IO ()
writeGameToFileIO arr filename = writeFile filename $ unwords $ map show $ arr

saveGame :: IORef GameState -> Window a -> Var(Maybe FilePath) -> IO ()
saveGame ref win filePath = do
	maybePath <- fileSaveDialog win True True "Сохранение игры..." [("Any file",["*.*"]),("Text",["*.txt"])] "" ""
	print maybePath
	case maybePath of
		Nothing -> return ()
		Just path -> do
			varSet filePath $ Just path
			st <- readIORef ref
			let brd = board st
			writeGameToFileIO brd path
			
loadGame :: IORef GameState -> Window a -> Var(Maybe FilePath) -> IO ()
loadGame ref win filePath = do
	maybePath <- fileOpenDialog win True True "Загрузка игры..." [("Any file",["*.*"]),("Text",["*.txt"])] "" ""
	print maybePath
	case maybePath of
		Nothing -> return ()
		Just path -> do
		varSet filePath $ Just path
		st <- readIORef ref
		let btns = buttons st
		brd' <- readGameFromFileIO path
		updateBtns btns brd'
		writeIORef ref (GameState brd' btns)
		
updateBtns :: [Button ()] -> Field -> IO ()
updateBtns btns brd = do
	let z = zip brd btns
	forM_ z $ \p -> set (snd p) [text := (btnLabel (fst p)), bgcolor := getColor (toInt (fst p))]
	
readGameFromFileIO :: FilePath -> IO Field
readGameFromFileIO filename = do
	elems <- readFromFileIO filename
	return elems
	
readFromFileIO :: FilePath -> IO [State]
readFromFileIO filename = do
	content <- readFile filename
	let elems = concatMap words $ lines content
	return $ map toState elems :: IO [State]
