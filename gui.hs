module Main where
import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import Logic
import Checking
import Types
import Data.IORef
import Control.Monad as Monad 

fPath :: String
fPath = "save.txt"




--getBtns :: Window a -> Field -> IO ([Button ()])
--getBtns wnd brd = sequence $ map (\x -> button wnd [text := (btnLabel x), clientSize := sz 30 30, bgcolor := getColor (toInt x), on command := setField wnd 1]) brd
getBtns wnd [] i btns = btns
getBtns wnd (x:brd) i btns = getBtns wnd brd (i + 1) (button wnd [text := (show i), clientSize := sz 30 30, bgcolor := getColor (toInt x), on click := setField wnd i]:btns)

-- прикрепляет кнопки к окну
placeBtns :: (Form f, Widget w) => f -> [w] -> IO ()
placeBtns wnd btns = set wnd [layout := minsize (sz 500 500) $column 8 $ map (\x -> margin 3 $ row 8 (map widget x)) (chunksOf 8 btns)]

main :: IO ()
main
  = start hello

hello :: IO ()
hello = do
	let wndTitle = "Game"
	wnd <- frame [ text := wndTitle, bgcolor := grey ]
	let say desc = infoDialog wnd wndTitle desc
	
	brd <- startFieldIO
	btns <- sequence $ getBtns wnd brd 0 []
	placeBtns wnd btns
	
	
	let st = GameState brd btns
	ref <- newIORef st
	
	filePath <- varCreate Nothing
	--saveGame ref wnd filePath
	
	loadGame ref wnd filePath
	return()
	
setField :: Window a -> Int -> IO()
setField wnd k = do
	brd <- startFieldIO
	btns <- sequence $ getBtns wnd brd 0 []
	--placeBtns wnd btns
	
	let st = GameState brd btns
	ref <- newIORef st
	
	--nextStep k
	
	filePath <- varCreate Nothing
	loadGame ref wnd filePath
	
	--saveGame ref wnd filePath
	return()
	
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

data GameState = GameState {
	board :: Field,
	buttons :: [Button ()]
}

	
getColor 1 = red
getColor 0 = white
getColor -1 = blue
