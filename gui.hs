module Main where
import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import Logic
import Data.IORef
import Control.Monad as Monad 

fPath :: String
fPath = "save.txt"


getBtns :: Window a -> Field -> IO ([Button ()])
getBtns wnd brd = sequence $ map (\x -> button wnd [text := (btnLabel x), clientSize := sz 30 30, bgcolor := getColor (toInt x), on command := setField wnd 1]) brd

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
	
	brd <- startField
	btns <- getBtns wnd brd
	placeBtns wnd btns
	
	
	let st = GameState brd btns
	ref <- newIORef st
	
	filePath <- varCreate Nothing
	saveGame ref wnd filePath
	
	loadGame ref wnd filePath
	return()
	
setField :: Window a -> Int -> IO()
setField wnd k = do
	brd <- startField
	btns <- getBtns wnd brd
	let st = GameState brd btns
	ref <- newIORef st
	filePath <- varCreate Nothing
	loadGame ref wnd filePath
	
	saveGame ref wnd filePath
	
	
	return()
