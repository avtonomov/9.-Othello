module Main where
import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import Logic
import Data.IORef
import Control.Monad as Monad 


data GameState = GameState {
	board :: Field,
	buttons :: [Button ()]
}

btnLabel :: Int -> String
btnLabel x = if (x > 64) then "" else (show x)

getBtns :: Window a -> Field -> IO ([Button ()])
getBtns wnd brd = sequence $ map (\x -> button wnd [text := (btnLabel $ (toInt x)), clientSize := sz 30 30]) brd

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
	
	brd <- startField
	btns <- getBtns wnd brd
	
	placeBtns wnd btns
	
	return()