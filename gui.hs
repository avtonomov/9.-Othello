module Main where
import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import PuzzleLogic
import Data.IORef
import Control.Monad as Monad 


main :: IO ()
main
  = start hello

hello :: IO ()
hello = do
	let wndTitle = "Game"
	wnd <- frame [ text := wndTitle, bgcolor := blue ]
	brd <- generateGameIO
	btns <- getBtns wnd brd
	placeBtns wnd btns
	
	let st = GameState brd btns
	ref <- newIORef st
	
  {--
  = do f    <- frame    [text := "Hello!"]
       quit <- button f [text := "Quit", on command := close f]
       set f [layout := margin 10 (column 5 [floatCentre (label "Hello")
                                     ,floatCentre (widget quit)
                                     ] )]
--}
data GameState = GameState {
	board :: Board,
	buttons :: [Button ()]
}

btnLabel :: Integer -> String
btnLabel x = if (x > 64) then "" else (show x)
									 
getBtns :: Window a -> Board -> IO ([Button ()])
getBtns wnd brd = sequence $ map (\x -> button wnd [text := (btnLabel x), clientSize := sz 50 50]) (elems brd)