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



getBtns' wnd [] i btns = btns
getBtns' wnd (x:brd) i btns = getBtns' wnd brd (i + 1) (button wnd [text := (show i), clientSize := sz 1 1, bgcolor := getColor (toInt x)]:btns)

placeBtns :: (Form f, Widget w) => f -> [w] -> IO ()
placeBtns wnd btns = set wnd [layout := minsize (sz 500 500) $column 8 $ map (\x -> margin 3 $ row 8 (map widget x)) (chunksOf 8 btns)]

setCommand btns wnd ref = do
	let z = zip [1..64] btns
	forM_ z $ \p -> set (snd p) [on command := setField wnd (fst p) ref]


main :: IO ()
main
  = start hello

hello :: IO ()
hello = do
	let wndTitle = "Game"
	wnd <- frame [ text := wndTitle, bgcolor := grey ]
	let say desc = infoDialog wnd wndTitle desc
		
	let brd = startGame
	btns <- sequence $ getBtns' wnd (fieldFromIO brd) 0 []
	let st = GameState brd btns White
	ref <- newIORef st
	setCommand btns wnd ref
	placeBtns wnd btns
	
	return()
	
setField wnd k ref = do
	st <- readIORef ref
	let btns = buttons st
	let brd = board st
	let plr = player st
	let brd' = nextStep brd plr k
	updateBtns btns (fieldFromIO brd')
	writeIORef ref (GameState brd' btns (toglePlayer plr))
	return()

updateBtns :: [Button ()] -> Field -> IO ()
updateBtns btns brd = do
	let z = zip brd btns
	forM_ z $ \p -> set (snd p) [ bgcolor := getColor (toInt (fst p))]
	
data GameState = GameState {
	board :: IO Field,
	buttons :: [Button ()],
	player :: State
}

	
getColor 1 = red
getColor 0 = white
getColor 2 = blue

btnLabel :: State -> String
btnLabel Black = "Black"
btnLabel White = "White"
btnLabel Empty = "Empty"

toglePlayer Black = White
toglePlayer White = Black
