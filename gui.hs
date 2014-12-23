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
getBtns' wnd (x:brd) i btns = getBtns' wnd brd (i + 1) (button wnd [text := (show i), clientSize := sz 1 1, bgcolor := getColor (x)]:btns)

placeBtns :: (Form f, Widget w) => f -> [w] -> IO ()
placeBtns wnd btns = set wnd [layout := minsize (sz 500 500) $column 8 $ map (\x -> margin 3 $ row 8 (map widget x)) (chunksOf 8 btns)]

setCommand btns wnd ref = do
	let z = zip [1..64] btns
	forM_ z $ \p -> set (snd p) [on command := setField wnd (fst p) ref]

setCommand_up btns ref = do
	st <- readIORef ref
	let brd = board st
	let plr = player st
	let z = zip [1..64] btns
	forM_ z $ \p -> update_button (snd p) (isMoved brd plr (fst p))


can_move btns ref xs = do
	st <- readIORef ref
	let brd = board st
	let plr = player st
	let z = zip [1..64] btns
	let temp = 0
	forM_ z $ \p -> if isMoved brd plr (fst p)==True then 1:xs
	
update_button p True = do
	set p [bgcolor := green]

update_button p False = do
	return() 


main :: IO ()
main
  = start hello


new_game ref = do
	st <- readIORef ref
	let btns = buttons st
	let brd' = startGame
	updateBtns btns (fieldFromIO brd')
	writeIORef ref (GameState brd' btns White)
	setCommand_up btns ref

hello :: IO ()
hello = do
	let wndTitle = "Game"
	wnd <- frame [ text := wndTitle, bgcolor := grey ]
	let say desc = infoDialog wnd wndTitle desc
	let brd = startGame
	btns <- sequence $ getBtns' wnd (fieldFromIO brd) 0 []
	let st = GameState brd btns White
	ref <- newIORef st

	top_Menu <- menuPane [text := "Игра"]
	menuItem top_Menu [on command := new_game ref, text := "Новая игра"]
	menuQuit top_Menu [on command := wxcAppExit, text := "Выход"]

  
	
	set wnd [menuBar := [top_Menu]]

	setCommand btns wnd ref
	placeBtns wnd btns
	setCommand_up btns ref
	return()
	
setField wnd k ref = do
	let wndTitle = "Game"
	let say desc = infoDialog wnd wndTitle desc
	st <- readIORef ref
	let btns = buttons st
	let brd = board st
	let plr = player st
	let brd' = nextStep brd plr k
	end_game brd' say
	updateBtns btns (fieldFromIO brd')
	writeIORef ref (GameState brd' btns (toglePlayer plr $ isMoved brd plr k))
	setCommand_up btns ref
	return()

end_game brd say
	|isEndGame brd ==True && winner brd == Black = say "Black power" 
	|isEndGame brd ==True && winner brd == White = say "White power" 
	|isEndGame brd ==True && winner brd == Empty = say "Nothing power" 
	|otherwise =  return()

updateBtns :: [Button ()] -> Field -> IO ()
updateBtns btns brd = do
	let z = zip brd btns
	forM_ z $ \p -> set (snd p) [ bgcolor := getColor (fst p)]
	
data GameState = GameState {
	board :: IO Field,
	buttons :: [Button ()],
	player :: State
}

	
getColor White = red
getColor Empty = white
getColor Black = blue

btnLabel :: State -> String
btnLabel Black = "Black"
btnLabel White = "White"
btnLabel Empty = "Empty"


toglePlayer::State -> Bool -> State
toglePlayer Black True = White
toglePlayer White True = Black
toglePlayer Black False = Black
toglePlayer White False = White
