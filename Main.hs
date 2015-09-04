module Main where

import UI.HSCurses.Curses hiding (pi)
import Data.Convertible.Base
import Data.Convertible.Instances
import Data.Word
import Control.Concurrent

main :: IO ()
main = do
	win <- initScr
	startColor
	wclear win
	cursSet CursorInvisible 
	echo False
	let Just fgColor = color "cyan"
	initPair (Pair 1) fgColor (Color 0) >> attrSet attr0 (Pair 1)
	mainDrawLoop win 0 20 0.05

mainDrawLoop :: Window -> Double -> Double -> Double  -> IO ()
mainDrawLoop win offset amplitude hz = do
	drawGraph win $ \x -> amplitude*sin(hz*pi*x + offset)
	mvWAddStr win 0 0 $ "A: " ++ show amplitude ++ " Hz: " ++ show hz 
	refresh
	input <- getCh
	case input of
		KeyChar 'd' -> mainDrawLoop win (offset-0.2) amplitude hz 
		KeyChar 'a' -> mainDrawLoop win (offset+0.2) amplitude hz
		KeyChar 'w' -> mainDrawLoop win offset (amplitude+1) hz
		KeyChar 's' -> mainDrawLoop win offset (amplitude-1) hz
		KeyChar 'e' -> mainDrawLoop win offset amplitude (hz-0.002)
		KeyChar 'r' -> mainDrawLoop win offset amplitude (hz+0.002)
		KeyChar 'q' -> endWin
		_           -> mainDrawLoop win offset amplitude hz

drawGraph :: Window -> (Double -> Double) -> IO ()
drawGraph win f = do
	wclear win
	(y, x) <- scrSize
	let coords = map (\x ->  (x, f x)) [0, 0.01..(realToFrac x)]
	mapM_ (drawPoint . centerAndRound y) coords
		where drawPoint (x, y) = mvAddCh y x (convert '#')
		      centerAndRound y (x', y') = (round x', round (y' + (realToFrac y)/2))  
