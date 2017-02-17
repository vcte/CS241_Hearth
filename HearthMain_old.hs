-- ----------------------------------------------------- --
-- --| Hearth - Haskell Interpreter for Haiku Forth |--- --
-- ----------------------------------------------------- --

import Graphics.Gloss
import GHC.Float
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Numeric as N

import Control.Monad
import System.IO
import System.Exit
import System.Environment

import Hearth

type PainterFunc = (Double -> Double -> Double -> [Double])
type Model = (PainterFunc, Float, Map.Map Key Picture)
type Key = [[String]]

-- main entrance / loop
main 						:: IO()
main 						= do
								-- parse arguments
								args <- getArgs
								code <- readArgs args
								
								-- print usage, return if error parsing input
								when (code == "") $ do {
									print "Usage: [filename] | [-c] [code]";
									exitFailure;
								}
								
								-- parse code into valid haiku forth expression
								-- level the expression, so that 4 values are always returned
								expr <- return $ level $ parseHaiku code
								
								-- create a partial function, w/ first parameter filled by the haiku expression
								func <- return $ interpretHaiku expr
								
								-- display animation or static image, depending on contents of code
								if   not $ isAnimation expr
								then image func
								else if isRandom expr
								then animation func
								else simulation func

-- Utility function to return Haiku Forth code, given command prompt arguments
readArgs 					:: [String] -> IO String
readArgs [] 				= readFile "haiku.txt"			-- read from default file, if no arguments given
readArgs [x]				= readFile x					-- read from given filename, if one argument given
readArgs ["-c", c]			= return $ c					-- read code directly from input, if -c option given
readArgs _ 					= return $ ""					-- otherwise, return empty string
								
-- Window used by all graphics routines
window						:: Display
window						= InWindow "Hearth" (256, 256) (10, 10)					-- size is (256, 256), initial position is (10, 10)
								
-- Produce a static image
image						:: PainterFunc -> IO()
image f						= display window (white) (frame f 0)					-- display one frame of animation, with time = 0
							
-- Produce an animation
animation					:: PainterFunc -> IO()
animation f					= animate window (white) (frame f)						-- run animation
							
-- Use simulation, to produce an animation with frame caching
simulation					:: PainterFunc -> IO()
simulation f				= simulate window (white) (60) (f, 0, Map.empty) 		-- run simulation at 60 fps, initialize model 
									   (visualize) (step)
									   
-- Convert the simulation model, into the picture to display
visualize					:: Model -> Picture
visualize (f, time, cache)	= Map.findWithDefault (Blank) (computeKey f time) cache

-- Step the simulation model forward by one iteration - adjust current time, add new picture to cache if not in cache
step						:: a -> Float -> Model -> Model
step _ dt (f, time, cache)	= (f, time', cache')
							where
								time' = time + dt
								cache' = case (Map.member key cache) of
											True	-> cache
											False	-> Map.insert key pic cache
								
								-- compute the key for the given moment in time
								key = computeKey f time'
								
								-- get drawing of frame at the next time step
								-- lazily evaluated, so only computed if map lookup fails
								pic = frame f time'
								
-- compute key to the cache, as list of rounded pixel values at certain locations in the frame
computeKey					:: PainterFunc -> Float -> Key
computeKey f time			= map (\(x, y) -> map round2 $ f x y t)
							$ map (\(x, y) -> (x / 256, y / 256))
							$ [(0, 0), (255, 255), (0, 64), (64, 0)] ++
							  [(64, 64), (128, 64), (192, 64), (64, 128), (128, 128), (192, 128), (64, 192), (128, 192), (192, 192)]
							where
								-- round floating point number to two decimal places
								round2 num = N.showFFloat (Just 2) (max 0 $ min 1 $ num) ""
								
								-- convert time to double, round to nearest tick
								t = fromIntegral (floor (time * 60)) / 60
							
-- Produce a frame of the animation in the given moment in time
-- For Gloss, the origin is at the top left corner (pixel coordinates)
-- In Haiku Forth, the origin is at the bottom left corner (cartesian coordinates)
-- concat constructs the list of pixel values in order, starting at top left corner
frame						:: PainterFunc -> Float -> Picture
frame f time				= bitmapOfByteString 256 256 							-- make a bitmap of size (256, 256)
							 (B.pack $ concat [(pixel f x y t)						-- build up a linked list to convert into a byte string
											  | y <- coords, 						-- starting from point (0, 0), to point (255, 255)
												x <- coords]) False					-- disable frame caching
							where
								t = float2Double time
								
								-- convert coordinate to be a floating point number between 0 and 1
								coords = [0 / 256, 1 / 256 .. 255 / 256]
								
-- calculate the rgba value for a single pixel, at a moment in time
-- For Gloss, the pixel is stored in the bitmap as [A, B, G, R]
-- Haiku Forth outputs pixel as [A, B, G, R]
pixel						:: PainterFunc -> Double -> Double -> Double -> [Word8]
pixel f x y t				= map (\e -> ceiling ((max 0 $ min 1 $ e) * 255)) stk	-- convert to number between [0, 256)
							where
								stk = f x y t
								