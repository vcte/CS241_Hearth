-- ----------------------------------------------------- --
-- --| Hearth - Haskell Interpreter for Haiku Forth |--- --
-- ----------------------------------------------------- --

import GHC.Float
import Data.Word
import qualified Data.ByteString as B

import Control.Monad
import System.IO
import System.Exit
import System.Environment

import Hearth

type PainterFunc = (Double -> Double -> Double -> [Double])

-- main entrance - run one frame of given Haiku Forth code, to measure performance
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
								
								-- calculate sum of a single frame
								print $ B.foldl' (+) 0 $ frame func 0.0
							
-- Utility function to return Haiku Forth code, given command prompt arguments
readArgs 					:: [String] -> IO String
readArgs [] 				= readFile "haiku.txt"			-- read from default file, if no arguments given
readArgs [x]				= readFile x					-- read from given filename, if one argument given
readArgs ["-c", c]			= return $ c					-- read code directly from input, if -c option given
readArgs _ 					= return $ ""					-- otherwise, return empty string
							
-- Produce a frame of the animation in the given moment in time
-- For Gloss, the origin is at the top left corner (pixel coordinates)
-- In Haiku Forth, the origin is at the bottom left corner (cartesian coordinates)
-- foldl constructs the list of pixel values in reverse, starting at bottom right corner
frame						:: PainterFunc -> Float -> B.ByteString
frame f time				= (B.pack $ concat [(pixel f x y t)						-- build up a linked list to convert into a byte string
											   | y <- coords, 						-- starting from point (255, 255), to point (0, 0)
												 x <- coords])
							where
								t = float2Double time
								coords = reverse [0 / 256, 1 / 256 .. 255 / 256]
								
-- calculate the rgba value for a single pixel
-- For Gloss, the pixel is stored as [A, B, G, R]
-- Haiku Forth outputs pixel as [A, B, G, R]
pixel						:: PainterFunc -> Double -> Double -> Double -> [Word8]
pixel f x y t				= map (\e -> ceiling ((max 0 $ min 1 $ e) * 255)) stk	-- convert to number between [0, 256)
							where
								-- convert coordinate to be a floating point number between 0 and 1
								stk = f x y t
								