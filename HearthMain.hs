{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

-- ----------------------------------------------------- --
-- --| Hearth - Haskell Interpreter for Haiku Forth |--- --
-- ----------------------------------------------------- --

import qualified "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Control.Monad
import Control.Applicative
import Data.Maybe
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Data.Time.Clock.POSIX
import System.Exit
import System.IO
import System.Environment

import Hearth

data GLIDs = GLIDs
	{ progId			:: !GLuint
	, vertexArrayId		:: !GLuint
	, vertexBufferId	:: !GLuint
	}

main							:: IO ()
main							= do
									-- parse arguments
									args <- getArgs
									code <- readArgs args
									
									let tran = readArgsTran args
									let verb = readArgsVerb args
									let size = readArgsSize args
									
									-- print usage and return if error parsing input
									when (code == "") $ do {
										print "Usage: HearthMain [-t] [-v] [-s size] [[filename] | [-c code]]";
										exitFailure;
									}
									
									-- parse code into valid haiku forth expression
									-- level the expression, so that 4 values are always returned
									expr <- return $ level $ parseHaiku code
									
									-- create GLSL code from the haiku forth expression
									-- generate code w/o temporary variables, if '-t' option given
									glsl <- if not tran
											then return $ translateHaiku expr
											else return $ translateHaiku' expr
									
									-- print out GLSL code, if '-v' / verbosity option set
									when (verb) $ do {
										putStrLn glsl
									}
									
									-- get time when program first starts
									time <- realToFrac <$> getPOSIXTime
									
									-- initialize, enter main loop, then finish when application is closed
									win <- initialize size
									glids <- initializeGL glsl
									inputLoop win glids time
									freeResources glids
									GLFW.terminate
									return ()
	
-- Utility function to return Haiku Forth code, given command line arguments
readArgs 					:: [String] -> IO String
readArgs ("-t":xs)			= readArgs xs					-- skip -t translation argument
readArgs ("-v":xs)			= readArgs xs					-- skip -v verbosity argument
readArgs ("-s":s:xs)		= readArgs xs					-- skip -s size argument
readArgs [] 				= readFile "haiku.txt"			-- read from default file, if no arguments given
readArgs [x]				= readFile x					-- read from given filename, if one argument given
readArgs ("-c":c:_)			= return $ c					-- read code directly from input, if -c option given
readArgs _					= return ""						-- otherwise, return error string

-- Utility function to read -t, or translation, command line option
readArgsTran				:: [String] -> Bool
readArgsTran				= any ((==) "-t")

-- Utility function to read -v, or verbosity, command line option
readArgsVerb				:: [String] -> Bool
readArgsVerb				= any ((==) "-v")

-- Utility function to read -s, or size, command line option, default size is 256 x 256
readArgsSize				:: [String] -> Int
readArgsSize []			= 256
readArgsSize ("-s":s:_)	= read s :: Int
readArgsSize (x:xs)		= readArgsSize xs
								
-- adapted from: http://funloop.org/post/2014-03-15-opengl-from-haskell.html
-- initialize window
initialize :: Int -> IO GLFW.Window
initialize size = do
	ok <- GLFW.init
	when (not ok) $ do
		_ <- fail "Failed to initialize GLFW"
		exitFailure
	mapM_ GLFW.windowHint
		[ GLFW.WindowHint'Samples 4 			-- 4x antialiasing
		, GLFW.WindowHint'ContextVersionMajor 3	-- OpenGL 3.3
		, GLFW.WindowHint'ContextVersionMinor 3
		-- we don't want the old OpenGL
		, GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
		]

	win <- GLFW.createWindow size size "Hearth" Nothing Nothing
	when (isNothing win) $ do
		_ <- fail "Failed to create OpenGL window"
		GLFW.terminate
		exitFailure
	
	let win' = fromJust win
	GLFW.makeContextCurrent win
	GLFW.setStickyKeysInputMode win' GLFW.StickyKeysInputMode'Enabled

	return win'

-- setup OpenGL objects, shaders
initializeGL :: String -> IO GLIDs
initializeGL glsl = do
	-- specify RGBA bitmask used to clear color buffers
	glClearColor 0 0 0 0
	progId <- loadProgram vertexShader' glsl
	vaId <- newVAO
	bufId <- fillNewBuffer vertexBufferData
	return $ GLIDs
		{ progId = progId
		, vertexArrayId = vaId
		, vertexBufferId = bufId
		}

-- free all buffer and array objects
freeResources :: GLIDs -> IO ()
freeResources GLIDs{..} = do
	with vertexBufferId $ glDeleteBuffers 1
	with vertexArrayId $ glDeleteVertexArrays 1

-- return a new VAO object
newVAO :: IO GLuint
newVAO = do
	vaId <- withNewPtr (glGenVertexArrays 1)
	glBindVertexArray vaId
	return vaId

-- transform a linked list of floats into a pointer to an array of ints
fillNewBuffer :: [GLfloat] -> IO GLuint
fillNewBuffer xs = do
	bufId <- withNewPtr (glGenBuffers 1)
	glBindBuffer gl_ARRAY_BUFFER bufId
	withArrayLen xs func -- give given vertices to OpenGL
	return bufId
	where
	func len ptr = glBufferData
		gl_ARRAY_BUFFER
		(fromIntegral (len * sizeOf (undefined :: GLfloat)))
		(ptr :: Ptr GLfloat)
		gl_STATIC_DRAW

-- given a buffer ID, and pointer to a buffer location, bind them
bindBufferToAttrib :: GLuint -> GLuint -> IO ()
bindBufferToAttrib bufId attribLoc = do
	glEnableVertexAttribArray attribLoc
	glBindBuffer gl_ARRAY_BUFFER bufId
	glVertexAttribPointer
		attribLoc			-- attribute location in the shader
		3					-- 3 components per vertex
		gl_FLOAT			-- coord type
		(fromBool False)	-- normalize?
		0					-- stride
		nullPtr				-- vertex buffer offset

-- load a new program, with the given shaders
loadProgram :: String -> String -> IO GLuint
loadProgram vertShader fragShader = do
	shaderIds <- mapM (uncurry loadShader)
		[ (gl_VERTEX_SHADER, vertShader)
		, (gl_FRAGMENT_SHADER, fragShader)
		]
	progId <- glCreateProgram
	putStrLn "Linking program"
	mapM_ (glAttachShader progId) shaderIds
	glLinkProgram progId
	_ <- checkStatus
		gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog progId
	mapM_ glDeleteShader shaderIds
	return progId

-- load and compile the shader source code string
loadShader :: GLenum -> String -> IO GLuint
loadShader shaderTypeFlag code = do
	shaderId <- glCreateShader shaderTypeFlag
	withCString code $ \codePtr ->
		with codePtr $ \codePtrPtr ->
			glShaderSource shaderId 1 codePtrPtr nullPtr
	putStrLn "Compiling shader..."
	glCompileShader shaderId
	_ <- checkStatus
		gl_COMPILE_STATUS glGetShaderiv glGetShaderInfoLog shaderId
	return shaderId

-- check current status
checkStatus :: (Integral a1, Storable a1)
	=> GLenum
	-> (t -> GLenum -> Ptr a1 -> IO a)
	-> (t -> a1 -> Ptr a3 -> Ptr Foreign.C.Types.CChar -> IO a2)
	-> t
	-> IO Bool
checkStatus statusFlag glGetFn glInfoLogFn componentId = do
	let
		fetch info = withNewPtr (glGetFn componentId info)
	status <- liftM toBool $ fetch statusFlag
	logLength <- fetch gl_INFO_LOG_LENGTH
	when (logLength > 0) $
		allocaArray0 (fromIntegral logLength) $ \msgPtr -> do
			_ <- glInfoLogFn componentId logLength nullPtr msgPtr
			msg <- peekCString msgPtr
			(if status then putStrLn else fail) msg
	return status

fragmentShader' :: String
fragmentShader' = unlines
	[ "#version 330 core"
	, "varying vec2 tpos;"
	, "uniform float time_val;"
	, "float PI = 3.1415926535897931;"
	, "out vec3 color;"
	, "void main()"
	, "{"
		 , "color = vec3(sin((tpos.x * 10.0) + time_val) * 0.1, 0.0, " ++ 
		   "1 - sqrt (abs(cos(tpos.x * 10.0) * cos(time_val) * 0.1 - tpos.y + 0.5)));"
	, "}"
	]
	
vertexShader' :: String
vertexShader' = unlines
	[ "#version 330 core"
	, "layout(location = 0) in vec3 vPosition_modelspace;"
	, "varying highp vec2 tpos;"
	, "void main()"
	, "{"
		, "tpos.x = ((vPosition_modelspace.x) + 1.0) / 2.0;"
		, "tpos.y = ((vPosition_modelspace.y) + 1.0) / 2.0;"
		, "gl_Position.xyz = vPosition_modelspace;"
		, "gl_Position.w = 1.0;"
	, "}"
	]
	
vertexBufferData :: [GLfloat]
vertexBufferData =
	-- x, y, z
	[ -1, -1, 0
	, -1, 1, 0
	, 1, -1, 0
	, 1, -1, 0
	, -1, 1, 0
	, 1, 1, 0
	]

inputLoop :: GLFW.Window -> GLIDs -> Double -> IO ()
inputLoop win glids time = do
	drawStuff glids time
	GLFW.swapBuffers win
	GLFW.pollEvents
	keyState <- GLFW.getKey win GLFW.Key'Escape
	closeWindow <- GLFW.windowShouldClose win
	when (keyState /= GLFW.KeyState'Pressed && closeWindow == False) $
		inputLoop win glids time

drawStuff :: GLIDs -> Double -> IO ()
drawStuff GLIDs{..} time = do
	glClear gl_COLOR_BUFFER_BIT
	glClear gl_DEPTH_BUFFER_BIT
	glUseProgram progId
	bindBufferToAttrib vertexBufferId 0
	glDrawArrays gl_TRIANGLES 0 6 -- for attrib array 0, draw 6 vertices
	glDisableVertexAttribArray 0 -- disable attrib array 0
	
	-- from: http://stackoverflow.com/questions/11726563/how-can-i-convert-a-haskell-string-into-a-ptr-ptr-glchar
	uniformLoc <- withCString "time_val" $ \c_string -> glGetUniformLocation progId $ castPtr c_string
	
	Just time' <- GLFW.getTime -- getPOSIXTime
	glUniform1f uniformLoc $ (realToFrac (time'))

withNewPtr :: Storable b => (Ptr b -> IO a) -> IO b
withNewPtr f = alloca (\p -> f p >> peek p)
