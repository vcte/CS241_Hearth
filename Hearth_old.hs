{-# LANGUAGE BangPatterns #-}

-- module declaration - export Hearth functions
module Hearth (parseHaiku, 
			   interpretHaiku, 
			   parseAndInterpretHaiku, 
			   isAnimation,
			   isRandom,
			   level)
where

-- ----------------------------------------------------- --
-- --| Hearth - Haskell Interpreter for Haiku Forth |--- --
-- ----------------------------------------------------- --

-- forthsalon.appspot.com

-- type definition of Haiku Forth expression
data Word = Expr ([Double] -> [Double])								-- data stack altering function
		  | Stack (([Double], [Double]) -> ([Double], [Double])) 	-- function affects data and return stacks
		  | Rand													-- random number generator function
		  | Var String 												-- variable, can be "x", "y" or "t"
		  | Const Double											-- numerical constant
		  | Func String												-- marks beginning or end of defined function
		  | Cmt String												-- marks beginning or end of comment
		  
-- define print functions for Word types
instance Show (Word) where
	show (Expr _)	= "<expr>"
	show (Stack _)	= "<stack>"
	show (Rand)		= "<random>"
	show (Var s)	= "Var " ++ s
	show (Const d)	= "Const " ++ show d
	show (Func s)	= "Func " ++ s
	show (Cmt s)	= "Cmt " ++ s
		  
-- parse a Haiku Forth expression, containing multiple words
parseHaiku					:: String -> [Word]
parseHaiku s				= parseHaiku' $ routines $ comments $ words s
							where
								parseHaiku' []				= []
								parseHaiku' (word : rest)	= parseWord word : parseHaiku' rest
								
-- parse a single word in the Haiku Forth vocabulary
parseWord					:: String -> Word
parseWord word				= case (word) of
								"x" 		-> Var "x"
								"y"			-> Var "y"
								"t" 		-> Var "t"
								">r"		-> Stack push
								"push"		-> Stack push
								"r>"		-> Stack pop
								"pop"		-> Stack pop
								"dup"		-> Expr dup
								"over"		-> Expr over
								"2dup"		-> Expr dup2
								"drop"		-> Expr tail
								"swap"		-> Expr swap
								"rot"		-> Expr rot
								"-rot"		-> Expr rot'
								"="			-> Expr eq
								"<>"		-> Expr neq
								"<"			-> Expr lt
								">"			-> Expr gt
								"<="		-> Expr lte
								">="		-> Expr gte
								"and"		-> Expr land
								"or"		-> Expr lor
								"not"		-> Expr lnot
								"min"		-> Expr min'
								"max"		-> Expr max'
								"+"			-> Expr add
								"-"			-> Expr sub
								"*"			-> Expr mul
								"/"			-> Expr div'
								"mod"		-> Expr mod'
								"pow"		-> Expr pow
								"**"		-> Expr pow
								"atan2"		-> Expr atan2'
								"negate"	-> Expr neg
								"sin"		-> Expr sin'
								"cos"		-> Expr cos'
								"tan"		-> Expr tan'
								"log"		-> Expr log'
								"exp"		-> Expr exp'
								"sqrt"		-> Expr sqrt'
								"floor"		-> Expr floor'
								"ceil"		-> Expr ceil'
								"abs"		-> Expr abs'
								"z+"		-> Expr zadd
								"z*"		-> Expr zmul
								"random"	-> Rand
								"pi"		-> Const pi
								"("			-> Cmt "("
								")"			-> Cmt ")"
								":"			-> Func ":"
								";"			-> Func ";"
								_			-> Const $ readDouble word
								
-- comments - remove all comments
-- forth comments are specified with "(", and end with ")"
comments					:: [String] -> [String]
comments []      			= []
comments ("(":ws)			= let (comment, rest) = break ((==) ")") ws
							  in  comments $ tail rest
comments (w:ws)				= w : comments ws

-- routines - expand all defined routines
-- forth routines are specified with ':', and end with ';'
routines					:: [String] -> [String]
routines					= routines' []
							where
								-- if routine declaration found, then add to list of routines
								routines' rs []    	  = []
								routines' rs (":":ws) = let (routine, rest) = break ((==) ";") ws;
															(rh:rb) = routine;
														in
															routines' ((rh, routines' rs rb) : rs) $ tail rest
								routines' rs (w:ws)   = (routines'' rs w) ++ (routines' rs ws)
								
								-- try to match defined routine names, with words in the instruction list
								routines'' []     w = [w]
								routines'' (r:rs) w | fst r == w = snd r
													| otherwise  = routines'' rs w
								
-- parse a string into a number, adjust formatting to match Haskell's Double
readDouble					:: String -> Double
readDouble word				= read $ readDouble' word
							where
								readDouble' ('-':rest)	= "-0" ++ rest
								readDouble' ('.':rest)	= "0." ++ rest
								readDouble' (word)		= word
								
-- returns true if Haiku Forth expression is animated, false if static
isAnimation					:: [Word] -> Bool
isAnimation []				= False
isAnimation (word:rest)		= case (word) of 
								Var "t"		-> True
								Rand		-> True
								_			-> isAnimation rest
								
-- returns true if Haiku Forth expression is animated with random values, false if not
isRandom					:: [Word] -> Bool
isRandom []					= False
isRandom (word:rest)		= case (word) of 
								Rand		-> True
								_			-> isRandom rest
								
-- modify Haiku Forth expression, so that it always returns 4 values on stack
level						:: [Word] -> [Word]
level expr					| length stk < 4	= expr ++ (drop (length stk) (parseHaiku "0.0 0.0 0.0 1.0"))
							| length stk > 4	= expr ++ (concat $ replicate (length stk - 4) (parseHaiku "drop"))
							| otherwise			= expr
							where
								stk = interpretHaiku expr 0.0 0.0 0.0
								
-- push a value from the data stack, onto the return stack
push						:: ([Double], [Double]) -> ([Double], [Double])
push ((d : ds), rs)			= (ds, d : rs)

-- pop a value from the return stack, onto the data stack
pop							:: ([Double], [Double]) -> ([Double], [Double])
pop (ds, (r : rs))			= (r : ds, rs)
								
-- duplicate the element on top of the stack
dup							:: [Double] -> [Double]
dup []						= []
dup (x:xs)					= x:x:xs
	
-- duplicate the second element under the top element of the stack
over						:: [Double] -> [Double]
over []						= []
over [x]					= [x]
over (x:y:xs)				= y:x:y:xs
	
-- duplicates the top two element of the stack
dup2						:: [Double] -> [Double]
dup2 []						= []
dup2 [x]					= [x]
dup2 (x:y:xs)				= x:y:x:y:xs
	
-- swap the top two elements on the stack
swap						:: [Double] -> [Double]
swap []						= []
swap [x]					= [x]
swap (x:y:xs)				= y:x:xs
	
-- rotate the top three elements on the stack
rot							:: [Double] -> [Double]
rot []						= []
rot [x]						= [x]
rot [x, y]					= [y, x]
rot (x:y:z:xs)				= z:x:y:xs

-- reverse rotate the top three elements on the stack
rot'						:: [Double] -> [Double]
rot' []						= []
rot' [x]					= [x]
rot' [x, y]					= [y, x]
rot' (x:y:z:xs)				= y:z:x:xs
	
-- compare the top two elements in the stack, return boolean result
cp							:: (Double -> Double -> Bool) -> [Double] -> [Double]
cp f []						= []
cp f [x]					= [0]
cp f (x:y:xs)				| f y x 	= 1 : xs
							| otherwise = 0 : xs
	
-- returns 1 if the top two stack elements are equal, 0 otherwise
eq							:: [Double] -> [Double]
eq							= cp (==)
							
-- returns 1 if the top two stack elements are not equal, 0 otherwise
neq							:: [Double] -> [Double]
neq							= cp (/=)
	
-- returns 1 if the second element in the stack is less than the top element, 0 otherwise
lt							:: [Double] -> [Double]
lt							= cp (<)
							
-- returns 1 if the second element in the stack is greater than the top element, 0 otherwise
gt							:: [Double] -> [Double]
gt							= cp (>)

-- returns 1 if the second element in the stack is less than or equal to the top element, 0 otherwise
lte							:: [Double] -> [Double]
lte							= cp (<=)

-- returns 1 if the second element in the stack is greater than or equal to the top element, 0 otherwise
gte							:: [Double] -> [Double]
gte							= cp (>=)
	
-- perform binary boolean operation on the top two elements on the stack
binop						:: (Bool -> Bool -> Bool) -> [Double] -> [Double]
binop f []					= []
binop f [x]					= [x]
binop f (x:y:xs)			| (x /= 0) `f` (y /= 0)	= 1 : xs
							| otherwise				= 0 : xs

-- find the logical 'and' of the top elements on the stack
land						:: [Double] -> [Double]
land						= binop (&&)

-- find the logical 'or' of the top two elements on the stack
lor							:: [Double] -> [Double]
lor							= binop (||)
	
-- find the logical 'not' of the top element on the stack
lnot						:: [Double] -> [Double]
lnot []						= []
lnot (x:xs)					| x == 0	= 1 : xs
							| otherwise = 0 : xs

-- perform binary operation on the top two elements on the stack, return result w/ rest of stack
binary						:: (Double -> Double -> Double) -> [Double] -> [Double]
binary f []					= []
binary f [x]				= [x]
binary f (x:y:xs)			= (f y x) : xs
	
-- select the smaller element of the top two elements on the stack
min'						:: [Double] -> [Double]	
min'						= binary min

-- select the larger element of the top two elements on the stack
max'						:: [Double] -> [Double]	
max'						= binary max
	
-- add the top two numbers on the stack
add							:: [Double] -> [Double]
add							= binary (+)

-- subtract the top two numbers on the stack
sub							:: [Double] -> [Double]
sub							= binary (-)

-- multiply the top two numbers on the stack
mul							:: [Double] -> [Double]
mul							= binary (*)

-- divide the top two numbers on the stack
div'						:: [Double] -> [Double]
div'						= binary (/)

-- find the modulus of the top two numbers on the stack, must convert to integer first
mod'						:: [Double] -> [Double]
mod'						= binary (\x y -> if (ceiling y) /= 0 then fromIntegral ((ceiling x) `mod` (ceiling y)) else 0.0)

-- find the exponent of the first two numbers on the stack
pow							:: [Double] -> [Double]
pow							= binary (**)

-- find the arctangent of the ratio of the first two numbers on the stack
atan2'						:: [Double] -> [Double]
atan2'						= binary (\x y -> atan (x / y))

-- perform unary operation on the first element on the stack, return result w/ rest of stack
unary						:: (Floating a, Ord a) => (a -> a) -> [a] -> [a]
unary f []					= []
unary f (x:xs)				= (f x) : xs

-- negate the element on top of the stack
neg							:: [Double] -> [Double]
neg							= unary (\x -> -x)

-- compute the sine of the top element on the stack
sin'						:: [Double] -> [Double]
sin'						= unary sin

-- compute the cosine of the top element on the stack
cos'						:: [Double] -> [Double]
cos'						= unary cos

-- compute the tangent of the top element on the stack
tan'						:: [Double] -> [Double]
tan'						= unary tan

-- compute the log base e of the top element on the stack
log'						:: [Double] -> [Double]
log'						= unary (log . abs)

-- compute the exponent with base e of the top element on the stack
exp'						:: [Double] -> [Double]
exp'						= unary exp

-- compute the sqrt of the top element on the stack
sqrt'						:: [Double] -> [Double]
sqrt'						= unary (sqrt . abs)

-- compute the log base e of the top element on the stack
floor'						:: [Double] -> [Double]
floor'						= unary (fromIntegral . floor)

-- compute the log base e of the top element on the stack
ceil'						:: [Double] -> [Double]
ceil'						= unary (fromIntegral . ceiling)

-- compute the log base e of the top element on the stack
abs'						:: [Double] -> [Double]
abs'						= unary abs

-- perform complex addition on the stack, do nothing if not enough numbers on stack
zadd						:: [Double] -> [Double]
zadd (a:b:c:d:xs)			= (a + c) : (b + d) : xs
zadd dat					= dat

-- perform complex multiplication on the stack, do nothing if not enough numbers on stack
zmul						:: [Double] -> [Double]
zmul (a:b:c:d:xs)			= (d * a + c * b) : (d * b - c * a) : xs
zmul dat					= dat

-- return a pseudo-random number in the range [0, 1)
rand						:: [a] -> Double -> Double -> Double -> [Double] -> [Double] -> [Double]
rand ws x y t dat ret		= r : dat
							where
								-- take a deterministic hash of all the inputs
								r = fracPart $ sum [ (((1 / (fracPart (i / 10) + 0.123)) ** 3.6) * 123.456789) ** 2.4 + 123.456 | 
													 i <- ws_ord ++ [x, y, t] ++ dat ++ ret ]
								
								-- function to get the fractional part of a floating point num
								fracPart = snd . properFraction
								
								-- convert list of instructions into list of ASCII ints
								ws_ord = [0] --concat [ [ ord char | char <- word ] | word <- ws ]
								
								-- function to convert character to ASCII code (floating point)
								ord :: Char -> Double
								ord = fromIntegral . fromEnum

-- interpret Haiku Forth expression, return result on stack, must be parsed + preprocessed beforehand
interpretHaiku 				:: [Word] -> Double -> Double -> Double -> [Double]
interpretHaiku !ws x y t	= interpretHaiku' ws x y t [] []
							where
								-- if no more instructions left to process, then return data stack
								-- use strict evaluation (!) on data and return stack
								interpretHaiku' [] x y t !dat !ret
									= dat
								interpretHaiku' (w:ws) x y t !dat !ret
									= case (w) of 
										-- check if expression first for speed, b/c expressions are the most commonly used
										-- use `seq` to force strict evaluation of the data stack, eliminate thunk accumulation
										Expr expr	-> let dat' = expr dat in dat' `seq` interpretHaiku' ws x y t dat' ret
										Var v		-> case (v) of
													   "x"	-> interpretHaiku' ws x y t (x : dat) ret
													   "y"	-> interpretHaiku' ws x y t (y : dat) ret
													   "t"	-> interpretHaiku' ws x y t (t : dat) ret
										Const c		-> interpretHaiku' ws x y t (c : dat) ret
										Stack stk	-> uncurry (interpretHaiku' ws x y t) (stk (dat, ret))
										Rand		-> interpretHaiku' ws x y t (rand ws x y t dat ret) ret
										_			-> interpretHaiku' ws x y t dat ret
										
-- compose functions to parse and interpret haiku string
parseAndInterpretHaiku		:: String -> Double -> Double -> Double -> [Double]
parseAndInterpretHaiku		= interpretHaiku . parseHaiku