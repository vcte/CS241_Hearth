module Hearth (parseHaiku, 
			   interpretHaiku, 
			   translateHaiku,
			   translateHaiku',
			   parseAndTranslateHaiku, 
			   isAnimation,
			   isRandom,
			   level)
where

-- ----------------------------------------------------- --
-- --| Hearth - Haskell Interpreter for Haiku Forth |--- --
-- ----------------------------------------------------- --

-- forthsalon.appspot.com

-- type definition of Haiku Forth expression
data Word = Expr ([String] -> [String])								-- data stack altering function
		  | Stack (([String], [String]) -> ([String], [String])) 	-- function affects data and return stacks
		  | Rand													-- random number generator function
		  | Var String 												-- variable, can be "x", "y" or "t"
		  | Const String											-- numerical constant
		  | Func String												-- marks beginning or end of defined function
		  | Cond String												-- marks if / else / then in conditional statement
		  | Cmt String												-- marks beginning or end of comment
		  
-- define print functions for Word types
instance Show (Word) where
	show (Expr _)	= "<expr>"
	show (Stack _)	= "<stack>"
	show (Rand)		= "<random>"
	show (Var s)	= "Var " ++ s
	show (Const d)	= "Const " ++ d
	show (Func s)	= "Func " ++ s
	show (Cond c)	= "Cond " ++ c
	show (Cmt s)	= "Cmt " ++ s

-- parse a Haiku Forth expression, containing multiple words
parseHaiku						:: String -> [Word]
parseHaiku s					= parseHaiku' $ routines $ loops $ comments $ words s
								where
									parseHaiku' []				= []
									parseHaiku' (word : rest)	= parseWord word : parseHaiku' rest
								
-- parse a single word in the Haiku Forth vocabulary
parseWord						:: String -> Word
parseWord word					= case (word) of
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
									"pi"		-> Const "PI"
									"("			-> Cmt "("
									")"			-> Cmt ")"
									"if"		-> Cond "if"
									"else"		-> Cond "else"
									"then"		-> Cond "then"
									"end"		-> Cond "then"
									":"			-> Func ":"
									";"			-> Func ";"
									_			-> Const word
								
-- comments - remove all comments
-- forth comments are specified with "(", and end with ")"
comments						:: [String] -> [String]
comments []      				= []
comments ("(":ws)				= let (comment, rest) = break ((==) ")") ws
								  in  comments $ tail rest
comments (w:ws)					= w : comments ws

-- loops - preprocessing macro to repeat statements
-- forth loops are specified by: [end] [start] do [word_1] .. [word_n] loop
loops							:: [String] -> [String]
loops []						= []
loops (e:s:"do":ws)				= let (stmt, rest) = break ((==) "loop") ws
								  in  (concat [map (\s -> if s /= "i" then s else show i) stmt 
											  | i <- init [read s :: Int .. read e :: Int]]) 
								   ++ (loops $ tail rest)
loops (w:ws)					= w : loops ws

-- routines - expand all defined routines
-- forth routines are specified with ':', and end with ';'
routines						:: [String] -> [String]
routines						= routines' []
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
readDouble						:: String -> Double
readDouble word					= read $ readDouble' word
								where
									readDouble' ('-':rest)	= "-0" ++ rest
									readDouble' ('.':rest)	= "0." ++ rest
									readDouble' (word)		= word
								
-- returns true if Haiku Forth expression is animated, false if static
isAnimation						:: [Word] -> Bool
isAnimation []					= False
isAnimation (word:rest)			= case (word) of 
									Var "t"		-> True
									Rand		-> True
									_			-> isAnimation rest
								
-- returns true if Haiku Forth expression is animated with random values, false if not
isRandom						:: [Word] -> Bool
isRandom []						= False
isRandom (word:rest)			= case (word) of 
									Rand		-> True
									_			-> isRandom rest
								
-- modify Haiku Forth expression, so that it always returns 4 values on stack
level							:: [Word] -> [Word]
level expr						| length stk < 4	= expr ++ (drop (length stk) (parseHaiku "0.0 0.0 0.0 1.0"))
								| length stk > 4	= expr ++ (concat $ replicate (length stk - 4) (parseHaiku "drop"))
								| otherwise			= expr
								where
									stk = interpretHaiku' expr [] []
								
-- push a value from the data stack, onto the return stack
push							:: ([String], [String]) -> ([String], [String])
push ((d : ds), rs)				= (ds, d : rs)

-- pop a value from the return stack, onto the data stack
pop								:: ([String], [String]) -> ([String], [String])
pop (ds, (r : rs))				= (r : ds, rs)
								
-- duplicate the element on top of the stack
dup								:: [String] -> [String]
dup []							= []
dup (x:xs)						= x:x:xs

-- duplicate the second element under the top element of the stack
over							:: [String] -> [String]
over []							= []
over [x]						= [x]
over (x:y:xs)					= y:x:y:xs

-- duplicates the top two element of the stack
dup2							:: [String] -> [String]
dup2 []							= []
dup2 [x]						= [x]
dup2 (x:y:xs)					= x:y:x:y:xs

-- swap the top two elements on the stack
swap							:: [String] -> [String]
swap []							= []
swap [x]						= [x]
swap (x:y:xs)					= y:x:xs

-- rotate the top three elements on the stack
rot								:: [String] -> [String]
rot []							= []
rot [x]							= [x]
rot [x, y]						= [y, x]
rot (x:y:z:xs)					= z:x:y:xs

-- reverse rotate the top three elements on the stack
rot'							:: [String] -> [String]
rot' []							= []
rot' [x]						= [x]
rot' [x, y]						= [y, x]
rot' (x:y:z:xs)					= y:z:x:xs
	
-- compare the top two elements in the stack, return boolean result
cp								:: String -> [String] -> [String]
cp f []							= []
cp f [x]						= ["0"]
cp f (x:y:xs)					= (unwords ["(", y, f, x, ")"]) : xs
								-- ("(" ++ "(" ++ y ++ " " ++ f ++ " " ++ x ++ ")" ++ " ? 1.0 : 0.0" ++ ")") : xs

-- returns 1 if the top two stack elements are equal, 0 otherwise
eq								:: [String] -> [String]
eq								= unary "float" . cp "=="
							
-- returns 1 if the top two stack elements are not equal, 0 otherwise
neq								:: [String] -> [String]
neq								= unary "float" . cp "!="
	
-- returns 1 if the second element in the stack is less than the top element, 0 otherwise
lt								:: [String] -> [String]
lt								= unary "float" . cp "<"
							
-- returns 1 if the second element in the stack is greater than the top element, 0 otherwise
gt								:: [String] -> [String]
gt								= unary "float" . cp ">"

-- returns 1 if the second element in the stack is less than or equal to the top element, 0 otherwise
lte								:: [String] -> [String]
lte								= unary "float" . cp "<="

-- returns 1 if the second element in the stack is greater than or equal to the top element, 0 otherwise
gte								:: [String] -> [String]
gte								= unary "float" . cp ">="

-- perform binary operation on the top two elements on the stack
binop							:: String -> [String] -> [String]
binop f []						= []
binop f [x]						= [x]
binop f (x:y:xs)				= (unwords ["(", y, f, x, ")"]) : xs

-- find the logical 'and' of the top elements on the stack
land							:: [String] -> [String]
land							= unary "float" . binop "&&" . swap . unary "bool" . swap . unary "bool"

-- find the logical 'or' of the top two elements on the stack
lor								:: [String] -> [String]
lor								= unary "float" . binop "||" . swap . unary "bool" . swap . unary "bool"
	
-- find the logical 'not' of the top element on the stack
lnot							:: [String] -> [String]
lnot []							= []
lnot xs 						= unary "not" xs

-- perform binary operation on the top two elements on the stack, return result w/ rest of stack
binary							:: String -> [String] -> [String]
binary f []						= []
binary f [x]					= [x]
binary f (x:y:xs)				= ("(" ++ f ++ "(" ++ y ++ ", " ++ x ++ ")" ++ ")") : xs

-- select the smaller element of the top two elements on the stack
min'							:: [String] -> [String]	
min'							= binary "min"

-- select the larger element of the top two elements on the stack
max'							:: [String] -> [String]	
max'							= binary "max"
	
-- add the top two numbers on the stack
add								:: [String] -> [String]
add								= binop "+"

-- subtract the top two numbers on the stack
sub								:: [String] -> [String]
sub								= binop "-"

-- multiply the top two numbers on the stack
mul								:: [String] -> [String]
mul								= binop "*"

-- divide the top two numbers on the stack
div'							:: [String] -> [String]
div'							= binop "/"

-- find the modulus of the top two numbers on the stack, must convert to integer first
mod'							:: [String] -> [String]
mod'							= binary "mod"

-- find the exponent of the first two numbers on the stack
pow								:: [String] -> [String]
pow								= binary "pow" -- . swap . unary "abs" . swap . unary "abs"

-- find the arctangent of the ratio of the first two numbers on the stack
atan2'							:: [String] -> [String]
atan2'							= binary "atan"

-- perform unary operation on the first element on the stack, return result w/ rest of stack
unary							:: String -> [String] -> [String]
unary f []						= []
unary f (x:xs)					= ("(" ++ f ++ "(" ++ x ++ ")" ++ ")") : xs

-- negate the element on top of the stack
neg								:: [String] -> [String]
neg								= unary "-"

-- compute the sine of the top element on the stack
sin'							:: [String] -> [String]
sin'							= unary "sin"

-- compute the cosine of the top element on the stack
cos'							:: [String] -> [String]
cos'							= unary "cos"

-- compute the tangent of the top element on the stack
tan'							:: [String] -> [String]
tan'							= unary "tan"

-- compute the log base e of the top element on the stack
log'							:: [String] -> [String]
log'							= unary "log" . unary "abs"

-- compute the exponent with base e of the top element on the stack
exp'							:: [String] -> [String]
exp'							= unary "exp"

-- compute the sqrt of the top element on the stack
sqrt'							:: [String] -> [String]
sqrt'							= unary "sqrt" . unary "abs"

-- compute the log base e of the top element on the stack
floor'							:: [String] -> [String]
floor'							= unary "floor"

-- compute the rounded up value of the top element on the stack
ceil'							:: [String] -> [String]
ceil'							= unary "ceil"

-- compute the absolute value of the top element on the stack
abs'							:: [String] -> [String]
abs'							= unary "abs"

-- perform complex addition on the stack, do nothing if not enough numbers on stack
zadd							:: [String] -> [String]
zadd (a:b:c:d:xs)				= (unwords ["(", a, "+", c, ")"]) : (unwords ["(", b, "+", d, ")"]) : xs
zadd dat						= dat

-- perform complex multiplication on the stack, do nothing if not enough numbers on stack
zmul							:: [String] -> [String]
zmul (a:b:c:d:xs)				= (unwords ["(", d, "*", a, "+", c, "*", b, ")"]) : (unwords ["(", d, "*", b, "-", c, "*", a, ")"]) : xs
zmul dat						= dat

-- glsl code to return a pseudo-random number in the range [0, 1)
rand							:: String
rand							= "(seed = fract(sin(104053.0*seed+mod(time_val, 100003.0)+101869.0*tpos.x+102533.0*tpos.y)*103723.0))"

-- translate Haiku Forth expression (w/ temp variables), return result as GLSL code, must be parsed + preprocessed beforehand
translateHaiku 					:: [Word] -> String
translateHaiku ws				= header ++ code ++ footer
								where
									(s, dat)  = interpretHaiku "" ws 1 [] [] 
									code = s ++ translateRGBA dat
									
-- translate Haiku Forth expression (w/ no temp vars), return result as GLSL code, must be parsed + preprocessed beforehand
translateHaiku' 				:: [Word] -> String
translateHaiku' ws				= header ++ code ++ footer
								where
									dat  = interpretHaiku' ws [] []
									code = translateRGBA dat
									
-- unpacks RGB and transparency values from data, and converts into GLSL code string
translateRGBA					:: [String] -> String
translateRGBA (alpha : colors)	= "color = vec3(" ++ join ", " (appendToAll (" * " ++ alpha) $ reverse colors) ++ ");"
									
-- header and footer for GLSL code
header = unlines 
			[ "#version 330 core"
			, "layout(location = 0) in vec3 vPosition_modelspace;"
			, "varying highp vec2 tpos;"
			, "uniform float time_val;"
			, "float seed;"
			, "float PI = 3.1415926535897931;"
			, "out vec3 color;"
			, "void main()"
			, "{"
			]
footer = "\n}"

-- string utility functions, taken from Haskell String library
join sep lst						= concat $ intersperse sep lst
intersperse sep (l:lst)				= l : prependToAll sep lst
prependToAll sep lst				= map ((++) sep) lst
appendToAll  sep lst				= map (flip (++) sep) lst
								
-- interpret a conditional
interpretCond						:: ([Word], [String], [String]) -> ([Word], [String])
interpretCond (ws, (d:dat), ret)= let (ws1, (_:ws2)) = break (\x -> case (x) of Cond "else" -> True; _ -> False) ws;
									  (ws3, (_:ws4)) = break (\x -> case (x) of Cond "then" -> True; _ -> False) ws2;
									  wst' = interpretHaiku' ws1 dat ret;
									  wsf' = interpretHaiku' ws3 dat ret;
								  in  (ws4, map (\(wt, wf) -> "(" ++ d ++ " != 0.0 ? (" ++ wt ++ ") : (" ++ wf ++ "))") 
										  $ zip wst' wsf')
								
-- interpret haiku forth code, return chain of glsl computations as string, and data stack w/ temporary variables
interpretHaiku						:: String -> [Word] -> Int -> [String] -> [String] -> (String, [String])
interpretHaiku s [] n dat ret		= (s, dat)
interpretHaiku s (w:ws) n dat ret	= case (w) of
									Expr expr	-> let (datNew, datOld) = span (any (\c -> c == ' ' || c == '(')) $ expr dat;
													   ns = [n .. n + (length datNew) - 1];
													   datNew' = ["temp" ++ show n' | n' <- ns];
												   in  interpretHaiku (s ++ (concat $ map (\(d, n') -> temp n' d) $ zip datNew ns))
													   ws (n + length datNew) (datNew' ++ datOld) ret
									Var v		-> case (v) of 
												   "x"	->  interpretHaiku (s ++ temp n "tpos.x") ws (n + 1) (("temp" ++ show n) : dat) ret
												   "y"	-> 	interpretHaiku (s ++ temp n "tpos.y") ws (n + 1) (("temp" ++ show n) : dat) ret
												   "t"	->	interpretHaiku (s ++ temp n "time_val + 10000") ws (n + 1) (("temp" ++ show n) : dat) ret
									Const c		-> interpretHaiku (s ++ temp n c) ws (n + 1) (("temp" ++ show n) : dat) ret
									Stack stk	-> uncurry (interpretHaiku s ws n) (stk (dat, ret))
									Rand		-> interpretHaiku (s ++ temp n rand) ws (n + 1) (("temp" ++ show n) : dat) ret
									_			-> interpretHaiku s ws n dat ret
									where
										-- produce glsl code to create a new temporary variable, with name temp[n]
										temp n c = "float temp" ++ show n ++ " = " ++ c ++ ";\n"
								
-- interpret haiku forth code, return data stack containing GLSL code strings
interpretHaiku'						:: [Word] -> [String] -> [String] -> [String]
interpretHaiku' [] dat ret			= dat							-- if no more instructions left to process, then return data stack
interpretHaiku' (w:ws) dat ret		= case (w) of
										-- check if expression first, b/c expressions are the most commonly used
										Expr expr	-> interpretHaiku' ws (expr dat) ret
										Var v		-> case (v) of
													   "x"	-> interpretHaiku' ws ("tpos.x" : dat) ret
													   "y"	-> interpretHaiku' ws ("tpos.y" : dat) ret
													   "t"	-> interpretHaiku' ws ("(time_val + 10000)" : dat) ret
										Const c		-> interpretHaiku' ws (c : dat) ret
										Stack stk	-> uncurry (interpretHaiku' ws) (stk (dat, ret))
										Cond "if"	-> let (ws', dat') = interpretCond (ws, dat, ret)
													   in  interpretHaiku' ws' dat' ret
										Rand		-> interpretHaiku' ws (rand : dat) ret
										_			-> interpretHaiku' ws dat ret

-- compose functions to parse and translate haiku string
parseAndTranslateHaiku				:: String -> String
parseAndTranslateHaiku				= translateHaiku . level . parseHaiku