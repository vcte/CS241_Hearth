-- ----------------------------------------------------- --
-- --| Hearth - Haskell Interpreter for Haiku Forth |--- --
-- ----------------------------------------------------- --

import Test.Hspec

import Hearth

-- determines whether floating-point contents of two lists are approximately equal
approx						:: [Double] -> [Double] -> Bool
approx [] _					= True
approx _ []					= True
approx (x:xs) (y:ys)		= abs(x - y) < 0.0000001 && approx xs ys

main :: IO ()
main = hspec $ do
	describe "+" $ do
		it "should add two numbers" $ do
			parseAndInterpretHaiku "1 2 + " 0 0 0 `shouldBe` [3]
			parseAndInterpretHaiku "1234 5678 + " 0 0 0 `shouldBe` [6912]
			parseAndInterpretHaiku "-2 3 + " 0 0 0 `shouldBe` [1]
			
	describe "-" $ do
		it "should subtrace two numbers" $ do
			parseAndInterpretHaiku "7 4 - " 0 0 0 `shouldBe` [3]
			parseAndInterpretHaiku "4 7 - " 0 0 0 `shouldBe` [-3]
	
	describe "*" $ do
		it "should multiply two numbers" $ do
			parseAndInterpretHaiku "7 4 * " 0 0 0 `shouldBe` [28]
			parseAndInterpretHaiku "7 -4 * " 0 0 0 `shouldBe` [-28]
			parseAndInterpretHaiku "7 0.5 * " 0 0 0 `shouldBe` [3.5]
			
	describe "/" $ do
		it "should divide two numbers" $ do
			parseAndInterpretHaiku "8 4 / " 0 0 0 `shouldBe` [2]
			parseAndInterpretHaiku "3 2 / " 0 0 0 `shouldBe` [1.5]
			
	describe "=" $ do
		it "should return whether two numbers are equal" $ do
			parseAndInterpretHaiku "1 2 = " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "3 3 = " 0 0 0 `shouldBe` [1]
			
	describe "<>" $ do
		it "should return whether two numbers are not equal" $ do
			parseAndInterpretHaiku "1 2 <> " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "3 3 <> " 0 0 0 `shouldBe` [0]
			
	describe "<" $ do
		it "should return whether one number is less than another" $ do
			parseAndInterpretHaiku "2 1 < " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "3 3 < " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "4 -3 < " 0 0 0 `shouldBe` [1]
			
	describe ">" $ do
		it "should return whether one number is greater than another" $ do
			parseAndInterpretHaiku "1 2 > " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "3 3 > " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "4 -3 > " 0 0 0 `shouldBe` [0]
			
	describe "<=" $ do
		it "should return whether one number is less than or equal to another" $ do
			parseAndInterpretHaiku "1 2 <= " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "3 3 <= " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "4 -3 <= " 0 0 0 `shouldBe` [1]
			
	describe ">=" $ do
		it "should return whether one number is greater than or equal to another" $ do
			parseAndInterpretHaiku "1 2 >= " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "3 3 >= " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "4 -3 >= " 0 0 0 `shouldBe` [0]
			
	describe "and" $ do
		it "should find the logical and between the top two elements on the stack" $ do
			parseAndInterpretHaiku "0 0 and " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "0 1 and " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "1 0 and " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "1 1 and " 0 0 0 `shouldBe` [1]
			
	describe "or" $ do
		it "should find the logical or between the top two elements on the stack" $ do
			parseAndInterpretHaiku "0 0 or " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "0 1 or " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "1 0 or " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "1 1 or " 0 0 0 `shouldBe` [1]
			
	describe "not" $ do
		it "should find the logical not of the first element on the stack" $ do
			parseAndInterpretHaiku "0 not " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "1 not " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "0 not not " 0 0 0 `shouldBe` [0]
			
	describe "min" $ do
		it "should select the smaller of the top two elements on the stack" $ do
			parseAndInterpretHaiku "2 3 min " 0 0 0 `shouldBe` [2]
			parseAndInterpretHaiku "2 -3 min " 0 0 0 `shouldBe` [-3]
			
	describe "max" $ do
		it "should select the larger of the top two elements on the stack" $ do
			parseAndInterpretHaiku "2 3 max " 0 0 0 `shouldBe` [3]
			parseAndInterpretHaiku "2 -3 max " 0 0 0 `shouldBe` [2]
			
	describe "mod" $ do
		it "should find the modulus of the top two elements on the stack" $ do
			parseAndInterpretHaiku "9 4 mod " 0 0 0 `shouldBe` [1]
			
	describe "pow" $ do
		it "should find the exponent of the top two elements on the stack" $ do
			parseAndInterpretHaiku "2 3 pow " 0 0 0 `shouldBe` [8]
			parseAndInterpretHaiku "2 3 ** " 0 0 0 `shouldBe` [8]
			
	describe "atan2" $ do
		it "should find the arctangent of the ratio of the top two elements on the stack" $ do
			parseAndInterpretHaiku "0 1 atan2 " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "1 0 atan2 " 0 0 0 `shouldBe` parseAndInterpretHaiku "pi 2 / " 0 0 0
			
	describe "negate" $ do
		it "should negate the first element on the stack" $ do
			parseAndInterpretHaiku "2 negate " 0 0 0 `shouldBe` [-2]
			parseAndInterpretHaiku "0 negate " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "-4 negate " 0 0 0 `shouldBe` [4]
			
	describe "sin" $ do
		it "should calculate the sine of the top element on the stack" $ do
			parseAndInterpretHaiku "0 sin " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "pi 2 / sin " 0 0 0 `shouldSatisfy` approx [1]
			
	describe "cos" $ do
		it "should calculate the cosine of the top element on the stack" $ do
			parseAndInterpretHaiku "0 cos " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "pi 2 / cos " 0 0 0 `shouldSatisfy` approx [0]
			
	describe "tan" $ do
		it "should calculate the tangent of the top element on the stack" $ do
			parseAndInterpretHaiku "0 tan " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "pi tan " 0 0 0 `shouldSatisfy` approx [0]
			
	describe "log" $ do
		it "should calculate the log base e of the top element on the stack" $ do
			parseAndInterpretHaiku "1 log " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "2 log exp " 0 0 0 `shouldBe` [2]
			
	describe "exp" $ do
		it "should compute the exponent of the first element on the stack" $ do
			parseAndInterpretHaiku "0 exp " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "2 exp log " 0 0 0 `shouldBe` [2]
			
	describe "sqrt" $ do
		it "should compute the square root of the first element on the stack" $ do
			parseAndInterpretHaiku "4 sqrt " 0 0 0 `shouldBe` [2]
			parseAndInterpretHaiku "1 sqrt " 0 0 0 `shouldBe` [1]
			
	describe "floor" $ do
		it "should round the number on top of the stack downwards" $ do
			parseAndInterpretHaiku "1.234 floor " 0 0 0 `shouldBe` [1]
			parseAndInterpretHaiku "-1.234 floor " 0 0 0 `shouldBe` [-2]
			
	describe "ceil" $ do
		it "should round the number on top of the stack upwards" $ do
			parseAndInterpretHaiku "1.234 ceil " 0 0 0 `shouldBe` [2]
			parseAndInterpretHaiku "-1.234 ceil " 0 0 0 `shouldBe` [-1]
			
	describe "abs" $ do
		it "should compute the absolute value of the first number on the stack" $ do
			parseAndInterpretHaiku "-123 abs " 0 0 0 `shouldBe` [123]
			parseAndInterpretHaiku "-1.2 abs" 0 0 0 `shouldBe` [1.2]
			
	describe "pi" $ do
		it "should push pi onto the stack" $ do
			parseAndInterpretHaiku "pi " 0 0 0 `shouldBe` [3.1415926535897931]
			
	describe "zadd" $ do
		it "should perform complex addition on the top two elements on the stack" $ do
			parseAndInterpretHaiku "1 11 5 9 z+ " 0 0 0 `shouldBe` [20, 6]
			
	describe "zmul" $ do
		it "should perform complex multiplication on the top two elements on the stack" $ do
			parseAndInterpretHaiku "1 11 5 9 z* " 0 0 0 `shouldBe` [64, -94]
			parseAndInterpretHaiku "2 3 2dup z* " 0 0 0 `shouldBe` [12, -5]
			
	describe "x" $ do
		it "should push the current x-coordinate onto the stack" $ do
			parseAndInterpretHaiku "x " 1 2 3 `shouldBe` [1]
			parseAndInterpretHaiku "x 2 + " 1 2 3 `shouldBe` [3]
			
	describe "y" $ do
		it "should push the current y-coordinate onto the stack" $ do
			parseAndInterpretHaiku "y " 1 2 3 `shouldBe` [2]
			parseAndInterpretHaiku "y x + " 1 2 3 `shouldBe` [3]
			parseAndInterpretHaiku "y x * " 1 2 3 `shouldBe` [2]
			
	describe "t" $ do
		it "should push the current time in seconds onto the stack" $ do
			parseAndInterpretHaiku "t " 1 2 3 `shouldBe` [3]
			parseAndInterpretHaiku "x y t + + " 1 2 3 `shouldBe` [6]
			parseAndInterpretHaiku "x y t * * " 1 2 3 `shouldBe` [6]
			
	describe "push / pop" $ do
		it "should move one value from the data stack to the return stack, and vice versa" $ do
			parseAndInterpretHaiku "1 2 3 push + pop * " 0 0 0 `shouldBe` [9]
			parseAndInterpretHaiku "4 2 2dup push push * pop pop / - " 0 0 0 `shouldBe` [6]
			
	describe "dup" $ do
		it "should duplicate the element on top of the stack" $ do
			parseAndInterpretHaiku "1 2 3 dup " 0 0 0 `shouldBe` [3, 3, 2, 1]
			parseAndInterpretHaiku "4 dup dup " 0 0 0 `shouldBe` [4, 4, 4]
			
	describe "over" $ do
		it "should duplicate the element under the top stack element" $ do
			parseAndInterpretHaiku "1 2 3 over " 0 0 0 `shouldBe` [2, 3, 2, 1]
			
	describe "2dup" $ do
		it "should duplicate the top two elements on the stack" $ do
			parseAndInterpretHaiku "1 2 3 2dup " 0 0 0 `shouldBe` [3, 2, 3, 2, 1]
			
	describe "drop" $ do
		it "should drop the top element from the stack" $ do
			parseAndInterpretHaiku "1 2 3 drop " 0 0 0 `shouldBe` [2, 1]
			
	describe "swap" $ do
		it "should swap the top two elements on the stack" $ do
			parseAndInterpretHaiku "1 2 3 swap " 0 0 0 `shouldBe` parseAndInterpretHaiku "1 3 2" 0 0 0
			
	describe "rot" $ do
		it "should rotate the top three elements on the stack" $ do
			parseAndInterpretHaiku "1 2 3 rot " 0 0 0 `shouldBe` parseAndInterpretHaiku "2 3 1" 0 0 0
			
	describe "-rot" $ do
		it "should reverse rotate the top three elements on the stack" $ do
			parseAndInterpretHaiku "1 2 3 -rot " 0 0 0 `shouldBe` parseAndInterpretHaiku "3 1 2" 0 0 0
			
	describe ":" $ do
		it "should allow the programmer to define a new forth word" $ do
			parseAndInterpretHaiku ": square dup * ; 4 square " 0 0 0 `shouldBe` [16]
			
		it "should allow for nested routine definitions" $ do
			parseAndInterpretHaiku ": square dup * ; : pythogorean square swap square + sqrt ; 3 4 pythogorean "
				0 0 0 `shouldBe` [5]
				
		it "should be able to override core words" $ do
			parseAndInterpretHaiku ": 2dup over over ; 1 2 3 2dup " 0 0 0 `shouldBe` parseAndInterpretHaiku "1 2 3 2dup " 0 0 0
			
	describe "comments" $ do
		it "should remove all comments before parsing the code" $ do
			parseAndInterpretHaiku ": square ( n -- n ) dup * ; 4 square " 0 0 0 `shouldBe` [16]
			
	describe "parseAndInterpretHaiku" $ do
		it "should be able to execute compound expressions" $ do
			-- expression taken from the 'minimal animation' haiku
			parseAndInterpretHaiku "pi 2 / sin 2 / 0.5 + " 0 0 0 `shouldBe` [1]
			
			-- formula to find distance between the points (0, 0) and (3, 4)
			parseAndInterpretHaiku "0 3 0 4 - abs 2 ** push - abs 2 ** pop + sqrt " 0 0 0 `shouldBe` [5]
			
			-- evaluate logical expressions
			parseAndInterpretHaiku "1 0 <> 1 0 > and not " 0 0 0 `shouldBe` [0]
			parseAndInterpretHaiku "1 0 <> not 1 0 > not or " 0 0 0 `shouldBe` [0]
			
			-- do trigonometry
			parseAndInterpretHaiku "pi 2 / sin pi 2 / cos atan2 sin " 0 0 0 `shouldBe` [1]
			