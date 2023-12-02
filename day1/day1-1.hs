-- The calibration consists of lines of text
-- each line originally contained a specific calibration value
-- on each line, the value can be found by combining the first digit and the last digit
-- For example:
-- 1abc2 = 12
-- a1b2c3d4e5f = 15
-- treb7uchet = 77
-- the answer to the puzzle input is the sum of all the calibration values


import Data.Char ( isDigit )


-- We need a function that can handle input and turn it into a usable list
-- it's easier if we put the puzzle input into a file and have it read from that
main = do
    content <- readFile "input1-1.txt"
    print $ answer (lines content)

-- We need a function that can loop over a large number of inputs and sum the outcomes
answer :: [String] -> Integer
answer x = helper x 0

helper :: [String] -> Integer -> Integer
helper [] acc = acc
helper [a] acc = helper [] (decode a 'h' 'o' + acc)
helper (x:xs) acc = helper xs (decode x 'h' 'o' + acc)

-- We need a function that can decode a single string into the 2 digit number
-- if there is nothing left of the string, return the digits found
-- if there are still characters left in the string, check if we found digit 1 yet if not set digit1, always set digit2 if a digit is found
decode :: String -> Char -> Char -> Integer
decode [] digit1 digit2 = read [digit1,digit2] :: Integer
decode (x:xs) digit1 digit2 =   if isDigit x then 
                                    if isDigit digit1 then
                                        decode xs digit1 x
                                    else
                                        decode xs x x
                                else
                                    decode xs digit1 digit2