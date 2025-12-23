module Commons where

-- does what it says, if the string is invalid it returns 0
stringToInteger :: String -> Integer
stringToInteger "" = 0
stringToInteger input = stringToIntegerAcc input 0

-- eats up digit by digit
stringToIntegerAcc :: String -> Integer -> Integer
stringToIntegerAcc "" number = number
stringToIntegerAcc input number = let firstCharacter:rest = input in
    case firstCharacter of
        '0' -> stringToIntegerAcc rest (10 * number)
        '1' -> stringToIntegerAcc rest (10*number + 1)
        '2' -> stringToIntegerAcc rest (10*number + 2)
        '3' -> stringToIntegerAcc rest (10*number + 3)
        '4' -> stringToIntegerAcc rest (10*number + 4)
        '5' -> stringToIntegerAcc rest (10*number + 5)
        '6' -> stringToIntegerAcc rest (10*number + 6)
        '7' -> stringToIntegerAcc rest (10*number + 7)
        '8' -> stringToIntegerAcc rest (10*number + 8)
        '9' -> stringToIntegerAcc rest (10*number + 9)
        _ -> 0