import Control.Monad.Cont (cont)
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

-- takes current pointer location, then a command, then returns new pointer location, at least 0 and at most 99
rotatePointer :: Integer -> String -> Integer
rotatePointer pointingLocation "" = pointingLocation
rotatePointer pointingLocation command = let direction:amount = command in
    case direction of
        'R' -> mod (pointingLocation + stringToInteger amount) 100
        'L' -> mod (pointingLocation - stringToInteger amount) 100

-- takes the list of commands, returns number of moments where pointed to 0
getZeroPointingMoments :: [String] -> Integer
getZeroPointingMoments commands = getZeroPointingMomentsAcc commands 50

-- accumulates with the current number pointing to
getZeroPointingMomentsAcc :: [String] -> Integer -> Integer
getZeroPointingMomentsAcc commands currentPointerLocation
    | commands == [] = 0 -- if we are pointing at 0, we have already accounted for it in the last call (when we processed the last command. we can ignore the empty command set because it starts at 50 and not 0)
    | otherwise = let nextCommand:otherCommands = commands in
        let newPointerLocation = rotatePointer currentPointerLocation nextCommand in
            case newPointerLocation of
                0 -> 1 + getZeroPointingMomentsAcc otherCommands newPointerLocation
                _ -> getZeroPointingMomentsAcc otherCommands newPointerLocation

main = do
    putStrLn "starting program"
    contents <- readFile "input.txt"
    let contentsLines = lines contents
    print (stringToInteger "042a3")
    print (rotatePointer 50 "R49")
    print(getZeroPointingMoments contentsLines)
    putStrLn "finished"