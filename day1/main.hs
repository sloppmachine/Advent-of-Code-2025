import Commons

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

-- 1 if you touch or rotate past 0. takes current pointer location, then a command
doesRotationTouchZero :: Integer -> String -> Integer
doesRotationTouchZero pointing


main = do
    putStrLn "starting program"
    contents <- readFile "day1/input.txt" -- relative to the main directory
    let contentsLines = lines contents
    print (stringToInteger "042a3")
    print (rotatePointer 50 "R49")
    print(getZeroPointingMoments contentsLines)
    putStrLn "finished"