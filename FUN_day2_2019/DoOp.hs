import System.Environment
import System.Exit
import Data.Char
import Text.Read

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem element (x:xs)
    | element == x = True
    | otherwise = myElem element xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv num1 num2
    | num2 == 0 = Nothing
    | otherwise = Just (num1 `div` num2)

safeNth :: [a] -> Int -> Maybe a
safeNth [] _ = Nothing
safeNth (x:xs) n
    | n == 0 = Just x
    | otherwise = safeNth xs (n-1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc (Just number) = Just (number + 1)
safeSucc Nothing = Nothing

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup element ((i,j):xs)
    | element == i = Just j
    | otherwise = myLookup element xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo fnc (Just ele1) (Just ele2) = Just (fnc ele1 ele2)
maybeDo fnc ele1 ele2 = Nothing

readInt :: String -> Maybe Int
readInt "" = Nothing
readInt string
    | (readMaybe string :: Maybe Int) /= Nothing = Just (read  string :: Int)
    | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do
    line <- getLine
    return (length line)

printAndGetLength :: String -> IO Int
printAndGetLength string = do
    putStr string
    putStr "\n"
    return (length string)

printLine :: Int -> IO ()
printLine 0 = putStr "+\n"
printLine n = do
    putStr "-"
    printLine (n-1)

printSpace :: Int -> IO ()
printSpace 0 = putStr "|\n"
printSpace n = do
    putStr " "
    printSpace (n-1)

printNSpace :: Int -> Int -> IO ()
printNSpace 0 _ = return ()
printNSpace repeat n = do
    putStr "|"
    printSpace n
    printNSpace (repeat-1) n

printBox :: Int -> IO ()
printBox size
    | size < 1 = return ()
    | size == 1 = putStr "++\n"
    | otherwise = do
        putStr "+"
        printLine (size * 2 - 2)
        printNSpace (size - 2) (size * 2 - 2)
        putStr "+"
        printLine (size * 2 - 2)

concatLines :: Int -> IO String
concatLines number_line
    | number_line == 0 = return []
    | otherwise = do
        line <- getLine
        rest <- concatLines (number_line - 1)
        return (line ++ rest)

getInt :: IO (Maybe Int)
getInt = do
    line <- getLine
    return (readInt (line :: String))

doOp :: Int -> [Char] -> Int -> IO ()
doOp num1 operator num2
    | operator == "+" = print (num1 + num2)
    | operator == "-" = print (num1 - num2)
    | operator == "*" = print (num1 * num2)
    | operator == "/" = print (num1 `div` num2)
    | operator == "%" = print (num1 `mod` num2)
    | otherwise = exitWith (ExitFailure 84)

main = do
    args <- getArgs
    let arg1 = args !! 0
    let arg2 = args !! 1
    let arg3 = args !! 2
    doOp (read arg1 :: Int) (arg2) (read arg3 ::Int)