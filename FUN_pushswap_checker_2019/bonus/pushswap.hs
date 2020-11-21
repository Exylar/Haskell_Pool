import System.Environment
import System.Exit
import Text.Read
import Data.List

main :: IO ()
main = do
    list <- getArgs
    check_list_has_num list
    let num_list = get_list_num list
    let nb_element = length num_list
    isSorted num_list
    doAction num_list
    printElement (nb_element - 2) "pa "
    putStrLn "pa"
    return ()

printElement :: Int -> String -> IO ()
printElement 0 element = return ()
printElement n element = do
  putStr element
  printElement (n-1) element

raAction :: Int -> [Int] -> [Int]
raAction 0 list = (tail list)
raAction n list = do
    raAction (n-1) ((tail list) ++ [head list])

rraAction :: Int -> [Int] -> [Int]
rraAction 0 list = (tail list)
rraAction n list = do
    rraAction (n-1) ([last list] ++ (init list))

check_list_has_num :: [String] -> IO ()
check_list_has_num [] = return ()
check_list_has_num (x:xs)
    | (readMaybe x :: Maybe Int) /= Nothing = check_list_has_num xs
    | otherwise = putStrLn "KO" >> exitWith (ExitFailure 84)

get_list_num :: [String] -> [Int]
get_list_num [] = []
get_list_num (x:xs)
    | (readMaybe x :: Maybe Int) /= Nothing = (read x :: Int) : get_list_num xs
    | otherwise = []

isSorted :: [Int] -> IO ()
isSorted [] = return ()
isSorted [x] = putStrLn "" >> exitWith (ExitSuccess)
isSorted (x:xs)
    | x <= (head xs) = isSorted xs
    | otherwise = return ()

getSmallessNumber :: [Int] -> Int -> Int -> Int -> Int
getSmallessNumber [] _ pos _ = pos
getSmallessNumber [x] _ pos _ = pos
getSmallessNumber (x:xs) ele pos pos_actual
    | ele < (head xs) = getSmallessNumber xs ele pos (pos_actual + 1)
    | otherwise = getSmallessNumber xs (head xs) pos_actual (pos_actual + 1)

doAction :: [Int] -> IO ()
doAction [x] = return ()
doAction list
    | (getSmallessNumber list (head list) 0 1) >= length list `div` 2  = printElement (length list - (getSmallessNumber list (head list) 0 1)) "rra " >> putStr "pb " >> doAction (rraAction (length list - (getSmallessNumber list (head list) 0 1)) list)
    | otherwise = printElement (length list - (getSmallessNumber list (head list) 0 1)) "ra " >> putStr "pb " >> doAction (raAction (getSmallessNumber list (head list) 0 1) list)