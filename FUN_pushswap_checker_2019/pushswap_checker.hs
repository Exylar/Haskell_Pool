import System.Environment
import System.Exit
import Text.Read
import Data.List

main :: IO ()
main = do
    action <- getLine
    let list_action = operation (groupBy (\a b -> b /= ' ') action)
    list_a <- check_arg
    let list_b = []
    doAction list_action list_a list_b
    return ()

operation :: [String] -> [String]
operation [] = []
operation (x:xs)
    | x == "sa" || x == " sa" = x : operation xs
    | x == "sb" || x == " sb" = x : operation xs
    | x == "sc" || x == " sc" = x : operation xs
    | x == "pa" || x == " pa" = x : operation xs
    | x == "pb" || x == " pb" = x : operation xs
    | x == "ra" || x == " ra" = x : operation xs
    | x == "rb" || x == " rb" = x : operation xs
    | x == "rr" || x == " rr" = x : operation xs
    | x == "rra" || x == " rra" = x : operation xs
    | x == "rrb" || x == " rrb" = x : operation xs
    | x == "rrr" || x == " rrr" = x : operation xs
    | otherwise = return []

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

isSorted :: [Int] -> [Int] -> IO ()
isSorted _ [x] = putStrLn "KO" >> exitWith (ExitFailure 84)
isSorted [] _ = putStrLn "KO" >> exitWith (ExitFailure 84)
isSorted [x] [] = return ()
isSorted (x:xs) list_b
    | x <= (head xs) = isSorted xs list_b
    | otherwise = putStrLn "KO" >> exitWith (ExitFailure 84)

check_arg :: IO [Int]
check_arg = do
    args <- getArgs
    check_list_has_num args
    return (get_list_num args)

swapElement :: Int -> Int -> [Int] -> [Int]
swapElement i j list
    | length list <= 1 = list
    | otherwise = do
        let elemI = list !! i
        let elemJ = list !! j
        let left = take i list
        let middle = take (j - i - 1) (drop (i + 1) list)
        let right = drop (j + 1) list
        left ++ [elemJ] ++ middle ++ [elemI] ++ right

pushHeadElement :: [Int] -> [Int] -> [Int]
pushHeadElement list_head list_to_concate
    | length list_head == 0 = list_to_concate
    | otherwise = [head list_head] ++ list_to_concate

pushTailElement :: [Int] -> [Int]
pushTailElement list
    | length list == 0 = list
    | otherwise = (tail list)

doAction :: [String] -> [Int] -> [Int] -> IO ()
doAction [] list_a list_b = isSorted list_a list_b >> putStrLn "OK"
doAction (x:xs) list_a list_b
    | x == "sa" || x == " sa" = doAction xs (swapElement 0 1 list_a) list_b
    | x == "sb" || x == " sb" = doAction xs list_a (swapElement 0 1 list_b)
    | x == "sc" || x == " sc" = doAction xs (swapElement 0 1 list_a) (swapElement 0 1 list_b)
    | x == "pa" || x == " pa" = doAction xs (pushHeadElement list_b list_a) (pushTailElement list_b)
    | x == "pb" || x == " pb" = doAction xs (pushTailElement list_a) (pushHeadElement list_a list_b)
    | x == "ra" || x == " ra" = doAction xs ((tail list_a) ++ [head list_a]) list_b
    | x == "rb" || x == " rb" = doAction xs (list_a) ((tail list_b) ++ [head list_b])
    | x == "rr" || x == " rr" = doAction xs ((tail list_a) ++ [head list_a])  ((tail list_b) ++ [head list_b])
    | x == "rra" || x == " rra" = doAction xs ([last list_a] ++ (init list_a)) list_b
    | x == "rrb" || x == " rrb" = doAction xs (list_a) ([last list_b] ++ (tail list_b))
    | x == "rrr" || x == " rrr" = doAction xs ([last list_a] ++ (init list_a)) ([last list_b] ++ (tail list_b))
    | otherwise = putStrLn "KO" >> exitWith (ExitFailure 84)