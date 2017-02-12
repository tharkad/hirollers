import System.Random
import Data.Map

type Board = (Int, [Int], Int)  -- roll, board, doubles
newBoard = (0, [1..9], 0) :: Board
dn = 6

setRoll :: Int -> Board -> Board
setRoll r (_,b,d) = (r,b,d)

setDoubles :: Int -> Board -> Board
setDoubles d (r,b,_) = (r,b,d)

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = [x:sublist | sublist <- sublists xs] ++ sublists xs

possibleMoves :: Board -> [[Int]]
possibleMoves (_,[],_) = []
possibleMoves (0,_,_) = []
possibleMoves (r,b,_) = [xs | xs <- sublists ([y | y <- b, y <= r]), sum xs == r]

playGame :: Board -> IO ()
playGame (r,[],d) = do
    putStrLn "You Won!"
    putStrLn "Press Enter to Start New Game:"
    response <- getLine
    playGame newBoard

playGame b@(r,xs,d) = do
    putStrLn ""
    if r == 0
    then do
        putStrLn "Rolling!"
        g <- getStdGen
        d1 <- randomRIO (1,dn::Int)
        d2 <- randomRIO (1,dn::Int)
        print d1
        print d2
        playGame (setRoll (d1+d2) (setDoubles (if (d1==d2) then (d+1) else d) b))
    else do
        if length (possibleMoves b) == 0
        then do
            if d == 0
            then do
                print b
                putStrLn "You Lost!"
                putStrLn "Press Enter to Start New Game:"   
                response <- getLine
                playGame newBoard
            else do
                putStrLn "Can't Move - Rolling!"
                g <- getStdGen
                d1 <- randomRIO (1,dn::Int)
                d2 <- randomRIO (1,dn::Int)
                print d1
                print d2
                playGame (setRoll (d1+d2) (setDoubles (if (d1==d2) then d else (d-1)) b))
        else do
            print b
            putStrLn "choose your move:"
            putStr $ showChoice allowedMoves
            userInput <- getLine 
            let choice = read userInput :: Int
            case Data.Map.lookup choice mapOfMoves of
                Just _ -> 
                    playGame (0, [x | x <- xs, not (x `elem` (possibleMoves b)!!(choice - 1))], d)
                Nothing -> do
                    putStrLn "Invalid choice. Choose again."
                    playGame b
            where
                allowedMoves :: [(Int, [Int])]
                allowedMoves = zip [1..] (possibleMoves b)
                showChoice :: [(Int, [Int])] -> String
                showChoice choices = unlines [show num ++ ": " ++ show move | (num, move) <- choices]
                mapOfMoves :: Data.Map.Map Int [Int]
                mapOfMoves = Data.Map.fromList allowedMoves

main = do
    playGame newBoard
