import System.Random
import Data.Map
import Data.Array

data BigBoard = BigBoard {
                    boardSize :: Int,
                    boardDepth :: Int,
                    boardState :: Array (Int, Int) Int
}

instance Show BigBoard where
    show (BigBoard size depth arr) = unlines [unwords [(showBoardNumber (arr Data.Array.! (x, y)) 3) | y <- [1..size]] | x <- [1..depth]]

data Die = Die {
            dieSize :: Int,
            dieValue :: Int
}

instance Show Die where
    show (Die dSize dValue) = show dValue

data Game = Game {
                numDice :: Int,
                dice :: [Die],
                bigBoard :: BigBoard,
                doublesTokens :: Int,
                totalRolls :: Int
}

instance Show Game where
    show (Game nDice dice board doubles rolls) = "\n*****\n" ++ show board ++ "\n" ++ "Roll: " ++ (unwords $ (Prelude.map show dice)) ++ "\nDoubles: " ++ show doubles ++ "\nTotal Rolls: " ++ show rolls ++ "\n"

unwordsNoSpaces :: [String] -> String
unwordsNoSpaces [] = []
unwordsNoSpaces (x:xs) = x ++ unwordsNoSpaces xs

boardPattern :: Int -> [Int]
boardPattern 0 = []
boardPattern size = [([1..size] ++ [size, size - 1 .. 1]) !! (x `mod` (size *2)) | x <- [0..]]

showBoardNumber :: Int -> Int -> String
showBoardNumber num width = if (num == 0) then (unwordsNoSpaces $ take width (repeat " ")) else ((unwordsNoSpaces $ take (width - (length (show num))) (repeat " ")) ++ (show num))

standardGame :: Game
standardGame = Game {
    numDice = 2,
    dice = [
        Die {dieSize = 6, dieValue = 0},
        Die {dieSize = 6, dieValue = 0}],
    bigBoard = BigBoard {
        boardSize = 9,
        boardDepth = 1,
        boardState = array ((1,1),(1,9)) [((x,y),y) | x<-[1], y<-[1..9]]},
    doublesTokens = 0,
    totalRolls = 0
}

wonGame :: Game
wonGame = Game {
    numDice = 2,
    dice = [
        Die {dieSize = 6, dieValue = 0},
        Die {dieSize = 6, dieValue = 0}],
    bigBoard = BigBoard {
        boardSize = 9,
        boardDepth = 1,
        boardState = array ((1,1),(1,9)) [((x,y),0) | x<-[1], y<-[1..9]]},
    doublesTokens = 0,
    totalRolls = 0
}

doubleGame :: Game
doubleGame = Game {
    numDice = 2,
    dice = [
        Die {dieSize = 6, dieValue = 0},
        Die {dieSize = 6, dieValue = 0}],
    bigBoard = BigBoard {
        boardSize = 9,
        boardDepth = 2,
        boardState = array ((1,1),(2,9)) [((x,y),((boardPattern 9) !! (9*(x-1)+(y-1))))  | x<-[1,2], y<-[1..9]]},
    doublesTokens = 0,
    totalRolls = 0
}

inProgGame :: Game
inProgGame = Game {
    numDice = 2,
    dice = [
        Die {dieSize = 6, dieValue = 3},
        Die {dieSize = 6, dieValue = 4}],
    bigBoard = BigBoard {
        boardSize = 9,
        boardDepth = 2,
        boardState = array ((1,1),(2,9)) [((x,y), (if ((x == 1) && (y `elem` [1,5])) || (y==3) then 0 else ((boardPattern 9) !! (9*(x-1)+(y-1))))) | x<-[1,2], y<-[1..9]]},
    doublesTokens = 0,
    totalRolls = 0
}

overGame :: Game
overGame = Game {
    numDice = 2,
    dice = [
        Die {dieSize = 6, dieValue = 1},
        Die {dieSize = 6, dieValue = 1}],
    bigBoard = BigBoard {
        boardSize = 9,
        boardDepth = 2,
        boardState = array ((1,1),(2,9)) [((x,y), (if ((x == 1) && (y `elem` [1,5])) || (y==2) then 0 else ((boardPattern 9) !! (9*(x-1)+(y-1))))) | x<-[1,2], y<-[1..9]]},
    doublesTokens = 1,
    totalRolls = 0
}

generateNewGame :: Int -> Int -> Int -> Int -> Game
generateNewGame numberOfDice diceSize sizeOfBoard numberOfBoards = Game {
    numDice = numberOfDice,
    dice = take numberOfDice (repeat (Die {dieSize = 6, dieValue = 0})),
    bigBoard = BigBoard {
        boardSize = sizeOfBoard,
        boardDepth = numberOfBoards,
        boardState = array ((1,1),(numberOfBoards,sizeOfBoard)) [((x,y),(boardPattern sizeOfBoard) !! (sizeOfBoard*(x-1)+(y-1))) | x<-[1..numberOfBoards], y<-[1..sizeOfBoard]]
    },
    doublesTokens = 0,
    totalRolls = 0
}

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

gameWon :: Game -> Bool
gameWon (Game {bigBoard = board}) = and $ Prelude.map (==0) [boardState board Data.Array.! (x,y)|x<-[1..boardDepth board],y<-[1..boardSize board]]

diceFromRolls :: [Int] -> Int -> [Die]
diceFromRolls rolls size = [Die {dieSize = size, dieValue = x} | x <- rolls]

isDoubles :: [Int] -> Bool
isDoubles dList = and $ Prelude.map (== head dList) dList

setRollDoubles :: [Int] -> Game -> Game
setRollDoubles dList g@(Game nDice dice board doubles rolls) = g {dice = diceFromRolls dList (dieSize (head dice)), doublesTokens = if (isDoubles dList) then (doubles + 1) else doubles, totalRolls = rolls + 1}

randomList :: Int -> Int -> IO([Int])
randomList 0 _ = return []
randomList num size = do
  r  <- randomRIO (1,size)
  rs <- randomList (num - 1) size
  return (r:rs) 

exposedCoordForColumn :: Int -> Game -> [(Int, Int)]
exposedCoordForColumn col g@(Game nDice dice board doubles rolls) = [ (x, col) | x <- [1..boardDepth board], ((boardState board Data.Array.! (x,col)) /= 0)] 

boardValueSum :: [(Int, Int)] -> Game -> Int
boardValueSum [] _ = 0
boardValueSum ((a,b):xs) g@(Game nDice dice board doubles rolls) = boardState board Data.Array.! (a,b) + boardValueSum xs g

rollSum :: Game -> Int
rollSum g@(Game nDice dice board doubles rolls) = sum ([dieValue x | x <- dice])

newPossibleMoves :: Game -> [[(Int, Int)]]
newPossibleMoves g@(Game nDice dice board doubles rolls) = [xs | xs <- (sublists reducedBoardList), (boardValueSum xs g) == (rollSum g)] where
    boardList = Prelude.foldr (++) [] ([take 1 (exposedCoordForColumn y g) |  y <- [1..boardSize board]])
    reducedBoardList = [(x,y) | (x,y) <- boardList, boardState board Data.Array.! (x,y) <= (rollSum g)]

newPlayGame :: Game -> IO()
newPlayGame g@(Game nDice dice board doubles rolls) = do
    if gameWon g 
    then do
        putStrLn "You Won!"
        putStrLn "Press Enter to Start New Game:"
        response <- getLine
        newPlayGame (generateNewGame nDice (dieSize (head dice)) (boardSize board) (boardDepth board))
    else do
        if dieValue (head dice) == 0 then do
            putStrLn "\n*** Rolling! ***"
            dList <- (randomList nDice (dieSize (head dice)))
            let newG = setRollDoubles dList g
            print newG
            newPlayGame newG
        else do
            if take 1 (newPossibleMoves g) == []
            then do
                if doubles == 0
                then do
                    print g
                    putStrLn "You Lost!"
                    putStrLn "Press Enter to Start New Game:"   
                    response <- getLine
                    newPlayGame (generateNewGame nDice (dieSize (head dice)) (boardSize board) (boardDepth board))
                else do
                    putStrLn "\nCan't Move - Press Enter to use Doubles Token and Roll Again:"
                    response <- getLine
                    dList <- (randomList nDice (dieSize (head dice)))
                    let newG = setRollDoubles dList g
                    print newG
                    newPlayGame newG
            else do
                putStrLn "here"

       
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
        if take 1 (possibleMoves b) == []
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
    