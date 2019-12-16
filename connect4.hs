data Slot = Empty
          | X
          | O
          deriving (Eq, Ord, Show)
start :: Int -> IO()
start 1 = play 1 X (createBoard 7)
start 2 = play 2 X (createBoard 7)
play :: Int -> Slot -> [[Slot]]-> IO()

play 2 turn board = do
              printBoard board
              if (turn == X) then do
                putStrLn "It's Xs turn!"
                line <- getLine
                let col = (read line :: Int)-1
                if((colPlayable col board)) then do
                 putStrLn "That column is full!"
                 play 2 X board
                else do
                 let temp = (add X col board)
                 let row = (findRow (getCol col board))
                 if (checkWin row col temp) then do
                   printBoard board
                   putStrLn "X Wins!"
                 else
                  play 2 O temp
              else do
                putStrLn "It's Os turn!"
                line <- getLine
                let col = (read line :: Int)-1
                if((colPlayable col board)) then do
                 putStrLn "That column is full!"
                 play 2 O board
                else do
                 let temp = (add O col board)
                 let row = (findRow (getCol col board))
                 if (checkWin row col temp) then do
                   printBoard board
                   putStrLn "O Wins!"
                 else
                  play 2 X temp
colPlayable :: Int-> [[Slot]]->Bool
colPlayable col board = (head (getCol col board)) /= Empty
printBoard :: [[Slot]] -> IO ()
printBoard ( ((row):rest)) =
                                do
                                printBoardHelper row
                                printBoard (rest)
printBoard _ = putStrLn ""

printBoardHelper :: [Slot]-> IO ()
printBoardHelper [] = putStrLn ""
printBoardHelper (x:xs) = do
                          putStr ((parse x) ++ " ")
                          printBoardHelper xs


createBoard :: Int -> [[Slot]]
createBoard num = (take num (repeat (take num (repeat (Empty)))))

itemAt :: Int->Int->[[Slot]]->Slot
itemAt row col board = (board!!row)!!col

checkWin :: Int -> Int -> [[Slot]] -> Bool
checkWin row col board = (checkWinHelper row col 1 4 board)||(checkWinHelper row col 2 4 board)||(checkWinHelper row col 3 4 board)||(checkWinHelper row col 4 4 board)||(checkWinHelper row col 5 4 board)||(checkWinHelper row col 6 4 board)||(checkWinHelper row col 7 4 board)

checkWinHelper :: Int -> Int -> Int -> Int -> [[Slot]] -> Bool
checkWinHelper row col dir 1 board = True
--down right
checkWinHelper row col 1 toCheck board = if (row<=3 &&col<=3)|| (toCheck /= 4) then
                                            if ((itemAt row col board) == (itemAt (row+1) (col+1) board)) then
                                               checkWinHelper (row+1) (col+1) 1 (toCheck-1) board
                                            else
                                            False
                                          else
                                          False
--left
checkWinHelper row col 2 toCheck board = if (col>=3)|| (toCheck /= 4) then
                                            if ((itemAt row col board) == (itemAt (row) (col-1) board)) then
                                               checkWinHelper (row) (col-1) 2 (toCheck-1) board
                                            else
                                               False
                                               else
                                               False
--right
checkWinHelper row col 3 toCheck board = if (col<=3)|| (toCheck /= 4) then
                                            if ((itemAt row col board) == (itemAt (row) (col+1) board)) then
                                               checkWinHelper (row) (col+1) 3 (toCheck-1) board
                                            else
                                               False
                                               else
                                               False
--up right
checkWinHelper row col 4 toCheck board = if (row>=3 &&col<=3)|| (toCheck /= 4) then
                                            if ((itemAt row col board) == (itemAt (row-1) (col+1) board)) then
                                               checkWinHelper (row-1) (col+1) 4 (toCheck-1) board
                                            else
                                               False
                                               else
                                               False
--up left
checkWinHelper row col 5 toCheck board = if (row>=3 &&col>=3)|| (toCheck /= 4) then
                                            if ((itemAt row col board) == (itemAt (row-1) (col-1) board)) then
                                               checkWinHelper (row-1) (col-1) 5 (toCheck-1) board
                                            else
                                               False
                                               else
                                               False
--down left
checkWinHelper row col 6 toCheck board = if (row<=3 &&col>=3)|| (toCheck /= 4) then
                                            if ((itemAt row col board) == (itemAt (row+1) (col-1) board)) then
                                               checkWinHelper (row+1) (col-1) 6 (toCheck-1) board
                                            else
                                               False
                                          else
                                             False
--down
checkWinHelper row col 7 toCheck board = if (row<=3)|| (toCheck /= 4) then
                                            if ((itemAt row col board) == (itemAt (row+1) (col) board)) then
                                               checkWinHelper (row+1) (col) 7 (toCheck-1) board
                                            else
                                               False
                                               else
                                               False

add :: Slot -> Int -> [[Slot]]->[[Slot]]
add piece col board = replace2d (findRow (getCol col board)) col piece board

replace :: Int -> Slot-> [Slot] -> [Slot]
replace i new xs = ((take i xs) ++ [new]) ++ (drop (i+1) xs)

replace2d :: Int -> Int -> Slot -> [[Slot]] -> [[Slot]]
replace2d i j new board = ((take i board) ++ [(replace j new (board!!i))]) ++ (drop (i+1) board)

getCol :: Int -> [[Slot]]-> [Slot]
getCol num board = getColHelper num board []

getColHelper :: Int->[[Slot]]->[Slot]->[Slot]
getColHelper num [] list = list
getColHelper num (top:board) list = getColHelper num board (list ++ [(top!!num)])


findRow :: [Slot] -> Int
findRow [] = 6
findRow list = if ((head list)/=Empty) then
                   6- ((length list))
                   else
                   findRow (tail list)

parse :: Slot -> String
parse Empty = "-"
parse X = "X"
parse O = "O"
