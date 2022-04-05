data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = [[Cell]]

data Game = Game { gameBoard :: Board,
				   gamePlayer :: Player,
				   gameState :: State
				 } deriving (Eq, Show)

initialGame = Game { gameBoard = initialBoard,
					 gamePlayer = PlayerX,
					 gameState = Running }
	where
		initialBoard = [[Nothing | i <- [1..3]] | j <- [1..3]]

--show board in console
showCell :: Cell -> IO()
showCell (Just PlayerX) = putStr "X"
showCell (Just PlayerO) = putStr "O"
showCell Nothing = putStr " "

showRow :: [Cell] -> IO()
showRow [x] = do 
				(showCell x)
				putStr "\n"
showRow (x:xs) = do 
					(showCell x)
					putStr "|"
					(showRow xs)

printBoard :: [[Cell]] -> IO()
printBoard [x] = showRow x
printBoard (x:board) = do
					showRow x
					putStrLn "-----"
					printBoard board

printPlayer :: Player -> IO()
printPlayer p
	|p == PlayerX = putStrLn "X na potezu"
	|p == PlayerO = putStrLn "O na potezu"

--checkGameOver
isRowFull2 :: Cell -> [Cell] -> Bool
isRowFull2 a [] = True
isRowFull2 x (y:ys)
	|x /= y = False
	|otherwise = isRowFull2 x ys

isRowFull :: [Cell] -> State
isRowFull (x:xs)
	|x == (Just PlayerX) && isRowFull2 (Just PlayerX) xs = GameOver (Just PlayerX)
	|x == (Just PlayerO) && isRowFull2 (Just PlayerO) xs = GameOver (Just PlayerO)
	|otherwise = Running

getColumns :: [[a]] -> [[a]]
getColumns cells = [[cell!!j | cell <- cells] | j <- [0..2]]

getDiagonals :: [[a]] -> [[a]]
getDiagonals cells = [[cells!!i!!i | i <- [0..2]], [cells!!i!!j | i <- [0..2], let j = 2-i]]

stateOR :: State -> State -> State
stateOR (GameOver a) s = GameOver a
stateOR s (GameOver a) = GameOver a
stateOR _ _ = Running

checkBoard :: Board -> State
checkBoard [] = Running
checkBoard (x:board) = stateOR (isRowFull x) (checkBoard board)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

isFull :: Board -> Bool
isFull board = not (foldl1 (||) [foldl1 (||) (map (isNothing) x) | x <- board])

winner :: Board -> State
winner board 
	|isFull board && checkBoard allThree == Running = GameOver Nothing
	|otherwise = checkBoard allThree
	where 
	allThree = board ++ (getColumns board) ++ (getDiagonals board)

--move validation
inRange :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inRange ((a, b), (c, d)) (e, f) = e>=a && e<=b && f>=a && f<=b

isCorrect = inRange ((0, 2), (0, 2))


--get player move
getCoords :: IO (Int, Int)
getCoords = do
			putStrLn "Unesite polje na koje zelite da igrate"
			putStr "vrsta: "
			line <- getLine
			let x = (read line :: Int)
			putStr "kolona: "
			line <- getLine
			let y = (read line :: Int)
			return (x, y)

replaceElem :: [a] -> Int -> a -> [a]
replaceElem [] 0 y = []
replaceElem (x:xs) 0 y = y:xs
replaceElem (x:xs) i y = x:(replaceElem xs (i-1) y)

switchPlayer :: Game -> Game
switchPlayer game =
	case gamePlayer game of
		PlayerX -> game { gamePlayer = PlayerO }
		PlayerO -> game { gamePlayer = PlayerX }

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game (x, y)
	|isCorrect (x, y) && isNothing (board!!x!!y) = 
		switchPlayer $ game {gameBoard = replaceElem board x (replaceElem (board !! x) y (Just player))}
	|otherwise = game
	where
		player = gamePlayer game
		board = gameBoard game

getOpponent :: Cell -> Cell
getOpponent (Just PlayerX) = (Just PlayerO)
getOpponent (Just PlayerO) = (Just PlayerX)
getOpponent _ = Nothing

filterMove :: Int -> Game -> Bool
filterMove i game = isNothing (board!!x!!y)
	where
		(x, y) = convertCoords i
		board = gameBoard game

convertCoords :: Int -> (Int, Int)
convertCoords i = (i `div` 3, i `mod` 3)

score :: Int -> Game -> Int
score i game
	| w == (GameOver (Just player)) = 1
	| w == (GameOver (getOpponent (Just player))) = (-1)
	| w == (GameOver Nothing) = 0
	| otherwise = (-scoreNext)
		where
			w = (winner board)
			(x, y) = convertCoords i
			board = gameBoard game
			player = gamePlayer game
			(scoreNext, _) = minimax (playerTurn game (x, y))


minimax :: Game -> (Int, Int)
minimax game 
	|list == [] = ((score 0 game), 0)
	|otherwise = maxl list
	where 
		list = [((score i game), i) | i <- [0..8], filterMove i game]

maxl :: [(Int, Int)] -> (Int, Int)
maxl [] = (0, 0)
maxl [(x, i)] = (x, i)
maxl ((x, i):xs)
	| x > max = (x, i)
	| otherwise = maxl xs
		where (max, _) = maxl xs

computerTurn :: Game -> Game
computerTurn game = playerTurn game (x, y)
	where
		(_, i) = minimax game
		(x, y) = convertCoords i

playGame :: Game -> IO()
playGame game
	| (winner board) == Running && player == PlayerX = do
							printPlayer player
							printBoard board
							(x, y) <- getCoords
							playGame (playerTurn game (x, y))
	| (winner board) == Running && player == PlayerO =
							playGame (computerTurn game)
	| (winner board) == (GameOver (Just PlayerX)) = do
							printBoard board
							putStr "Kraj igre. Pobedio je X!"
	| (winner board) == (GameOver (Just PlayerO)) = do
							printBoard board
							putStr "Kraj igre. Pobedio je O!"
	| (winner board) == (GameOver Nothing) = do
							printBoard board
							putStr "Kraj igre. Nereseno."
	where 
		player = gamePlayer game
		state = gameState game
		board = gameBoard game

startGame = playGame initialGame