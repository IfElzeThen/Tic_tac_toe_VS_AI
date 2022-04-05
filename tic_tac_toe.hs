
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


switchPlayer :: Game -> Game
switchPlayer game =
	case gamePlayer game of
		PlayerX -> game { gamePlayer = PlayerO }
		PlayerO -> game { gamePlayer = PlayerX }

isRowFull2 :: Cell -> [Cell] -> Bool
isRowFull2 a [] = True
isRowFull2 x (y:ys)
	|x /= y = False
	|otherwise = True && isRowFull2 x ys

isRowFull :: [Cell] -> State
isRowFull (x:xs)
	|x == (Just PlayerX) && isRowFull2 (Just PlayerX) xs = GameOver (Just PlayerX)
	|x == (Just PlayerO) && isRowFull2 (Just PlayerO) xs = GameOver (Just PlayerO)
	|otherwise = Running

getColumns :: [[a]] -> [[a]]
getColumns cells = [[cell!!j | cell <- cells] | j <- [0..2]]

getDiagonals :: [[a]] -> [[a]]
getDiagonals cells = [[cells!!i!!i | i <- [0..2]], [cells!!i!!j | i <- [0..2], let j = 2-i]]

stateAND :: State -> State -> State
stateAND (GameOver a) s = GameOver a
stateAND s (GameOver a) = GameOver a
stateAND _ _ = Running

checkBoard :: Board -> State
checkBoard [] = Running
checkBoard (x:board) = stateAND (isRowFull x) (checkBoard board)

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



inRange :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inRange ((a, b), (c, d)) (e, f) = e>=a && e<=b && f>=a && f<=b

isCorrect = inRange ((0, 2), (0, 2))



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


zameniElement :: [a] -> Int -> a -> [a]
zameniElement [] 0 y = []
zameniElement (x:xs) 0 y = y:xs
zameniElement (x:xs) i y = x:(zameniElement xs (i-1) y)


playerTurn :: Game -> (Int, Int) -> Game
playerTurn game (x, y)
	|isCorrect (x, y) && isNothing (board!!x!!y) = 
		switchPlayer $ game {gameBoard = zameniElement board x (zameniElement (board !! x) y (Just player))}
	|otherwise = game
	where
		player = gamePlayer game
		board = gameBoard game


printPlayer :: Player -> IO()
printPlayer p
	|p == PlayerX = putStrLn "X na potezu"
	|p == PlayerO = putStrLn "O na potezu"


playGame :: Game -> IO()
playGame game
	| (winner board) == Running = do
							printPlayer player
							printBoard board
							(x, y) <- getCoords
							playGame (playerTurn game (x, y))
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
