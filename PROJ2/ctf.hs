-- Author: Gabriella Quattrone (SID: 913583423)
-- Author: Jenhifer Salas      (SID: 914329903)

-- This program will make a move in a game of
-- capture the flag.
-- It takes a current state as a string, a list
-- of states, and turn marker as a character.
-- It returns the next state as a string

-- In general we need to do the following:
--      1. generate the possible moves depending on depth
--      2. return a list of moves in the order of best to worst
--      3. check if the first move of the list has been is in history
--          if True then check the next possible best move in the list (repeat step 3)
--          if False then return the move

-- Assume white at top of board always, black at bottom. Turns can still change.

-- Heuristic 1: having more or less pawns (more = closer to winning, less = further away from winning)
-- Heuristic 2: how close the flag is to the other side (rectilinear)
-- Heuristic 3: did we capture a flag?
import Debug.Trace
import Data.Char

-- The Types
type Board = String -- a state of the board
type Boards = [Board]
type EBoard = (Board, Int) -- a state of the board and heuristic value
type EBoards = [EBoard]  -- a list of evaluated boards
type Coordinate = (Int, Int) -- the coordinates for each piece on the board
type Move = (Coordinate, Char)


-- The Interface
capture :: Boards -> Char -> Int -> Board
capture history turn depth = []
-- pass in size somehow
-- pass to minimax, which will call movegen


miniMax :: Char -> Char -> Int -> Int -> Int -> Board -> Board -> EBoard
miniMax player turn depth size dim parent state
    | depth == 0 =
        (parent, evaluateBoard state player dim)
    | player == turn =
        get (<) (map (miniMax player (opposite turn) (depth-1) size dim parent) (validMoves (moveGen state dim  size turn) size))
    | otherwise =
        get (>) (map (miniMax player (opposite turn) (depth-1) size dim parent) (validMoves (moveGen state dim  size turn) size))


-- Evaluate board
evaluateBoard :: Board -> Char -> Int -> Int
evaluateBoard state letter dim = (captured state (opposite letter)) + ((numPcs state letter) + (flagDistance state letter 0 dim))


-- A bit faster than quick sort.
get :: (Int->Int->Bool) -> EBoards -> EBoard
get _ [] = ([],0)
get func (b:bs) = get_helper func bs b

get_helper :: (Int->Int->Bool) -> EBoards -> EBoard -> EBoard
get_helper func boards current
    | null boards = current
    | apply func current (head boards) = get_helper func (tail boards) (head boards)
    | otherwise = get_helper func (tail boards) current


-- Quicksort
-- minSort :: EBoards -> EBoards
-- minSort []      = []
-- minSort (x:xs)  = minSort (filter (apply (<) x) xs) ++ [x] ++ minSort (filter (apply (>=) x) xs)
--
-- maxSort :: EBoards -> EBoards
-- maxSort []      = []
-- maxSort (x:xs)  = maxSort (filter (apply (>=) x) xs) ++ [x] ++ maxSort (filter (apply (<) x) xs)


apply :: (Int -> Int -> Bool) -> EBoard -> EBoard -> Bool
apply func board1 board2 = func (snd board1) (snd board2)


-- Heuristic Evaluators
-- Heuristic #1: Number of Pieces on Board
numPcs :: Board -> Char -> Int
numPcs state turn = numPcsHelper state turn 0

numPcsHelper :: Board -> Char -> Int -> Int
numPcsHelper state letter count
    | null state            = count
    | (head state) == letter
        || (head state) == toUpper letter
        || (head state) == toLower letter
                            = numPcsHelper (tail state) letter (count + 1)
    | otherwise             = numPcsHelper (tail state) letter count


-- Takes in a board and a turn to see if the flag for that turn's color was captured
captured :: Board -> Char -> Int
captured state letter
    | null state                   = 10 -- flag was captured
    | head state == toUpper letter = 0
    | otherwise                    = captured (tail state) letter


-- Gives points up to the size of the board for how close the turn's flag is to
-- the opposite side
flagDistance :: Board -> Char -> Int -> Int -> Int
flagDistance [] _ _ _ = 0
flagDistance (x:xs) turn pos dim
    | turn == 'w' && x == 'W' = fst (rowCol pos dim) + 1
    | turn == 'b' && x == 'B' = dim - fst (rowCol pos dim)
    | otherwise               = flagDistance xs turn (pos + 1) dim


-- Checks for valid moves by checking that the size of the boards match the
-- original size
validMoves :: Boards -> Int -> Boards
validMoves evaList size = filter ((\ x y -> x == (length y)) size) evaList


-- Find if the game is over already
-- if W at end or B at beginning or missing B or missing W
gameOver ::  Board -> Int -> Int -> Bool
gameOver state dim size = gameOverHelper state True True 0 dim size

gameOverHelper :: Board -> Bool -> Bool -> Int -> Int -> Int -> Bool
gameOverHelper mutState missW missB pos dim size
    | null mutState =
        if (pos /= size || missW || missB) then True else False
    | head mutState == 'W' =
        if (fst (rowCol pos dim)) == (dim-1) then True
        else gameOverHelper (tail mutState) False missB (pos + 1) dim size
    | head mutState == 'B' =
        if (fst (rowCol pos dim)) == 0 then True
        else gameOverHelper (tail mutState) missW False (pos + 1) dim size
    | otherwise  =
        gameOverHelper (tail mutState) missW missB (pos + 1) dim size


-- Moves Generator
-- Assume size of board has already been sqrt'd to dim
moveGen :: Board -> Int -> Int -> Char -> Boards
moveGen currState dim size letter =
    if  gameOver currState dim size then [currState]
    else moveGenHelper currState currState dim 0 letter
    -- moveGenHelper currState currState dim 0 letter

moveGenHelper :: Board -> Board -> Int -> Int -> Char -> Boards
moveGenHelper currState mutState dim pos letter
    | null mutState          = []
    | head mutState == letter ||
      head mutState == toUpper letter
      =  (move currState (head mutState) dim (rowCol pos dim)) ++ moveGenHelper currState (tail mutState) dim (pos + 1) letter
    | otherwise              = moveGenHelper currState (tail mutState) dim (pos + 1) letter


-- Moves Validator
move :: Board -> Char -> Int -> Coordinate -> Boards
move currState turn dim coord = moveHelper currState currState turn dim 0 coord (possPos turn dim coord)

moveHelper :: Board -> Board -> Char -> Int -> Int -> Coordinate -> [Move] -> Boards
moveHelper currState iterState turn dim pos coord possMoves
    | null possMoves || null iterState
        = []
    | not (inBounds(fst (head possMoves)) dim)
        = moveHelper currState iterState turn dim pos coord (tail possMoves)
    | (rowCol pos dim == fst (head possMoves)) && (inBounds(fst (head possMoves)) dim)
        = performMove currState (fst (head possMoves)) coord (snd (head possMoves)) 0 dim turn
            : moveHelper currState (tail iterState) turn dim (pos + 1) coord (tail possMoves)
    | otherwise
         = moveHelper currState (tail iterState) turn dim (pos + 1) coord possMoves

inBounds :: Coordinate -> Int -> Bool
inBounds (row, col) dim = row >= 0 && col >= 0 && row < dim && col < dim

-- Actually do the move
performMove :: Board -> Coordinate -> Coordinate -> Char -> Int -> Int -> Char -> Board
performMove currState targetPos pcPos 'm' viewPos dim piece
    | null currState  || (rowCol viewPos dim == targetPos && head currState /= '-')
        = []
    | rowCol viewPos dim == targetPos
        = piece : performMove (tail currState) targetPos pcPos 'm' (viewPos + 1) dim piece
    | rowCol viewPos dim == pcPos
        = '-' : performMove (tail currState) targetPos pcPos 'm' (viewPos + 1) dim piece
    | otherwise
        = head currState : performMove (tail currState) targetPos pcPos 'm' (viewPos + 1) dim piece

performMove currState targetPos pcPos 'j' viewPos dim piece = performJump currState targetPos pcPos viewPos dim piece (checkJumpee pcPos targetPos)

performJump :: Board -> Coordinate -> Coordinate -> Int -> Int -> Char -> Coordinate -> Board
performJump currState targetPos pcPos viewPos dim piece jumpee
    | null currState
        || (rowCol viewPos dim == targetPos && head currState /= '-')
        || (rowCol viewPos dim == jumpee && (toLower (head currState) /= opposite piece))
        = []
    | rowCol viewPos dim == targetPos
        = piece : performJump (tail currState) targetPos pcPos (viewPos + 1) dim piece jumpee
    | rowCol viewPos dim == pcPos || rowCol viewPos dim == jumpee
        = '-' : performJump (tail currState) targetPos pcPos (viewPos + 1) dim piece jumpee
    | otherwise
        = head currState : performJump (tail currState) targetPos pcPos (viewPos + 1) dim piece jumpee

checkJumpee :: Coordinate -> Coordinate -> Coordinate
checkJumpee (pRow, pCol) (tRow, tCol)
    | pRow == tRow = (pRow , div (pCol + tCol) 2)
    | pCol == tCol = (div (pRow + tRow) 2 , pCol)
    | otherwise    = (-1, -1)

-- To be used for jumps only, checks for the opposite character
opposite :: Char -> Char
opposite letter
    | letter == 'b' = 'w'
    | letter == 'w' = 'b'
    | otherwise     = '-'

-- Coordinates Developer
-- Returns the row and column of an indicated position
-- Assume the dimension has already been sqrt'd to dim
rowCol :: Int -> Int -> Coordinate
rowCol pos dim = ((pos `div` dim), (pos `mod` dim))


-- Positions Generator
possPos :: Char -> Int -> Coordinate -> [Move]
possPos 'w' dim (row, col) = [ ((row, col - 2), 'j'), ((row, col - 1), 'm'), ((row, col + 1), 'm'), ((row, col + 2), 'j'), ((row + 1, col), 'm'), ((row + 2, col), 'j')]
possPos 'b' dim (row, col) = [ ((row - 2, col), 'j'), ((row - 1, col), 'm'), ((row, col - 2), 'j'), ((row, col - 1), 'm'), ((row, col + 1), 'm'), ((row, col + 2), 'j')]
possPos 'W' dim (row, col) = [ ((row - 1, col), 'm'), ((row, col - 1), 'm'), ((row, col + 1), 'm'), ((row + 1, col), 'm')]
possPos 'B' dim (row, col) = [ ((row - 1, col), 'm'), ((row, col - 1), 'm'), ((row, col + 1), 'm'), ((row + 1, col), 'm')]
