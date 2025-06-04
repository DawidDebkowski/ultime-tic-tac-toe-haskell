module State (State(..), showBoard, showSmallBoard, checkMove, makeMove, startingState, other) where
import Board
import Data.List (intercalate)
import Crypto.Hash.MD5 (start)

data State = State {
    board :: BigBoard, -- plansza
    current :: Player, -- gracz wykonujacy ruch
    nextIndex :: Maybe Int -- index malej planszy gdzie trzeba wykonac ruch
}

startingState :: State
startingState = State {
    board = replicate 9 (replicate 9 Empty), -- plansza 9 pustych malych plansz
    current = X, -- zaczyna X
    nextIndex = Nothing -- mozna zagrac w kazdej malej planszy
}

-- sprawdzenie poprawności ruchu
checkMove :: State -> (Int, Int) -> Bool
checkMove s (smallBoardIndex, cellIndex) = 
    -- poprawne indexy
    inRange smallBoardIndex &&
    inRange cellIndex &&
    -- ruch wykonywany na poprawnej planszy
    (nextIndex s == Just smallBoardIndex || nextIndex s == Nothing) &&
    -- plansza na ktorej mozna grac
    map checkSmallBoard (board s) !! smallBoardIndex == Nothing &&
    -- pole jest puste
    board s !! smallBoardIndex !! cellIndex == Empty
    where
        inRange i = i >= 0 && i < 9

other :: Player -> Player
other X = O
other O = X

-- wykonanie ruchu
makeMove :: State -> (Int, Int) -> State
makeMove s (smallBoardIndex, cellIndex) = newState
    where
        newBoard = updateBoard (board s) smallBoardIndex cellIndex (Taken (current s))
        -- wyliczenie nastepnej planszy
        next = if (map checkSmallBoard newBoard) !! cellIndex == Nothing then Just cellIndex else Nothing
        newState = State newBoard (other (current s)) next

-- tworzy liste 3 rzedow z malej planszy (dla rzedu: Taken X, Nothing, Taken O zwraca "X _ O" i tak dla kazdego rzedu)
showSmallBoard :: SmallBoard -> [String]
showSmallBoard sb = [row r | r <- [0..2]]
    where row r = intercalate " " [show (sb !! (r*3 + c)) | c <- [0..2]]

-- wypisywanie planszy
showBoard :: State -> String
-- polaczenie 3 list 3/4 rzedow w 1 liste i polaczenie z separatorem "\n"
showBoard s = intercalate "\n" (concat ([row r | r <- [0..2]]))
    where
        b = board s
        -- lista 3/4 rzedow
        -- R - index rzedu smallBoard [0..2], r - index rzedu tekstu [0..2], C - index komuny SmallBoard [0..2]
        -- R*3 + C - index SmallBoard, !! r - wyciaga r-ty rzad z showSmallBoard 
        row br = [intercalate " | " [showSmallBoard (b !! (br*3 + bc)) !! r | bc <- [0..2]] | r <- [0..2]] ++ [if br<2 then replicate 22 '-' else ""]
