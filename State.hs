module State (State(..), showBoard, showSmallBoard, checkMove, makeMove, startingState, other) where
import Board
import Data.List (intercalate)

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
-- showSmallBoard :: SmallBoard -> [String]
-- showSmallBoard sb = [row r | r <- [0..2]]
--     where row r = intercalate " " [show (sb !! (r*3 + c)) | c <- [0..2]]
showSmallBoard :: SmallBoard -> [String]
showSmallBoard sb = case checkSmallBoard sb of
    Nothing -> [ row r | r <- [0..2] ]
    Just Nothing -> ["■ ■ ■","■ ■ ■","■ ■ ■"]
    Just (Just O) -> ["/ - \\","|   |","\\ _ /"]
    Just (Just X) -> ["\\   /","  X  ","/   \\"]
    where
        row r = intercalate " " [ show (sb !! (r*3 + c)) | c <- [0..2] ]

showBoard :: State -> String
showBoard s = intercalate "\n" (
        ("-" ++ (if Just 0 == nextIndex s then replicate 7 '=' else replicate 7 '-') ++ "-" ++ (if Just 1 == nextIndex s then replicate 7 '=' else replicate 7 '-') ++ "-" ++ (if Just 2 == nextIndex s then replicate 7 '=' else replicate 7 '-') ++ "-") : 
        concat [blockRows br | br <- [0..2]]
    )
  where
    b = board s

    cellRow :: Int -> Int -> String
    cellRow br r = 
        (if Just (br*3) == nextIndex s then "||" else "| ") ++
        showSmallBoard (b !! (br*3)) !! r ++
        (if Just (br*3) == nextIndex s then "|| " else if Just (br*3+1) == nextIndex s then " ||" else " | ") ++
        showSmallBoard (b !! (br*3+1)) !! r ++
        (if Just (br*3+1) == nextIndex s then "|| " else if Just (br*3+2) == nextIndex s then " ||" else " | ") ++
        showSmallBoard (b !! (br*3+2)) !! r ++
        (if Just (br*3+2) == nextIndex s then "||" else " |")
    -- cellRow br r = "| " ++ intercalate " | " [showSmallBoard (b !! (br*3 + bc)) !! r | bc <- [0..2]] ++ " |"

    blockRows :: Int -> [String]
    blockRows br = 
        [cellRow br r | r <- [0..2]] ++ 
        ["-" ++ (if Just ((br+1)*3) == nextIndex s || Just (br*3) == nextIndex s then replicate 7 '=' else replicate 7 '-') ++ "-" ++ (if Just ((br+1)*3+1) == nextIndex s || Just (br*3+1) == nextIndex s then replicate 7 '=' else replicate 7 '-') ++ "-" ++ (if Just ((br+1)*3+2) == nextIndex s || Just (br*3+2) == nextIndex s then replicate 7 '=' else replicate 7 '-') ++ "-"]

    border = replicate 25 '-'