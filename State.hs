module State (Player(..), State(..)) where

data Player = X | O deriving (Eq, Show)

data State = State {
    board :: BigBoard, -- plansza
    smallState :: SmallState, -- stany malych plansz
    current :: Player, -- gracz wykonujacy ruch
    nextIndex :: Maybe Int -- index malej planszy gdzie trzeba wykonac ruch
}

startingState :: State
startingState = State {
    board = replicate 9 (replicate 9 Empty), -- plansza 9 pustych malch plansz
    smallWon = replicate 9 Nothing, -- kazda plansza jest dostepna do grania
    current = X, -- zaczyna X
    nextIndex = Nothing -- mozna zagrac w kazdej malej planszy
}

-- sprawdzenie ruchu
checkMove :: State -> (Int, Int) -> State
checkMove s (smallBoardIndex, cellIndex) = 
    -- poprawne indexy
    inRange smallBoardIndex &&
    inRange cellIndex &&
    -- ruch wykonywany na poprawnej planszy
    (nextIndex s = smallBoardIndex || nextIndex s = Nothing) &&
    -- plansza na ktorej mozna grac
    smallState s !! smallBoardIndex = Nothing &&
    -- pole jest puste
    board s !! smallBoardIndex !! cellIndex = Empty
    where
        inRange i = i >= 0 && i < 9

-- wykonanie ruchu
makeMove :: State -> (Int, Int) -> State
makeMove s (smallBoardIndex, cellIndex) = newState
    where
        newBoard = updadeBoard (board s) smallBoardIndex cellIndex (Taken (current s))
        newSmallState = updateSmallState newBoard
        -- wyliczenie nastepnej planszy
        next = if meta !! cellIndex == Nothing then Just ci else Nothing
        newState = State newBoard newSmallState (other (current s)) next