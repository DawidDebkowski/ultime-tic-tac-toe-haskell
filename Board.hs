module Board (Cell(..), SmallBoard, BigBoard, Player(..), checkSmallBoard, updateBoard, checkBigBoard) where
import Data.Maybe (isJust)

data Player = X | O deriving (Eq, Show)

data Cell = Empty | Taken Player deriving (Eq)
instance Show Cell where
    show Empty = "_"
    show (Taken X) = "X"
    show (Taken O) = "O"

type SmallBoard = [Cell]
type BigBoard = [SmallBoard]

-- -- stany malych plansz
-- -- Nothing - plansza na ktorej mozna grac
-- -- Just (Nothing) - remis
-- -- Just (Just X/O) - wygrana X/O
-- type SmallState = [Maybe (Maybe Player)]

-- tablica wygrywajacych kombinacji indexow
wins :: [[Int]]
wins = [[0,1,2], [3,4,5], [6,7,8], [0,3,6], [1,4,7], [2,5,8], [0,4,8], [2,4,6]]

-- sprawdzenie wygranej na malej planszy
checkSmallBoard :: SmallBoard -> Maybe (Maybe Player)
checkSmallBoard sb
    | any threeX wins = Just (Just X) -- wygrana X
    | any threeO wins = Just (Just O) -- wygrana O
    | all (/= Empty) sb = Just Nothing -- remis
    | otherwise = Nothing -- mozna grac
    where
        -- sprawdzamy czy pod danymi 3 indexami sa tylko X/O
        threeX idxs = all (== Taken X) (map (sb !!) idxs)
        threeO idxs = all (== Taken O) (map (sb !!) idxs)

-- -- updatowanie stanow malych plansz
-- updateSmallState :: BigBoard -> SmallState
-- updateSmallState bb = map checkSmallBoard bb

-- sprawdzenie wygranej na duzej planszy
-- checkBigBoard :: SmallState -> Maybe (Maybe Player)
checkBigBoard :: BigBoard -> Maybe (Maybe Player)
checkBigBoard bb
    | any threeX wins = Just (Just X) -- wygrana X
    | any threeO wins = Just (Just O) -- wygrana O
    | all isJust ss = Just Nothing -- remis (isJust True, gdy Just X/O)
    | otherwise = Nothing -- mozna grac
    where 
        ss = map checkSmallBoard bb
        threeX idxs = all (== Just (Just X)) (map (ss !!) idxs)
        threeO idxs = all (== Just (Just O)) (map (ss !!) idxs)

-- dodawanie nowego pola na plansze
updateBoard :: BigBoard -> Int -> Int -> Cell -> BigBoard
-- smallBoardy od 0 do smallBoardIndex-1 ++ zupdatowany smallBoard ++ smallBoardy od smallBoardIndex+1 do konca
updateBoard b smallBoardIndex cellIndex value = take smallBoardIndex b ++ [updatedsb] ++ drop (smallBoardIndex+1) b
    where
        -- wyjecie malej planszy
        sb = b !! smallBoardIndex
        -- wartosci od 0 do ci-1 ++ wstawiana wartosc ++ wartosci od ci+1 do konca
        updatedsb = take cellIndex sb ++ [value] ++ drop (cellIndex+1) sb 