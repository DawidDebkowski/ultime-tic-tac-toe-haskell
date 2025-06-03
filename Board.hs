import Data.List (intercalate)
import Data.Maybe (isJust)

module Board (Cell(..), SmallBoard, BigBoard) where

data Cell = Empty | Taken deriving (Eq, Show)
instance Show Cell where
    show Empty = "_"
    show (Taken X) = "X"
    show (Taken O) = "O"

type SmallBoard = [Cell]
type BigBoard = [SmallBoard]

-- stany malych plansz
-- Nothing - plansza na ktorej mozna grac
-- Just (Nothing) - remis
-- Just (Just X/O) - wygrana X/O
type SmallState = [Maybe (Maybe Player)]

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

-- updatowanie stanow malych plansz
updateSmallState :: BigBoard -> SmallState
updateSmallState bb = map checkSmallBoard bb

-- sprawdzenie wygranej na duzej planszy
checkBigBoard :: SmallState -> Maybe (Maybe Player)
checkBigBoard ss
    | any threeX wins = Just (Just X) -- wygrana X
    | any threeO wins = Just (Just O) -- wygrana O
    | all isJust sw = Just Nothing -- remis (isJust True, gdy Just X/O)
    | otherwise = Nothing -- mozna grac
    where 
        threeX idxs = all (== Just (Just X)) (map (ss !!) idxs)
        threeO idxs = all (== Just (Just O)) (map (ss !!) idxs)

-- tworzy liste 3 rzedow z malej planszy (dla rzedu: Taken X, Nothing, Taken O zwraca "X _ O" i tak dla kazdego rzedu)
showSmallBoard :: SmallBoard -> [String]
showSmallBoard sb = [row r | r <- [0..2]]
    where row r = intercalate " " [show (sb !!) (r*3 + c) | c <- [0..2]]

-- wypisywanie planszy
showBoard :: State -> String
-- polaczenie 3 list 3/4 rzedow w 1 liste i polaczenie z separatorem "\n"
showBoard s = intercalate "\n" (concat ([row r | r <- [0..2]]))
    where
        b = board s
        -- lista 3/4 rzedow
        -- R - index rzedu smallBoard [0..2], r - index rzedu tekstu [0..2], C - index komuny SmallBoard [0..2]
        -- R*3 + C - index SmallBoard, !! r - wyciaga r-ty rzad z showSmallBoard 
        row R = [intercalate " | " [showSmallBoard (b !! (R*3 + C)) !! r | C <- [0..2]] | r <- [0..2]] ++ [if R<2 then replicate 22 "-" else ""]

-- dodawanie nowego pola na plansze
updateBoard :: Board -> Int -> Int -> Cell -> Board
-- smallBoardy od 0 do smallBoardIndex-1 ++ zupdatowany smallBoard ++ smallBoardy od smallBoardIndex+1 do konca
updateBoard b smallBoardIndex cellIndex value = take smallBoardIndex b ++ [updatedsm] ++ drop smallBoardIndex+1 b
    where
        -- wyjecie malej planszy
        sb = b !! smallBoardIndex
        -- wartosci od 0 do ci-1 ++ wstawiana wartosc ++ wartosci od ci+1 do konca
        updatedsm = take cellIndex sb ++ [value] ++ drop cellIndex+1 sb 