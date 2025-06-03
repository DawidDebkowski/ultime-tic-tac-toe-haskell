module MiniMax where
import State
import Board

inf :: Int
inf = 1000000

checkAndMakeMove :: State -> (Int, Int) -> Maybe State
checkAndMakeMove s (x, y) = if checkMove s (x,y) then Just (makeMove s (x, y)) else Nothing

-- funkcja do listy ruchow z pozycji
getPossibleMoves:: State -> [State]
getPossibleMoves s = case nextIndex s of
    Just nextBoardIndex ->
        [ state | moveIndex <- [1..9], Just state <- [checkAndMakeMove s (nextBoardIndex, moveIndex)] ]
    Nothing ->
        [ state | moveIndex <- [1..9], nextBoardIndex <- [1..9], Just state <- [checkAndMakeMove s (nextBoardIndex, moveIndex)] ]

-- funkcja do oceny heurystycznej stanu planszy
evalState :: State -> Int
evalState s =
    case checkBigBoard (board s) of
        Just (Just (current s)) -> inf
        Just (Just (other s))   -> -inf
        _                   -> 0

minimax :: State -> (Int, Int)
minimax s = (0, 0)

-- minimax wybierający następny ruch