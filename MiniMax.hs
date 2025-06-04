module MiniMax where
import State
import Board
import Control.Monad (when)

inf :: Int
inf = 1000000

data StateMove = StateMove {
    state :: State,
    move  :: (Int, Int),
    eval  :: Int
}

instance Eq StateMove where
    a == b = (move a == move b) && (eval a == eval b)

instance Ord StateMove where
    compare a b = compare (eval a) (eval b)

checkAndMakeMove :: State -> (Int, Int) -> Maybe State
checkAndMakeMove s (x, y) = if checkMove s (x,y) then Just (makeMove s (x, y)) else Nothing

-- funkcja do listy ruchow z pozycji
getPossibleMoves:: State -> [StateMove]
getPossibleMoves s = case nextIndex s of
    Just nextBoardIndex ->
        [StateMove { state = state', move = (nextBoardIndex, moveIndex), eval = 0 } |
            moveIndex <- [0..8],
            Just state' <- [checkAndMakeMove s (nextBoardIndex, moveIndex)] ]
    Nothing ->
        [StateMove { state = state', move = (nextBoardIndex, moveIndex), eval = 0 } |
            moveIndex <- [0..8],
            nextBoardIndex <- [0..8],
            Just state' <- [checkAndMakeMove s (nextBoardIndex, moveIndex)] ]

-- funkcja do oceny heurystycznej stanu planszy
evalState :: State -> Int
evalState s =
    case checkBigBoard (board s) of
        Just (Just p) | p == current s           -> inf
        Just (Just p) | p == other (current s)   -> -inf
        Just Nothing                             -> 0
        _                                        -> 0

minimax :: StateMove -> Bool -> Int -> StateMove
minimax s maximizing 0 = StateMove {state = state s, move = move s, eval = evalState (state s)}

minimax s maximizing depth =
    let nextStates = [minimax state (not maximizing) (depth-1) | state <- getPossibleMoves (state s)]
    in if maximizing
        then maximum nextStates
        else minimum nextStates



-- minimax wybierający następny ruch