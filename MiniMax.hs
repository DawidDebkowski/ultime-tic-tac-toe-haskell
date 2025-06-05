module MiniMax where
import State
import Board
import Debug.Trace

inf :: Int
inf = 1000000

data StateMove = StateMove {
    state :: State,
    move  :: (Int, Int),
    eval  :: Int,
    moveDepth :: Int
}

instance Eq StateMove where
    a == b = (move a == move b) && (eval a == eval b)

instance Ord StateMove where
    compare a b = compare (eval a) (eval b)

----- znajdywanie ruchów --------------

checkAndMakeMove :: State -> (Int, Int) -> Maybe State
checkAndMakeMove s (x, y) = if checkMove s (x,y) then Just (makeMove s (x, y)) else Nothing

-- funkcja do listy ruchow z pozycji
getPossibleMoves:: State -> [StateMove]
getPossibleMoves s = case nextIndex s of
    Just nextBoardIndex ->
        [StateMove { state = state', move = (nextBoardIndex, moveIndex), eval = 0, moveDepth = 0 } |
            moveIndex <- [0..8],
            Just state' <- [checkAndMakeMove s (nextBoardIndex, moveIndex)] ]
    Nothing ->
        [StateMove { state = state', move = (nextBoardIndex, moveIndex), eval = 0, moveDepth = 0 } |
            moveIndex <- [0..8],
            nextBoardIndex <- [0..8],
            Just state' <- [checkAndMakeMove s (nextBoardIndex, moveIndex)] ]

countPawns :: Maybe [Cell] -> Player -> Int
countPawns Nothing _ = 0
countPawns (Just sb) player =
    length [c | c <- sb, c == Taken player]

evalNextBoard :: State -> Int
evalNextBoard s = case nextIndex s of
                Nothing -> 7 * if current s == X then -1 else 1
                Just idx ->
                    let sb = (board s) !! idx
                    in countPawns (Just sb) X -
                    countPawns (Just sb) O +
                    evalSmallBoard sb X -
                    evalSmallBoard sb O

twoInALineScore :: BigBoard -> Int
twoInALineScore bb =
    let x_wonBoards = wonSmallBoards bb X
        o_wonBoards = wonSmallBoards bb O

        countInLine boards line = length (filter (`elem` boards) line)

        xTwoInLine = filter
            (\line -> countInLine x_wonBoards line == 2
                   && countInLine o_wonBoards line == 0
                   && any (\i -> i `notElem` x_wonBoards && i `notElem` o_wonBoards) line
            ) wins

        oTwoInLine = filter
            (\line -> countInLine o_wonBoards line == 2
                   && countInLine x_wonBoards line == 0
                   && any (\i -> i `notElem` x_wonBoards && i `notElem` o_wonBoards) line
            ) wins
        twoInLineScore = length xTwoInLine - length oTwoInLine
    in twoInLineScore * 10 -- 10 points per two-in-a-line, adjust as needed

-- sprawdza czy na jakiejkolwiek linii sa dwa takie same symbole i jedno puste pole
twoInLine :: SmallBoard -> Player -> Bool
twoInLine sb player = any hasTwoAndEmpty wins
  where
    hasTwoAndEmpty idxs =
      let line = map (sb !!) idxs
      in length (filter (== Taken player) line) == 2 && length (filter (== Empty) line) == 1

evalSmallBoard :: SmallBoard -> Player -> Int
evalSmallBoard sb player
  | twoInLine sb player = 10
  | twoInLine sb (other player) = -10
  | otherwise = 0

evalSmallBoards :: BigBoard -> Player -> Int
evalSmallBoards bb player = length [sb | sb <- bb, twoInLine sb player]

-- funkcja do oceny heurystycznej stanu planszy
evalState :: StateMove -> Int
evalState sm =
evalState :: StateMove -> Int
evalState sm =
    case checkBigBoard (board s) of
        Just (Just p) | p == X           -> inf
        Just (Just p) | p == O           -> -inf
        Just Nothing                      -> generalScore
        _ -> evalNextBoard s + generalScore
        where
            s = state sm
            generalScore = twoInALineScore (board s) + evalSmallBoard (board s !! snd (move sm) ) X - -- sprawdzamy aktualna plansze, zeby miala wieksza wartosc
                evalSmallBoard (board s !! snd (move sm) ) O + -- tak samo dla O
                evalSmallBoards (board s) X - -- sprawdzamy ile plansz jest prawie wygranych
                evalSmallBoards (board s) O

minimax :: StateMove -> Bool -> Int -> StateMove
minimax sm maximizing 0 = StateMove {state = state sm, move = move sm, eval = evalState sm}

minimax s maximizing depth =
    let nextStates = [minimax state (not maximizing) (depth-1) | state <- getPossibleMoves (state s)]
    in if maximizing
        then maximum nextStates
        else minimum nextStates


minimaxAlphaBeta :: StateMove -> Bool -> Int -> Int -> Int -> StateMove
minimaxAlphaBeta currentState maximizing depth alpha beta
    | depth == 0 =
        StateMove { state = state currentState, move = (-1,-1), eval = evalState currentState } -- move is placeholder at leaf

    | maximizing =
        let
            possibleStateMoves = getPossibleMoves (state currentState)

            -- Inner loop function to iterate through moves
            loopMoves :: [StateMove] -> StateMove -> Int -> StateMove
            loopMoves [] bestMoveSoFar _currentAlpha = bestMoveSoFar
            loopMoves (currentPotentialMove:remainingMoves) bestMoveSoFar currentAlphaInternal =
                -- If currentAlphaInternal (best for maximizer) is already >= beta (worst for minimizer), prune
                if currentAlphaInternal >= beta then
                    bestMoveSoFar
                else
                    let
                        -- Evaluate this move by calling minimax on the resulting state
                        -- currentPotentialMove.state is the state *after* the move
                        childResult = minimaxAlphaBeta currentPotentialMove False (depth - 1) currentAlphaInternal beta

                        -- If this path is better, update bestMoveSoFar
                        -- The 'move' is currentPotentialMove.move, 'eval' is from childResult
                        newBestMove = if eval childResult > eval bestMoveSoFar
                                      then StateMove { state = state currentPotentialMove, -- state after this move
                                                       move = move currentPotentialMove,   -- the move taken
                                                       eval = eval childResult }
                                      else bestMoveSoFar

                        newAlpha = max currentAlphaInternal (eval newBestMove)
                    in loopMoves remainingMoves newBestMove newAlpha
        -- Initial best move for maximizing player (worst possible score)
        -- The 'move' in initialBest is a placeholder.
        in if null possibleStateMoves
           then StateMove { state = state currentState, move = (-1,-1), eval = evalState currentState } -- No moves, evaluate current
           else loopMoves possibleStateMoves (StateMove {state = state currentState, move = (-1,-1), eval = -inf -1}) alpha

    | otherwise = -- Minimizing player
        let
            possibleStateMoves = getPossibleMoves (state currentState)

            loopMoves :: [StateMove] -> StateMove -> Int -> StateMove
            loopMoves [] bestMoveSoFar _currentBeta = bestMoveSoFar
            loopMoves (currentPotentialMove:remainingMoves) bestMoveSoFar currentBetaInternal =
                if alpha >= currentBetaInternal then
                    bestMoveSoFar
                else
                    let
                        childResult = minimaxAlphaBeta currentPotentialMove True (depth - 1) alpha currentBetaInternal

                        newBestMove = if eval childResult < eval bestMoveSoFar
                                      then StateMove { state = state currentPotentialMove,
                                                       move = move currentPotentialMove,
                                                       eval = eval childResult }
                                      else bestMoveSoFar

                        newBeta = min currentBetaInternal (eval newBestMove)
                    in loopMoves remainingMoves newBestMove newBeta

        in if null possibleStateMoves
           then StateMove { state = state currentState, move = (-1,-1), eval = evalState currentState } -- No moves, evaluate current
           else loopMoves possibleStateMoves (StateMove {state = state currentState, move = (-1,-1), eval = inf + 1}) beta

-- Top-level function to call minimax
-- Call this function from your game loop when it's the AI's turn
findBestMove :: State -> Int -> Bool -> StateMove
findBestMove s searchDepth maxMin =
    -- Assuming the 'current s' player is the one we want to find the best move for (the AI)
    -- and this player is trying to maximize their score.
    minimaxAlphaBeta (StateMove{state = s, move = (-1, -1), eval = 0}) maxMin searchDepth (-inf -1) (inf + 1) -- Initial alpha, beta


-- minimax wybierający następny ruch