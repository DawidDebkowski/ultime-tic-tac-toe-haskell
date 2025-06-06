module MiniMax where
import State
import Board
import Debug.Trace
import Data.Maybe (isNothing)
import Data.List (sort, sortBy)
import Data.Ord (comparing, Down(Down))

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
        [StateMove { state = state', move = (nextBoardIndex, moveIndex), eval = evalState state' (nextBoardIndex, moveIndex) 0, moveDepth = 0 } |
            moveIndex <- [0..8],
            Just state' <- [checkAndMakeMove s (nextBoardIndex, moveIndex)] ]
    Nothing ->
        [StateMove { state = state', move = (nextBoardIndex, moveIndex), eval = evalState state' (nextBoardIndex, moveIndex) 0, moveDepth = 0 } |
            moveIndex <- [0..8],
            nextBoardIndex <- [0..8],
            Just state' <- [checkAndMakeMove s (nextBoardIndex, moveIndex)] ]

-----------------------------

----- ewaluacje --------------

countPawns :: Maybe [Cell] -> Player -> Int
countPawns Nothing _ = 0
countPawns (Just sb) player =
    length [c | c <- sb, c == Taken player]

-- specjalna ewaluacja dla "następnej" planszy
-- jeżeli przeciwnik będzie mógł się ruszyć na "any", to źle dla osoby wykonującej ruch
-- w przeciwnym przypadku robi eval + liczy pionki
evalNextBoard :: State -> Int
evalNextBoard s = case nextIndex s of
                Nothing -> 7 * if current s == X then -1 else 1
                Just idx ->
                    let sb = (board s) !! idx
                    in countPawns (Just sb) X -
                    countPawns (Just sb) O +
                    evalSmallBoard sb X -
                    evalSmallBoard sb O

twoInALineBigBoard :: BigBoard -> Int
twoInALineBigBoard bb =
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
    in twoInLineScore 

-- sprawdza czy na jakiejkolwiek linii sa dwa takie same symbole i jedno puste pole
twoInLine :: SmallBoard -> Player -> Bool
twoInLine sb player = any hasTwoAndEmpty wins
  where
    hasTwoAndEmpty idxs =
      let line = map (sb !!) idxs
      in length (filter (== Taken player) line) == 2 && length (filter (== Empty) line) == 1

evalSmallBoard :: SmallBoard -> Player -> Int
evalSmallBoard sb player
    | checkSmallBoard sb == Just (Just player) = 3
    | twoInLine sb player = 1
    | twoInLine sb (other player) = -1
    | otherwise = 0

evalSmallBoards :: BigBoard -> Player -> Int
evalSmallBoards bb player = length [sb | sb <- bb, isNothing (checkSmallBoard sb), twoInLine sb player] + countWonSmallBoards bb player * 4

countWonSmallBoards :: BigBoard -> Player -> Int
countWonSmallBoards bb player = length [sb | sb <- bb, checkSmallBoard sb == Just (Just player)]

evalState :: State -> (Int, Int) -> p -> Int
evalState s mv d = evalStateMove StateMove{state = s, move = mv, eval = 0, moveDepth=0}

-- funkcja do oceny heurystycznej stanu planszy
evalStateMove :: StateMove -> Int
evalStateMove sm =
    case checkBigBoard (board s) of
        Just (Just p) | p == X           -> inf
        Just (Just p) | p == O           -> -inf
        Just Nothing                      -> generalScore
        _ -> evalNextBoard s + generalScore
        where
            s = state sm
            generalScore = twoInALineBigBoard (board s) * 50 + -- dwa w lii na dużej
                evalSmallBoard (board s !! snd (move sm) ) X * 10 - -- sprawdzamy aktualna plansze, zeby miala wieksza wartosc 
                evalSmallBoard (board s !! snd (move sm) ) O * 10 + -- tak samo dla O
                evalSmallBoards (board s) X * 5 - -- sprawdzamy ile plansz jest prawie wygranych
                evalSmallBoards (board s) O * 5

--------------------------------------

-------------- minimax ---------------

isTerminal :: StateMove -> Bool
isTerminal sm = case checkBigBoard (board (state sm)) of
    Just _  -> True
    Nothing -> False

isChildPreferredMax :: StateMove -> StateMove -> Bool
isChildPreferredMax childResult bestMoveSoFar =
    (eval childResult > eval bestMoveSoFar) || -- Strictly better
    (eval childResult == eval bestMoveSoFar && -- Tie in eval, check moveDepth
        ( (eval childResult == inf && moveDepth childResult > moveDepth bestMoveSoFar) || -- Win faster
          (eval childResult == -inf && moveDepth childResult < moveDepth bestMoveSoFar)    -- Lose slower
        )
    )

isChildPreferredMin :: StateMove -> StateMove -> Bool
isChildPreferredMin childResult bestMoveSoFar =
    (eval childResult < eval bestMoveSoFar) || -- Strictly better for minimizer
    (eval childResult == eval bestMoveSoFar && -- Tie in eval
        ( (eval childResult == inf && moveDepth childResult < moveDepth bestMoveSoFar) || -- X wins slower (good for O)
          (eval childResult == -inf && moveDepth childResult > moveDepth bestMoveSoFar)    -- O wins faster
        )
    )

createStateFromChild :: StateMove -> StateMove -> StateMove
createStateFromChild mv chld = StateMove { state = state mv,
                                           move = move mv,
                                           eval = eval chld,
                                           moveDepth = moveDepth chld }

minimaxAlphaBeta :: StateMove -> Bool -> Int -> Int -> Int -> StateMove
minimaxAlphaBeta currentState maximizing searchDepth alpha beta
    | searchDepth == 0 || isTerminal currentState =
        StateMove { state = state currentState, move = move currentState, eval = evalStateMove currentState, moveDepth = searchDepth }
    | otherwise =
        let
            -- dla polepszenia alfa-beta -> rozważamy ruchy od potencjalnie dla nas najlepszego
            possibleStateMoves =
                let moves = getPossibleMoves (state currentState)
                in if maximizing
                    then sortBy (comparing (Down . eval)) moves 
                    else sort moves   

            unifiedLoop :: Int                      -- alfa
                        -> Int                      -- beta
                        -> [StateMove]              -- tablica ruchów do przerobienia
                        -> StateMove                -- najlepszy dotychczasowy
                        -> StateMove
            unifiedLoop _ _ [] bestSoFar = bestSoFar
            unifiedLoop currentAlpha currentBeta (currentPotentialMove:remainingMoves) bestSoFar
                | currentAlpha > currentBeta = bestSoFar
                | otherwise =
                    let
                        msgPrefix = if maximizing then "UnifiedMax: " else "UnifiedMin: "
                        msg = msgPrefix ++ "Depth=" ++ show searchDepth ++
                              ", Alpha=" ++ show currentAlpha ++ ", Beta=" ++ show currentBeta ++
                              ", BestEval=" ++ show (eval bestSoFar) ++ ", Move=" ++ show (move bestSoFar)
                        tracedAction action = if searchDepth == 4 then trace msg action else action

                        childResult = minimaxAlphaBeta currentPotentialMove (not maximizing) (searchDepth - 1) currentAlpha currentBeta

                        isPreferred = if maximizing
                                      then isChildPreferredMax childResult bestSoFar
                                      else isChildPreferredMin childResult bestSoFar

                        newBestSoFar = if isPreferred
                                       then createStateFromChild currentPotentialMove childResult
                                       else bestSoFar

                        (nextAlpha, nextBeta) = if maximizing
                                                then (max currentAlpha (eval newBestSoFar), currentBeta)
                                                else (currentAlpha, min currentBeta (eval newBestSoFar))
                    in 
                       tracedAction $ 
                       unifiedLoop nextAlpha nextBeta remainingMoves newBestSoFar

            initialBestEval = if maximizing then -inf -1 else inf + 1
            initialBestSoFar = StateMove {state = state currentState, move = (-1,-1), eval = initialBestEval, moveDepth = searchDepth}

        in if null possibleStateMoves
           then StateMove { state = state currentState, move = (-1,-1), eval = evalStateMove currentState, moveDepth = searchDepth }
           else unifiedLoop alpha beta possibleStateMoves initialBestSoFar

-- Top-level function to call minimax
-- Call this function from your game loop when it's the AI's turn
findBestMove :: State -> Int -> Bool -> StateMove
findBestMove s searchDepth maxMin =
    -- Assuming the 'current s' player is the one we want to find the best move for (the AI)
    -- and this player is trying to maximize their score.
    minimaxAlphaBeta (StateMove{state = s, move = (-1, -1), eval = 0, moveDepth = searchDepth}) maxMin searchDepth (-inf -1) (inf + 1) -- Initial alpha, beta


-- minimax wybierający następny ruch