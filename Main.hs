module Main where
import State
import Board
import MiniMax ( StateMove(move, eval), findBestMove )

-- parsowanie ruchu
parseMove :: String -> Maybe (Int, Int)
parseMove s = case words s of
    [b, c]
        | all valid [b, c] ->
            let smallBoardIndex = (read b) - 1
                cellIndex = (read c) - 1
            in Just (smallBoardIndex, cellIndex)
    _ -> Nothing
  where
    valid t = length t == 1 && head t `elem` ['1'..'9']

-- główna pętla (main loop)
gameLoop :: State -> IO ()
gameLoop st = do
  putStrLn $ showBoard st
  case checkBigBoard (board st) of
    Just (Just p) -> putStrLn $ "Player " ++ show p ++ " wins!"
    Just Nothing  -> putStrLn "Game is a draw!"
    Nothing       -> do
      putStrLn $ "Current: " ++ show (current st)
      putStrLn $ "Play in sub-board: " ++ maybe "any" (show . (+1)) (nextIndex st)
      if current st == O -- AI plays as O
        then do
          putStrLn "AI is thinking..."
          let aiMove = findBestMove st 8 False -- 4 is the search depth, adjust as needed
          putStrLn $ "AI plays: " ++ show (fst (move aiMove) + 1) ++ " " ++ show (snd (move aiMove) + 1) ++ " " ++ show (eval aiMove)
          gameLoop (makeMove st (move aiMove))
        else do
          putStrLn "Enter move as: sub-board(1-9) cell(1-9), e.g. '5 3':"
          line <- getLine
          case parseMove line of
            Just mv | checkMove st mv -> gameLoop (makeMove st mv)
            _ -> putStrLn "Invalid move, try again." >> gameLoop st

botGameLoop :: State -> IO ()
botGameLoop st = do
  putStrLn $ showBoard st
  case checkBigBoard (board st) of
    Just (Just p) -> putStrLn $ "Player " ++ show p ++ " wins!"
    Just Nothing  -> putStrLn "Game is a draw!"
    Nothing       -> do
      putStrLn $ "Current: " ++ show (current st)
      putStrLn $ "Play in sub-board: " ++ maybe "any" (show . (+1)) (nextIndex st)
      putStrLn $ "AI (" ++ show (current st) ++ ") is thinking..." ++ show (current st == X)
      let aiMove = findBestMove st 4 (current st == X) -- Adjust depth for speed/performance
      putStrLn $ "AI (" ++ show (current st) ++ ") plays: " ++ show (fst (move aiMove) + 1) ++ " " ++ show (snd (move aiMove) + 1) ++ " | eval: " ++ show (eval aiMove)
      botGameLoop (makeMove st (move aiMove))

-- main
main:: IO();
main = botGameLoop startingState;