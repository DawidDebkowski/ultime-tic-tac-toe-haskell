module Main where
import State
import Board

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
      putStrLn "Enter move as: sub-board(1-9) cell(1-9), e.g. '5 3':"
      line <- getLine
      case parseMove line of
        Just mv | checkMove st mv -> gameLoop (makeMove st mv)
        _ -> putStrLn "Invalid move, try again." >> gameLoop st

-- main
main:: IO();
main = gameLoop startingState;