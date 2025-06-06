module Main where
import State
import Board
import MiniMax ( StateMove(move, eval), findBestMove )
import MiniMax ( StateMove(move, eval), findBestMove )
import Data.Sequences (toLower)

validateSingleInt :: String -> [Char] -> Bool
validateSingleInt c range = length c == 1 && head c `elem` range

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

-- główna pętla (main loop)
oneBotGameLoop :: State -> Player -> IO ()
oneBotGameLoop st botPlayer = do
  putStrLn $ showBoard st
  case checkBigBoard (board st) of
    Just (Just p) -> putStrLn $ "Player " ++ show p ++ " wins!"
    Just Nothing  -> putStrLn "Game is a draw!"
    Nothing       -> do
      putStrLn $ "Current: " ++ show (current st)
      putStrLn $ "Play in sub-board: " ++ maybe "any" (show . (+1)) (nextIndex st)
      if current st == botPlayer -- AI plays as O
        then do
          putStr "AI is thinking..."
          let aiMove = findBestMove st 4 (current st == X) -- 4 is the search depth, adjust as needed
          putStrLn $ if eval aiMove < 100 then " and thinking..." else " and is deeply troubled..."
          putStrLn $ "AI plays: " ++ show (fst (move aiMove) + 1) ++ " " ++ show (snd (move aiMove) + 1) ++ " " ++ show (eval aiMove)
          oneBotGameLoop (makeMove st (move aiMove)) botPlayer 
        else do
          putStrLn "Enter move as: sub-board(1-9) cell(1-9), e.g. '5 3':"
          line <- getLine
          case parseMove line of
            Just mv | checkMove st mv -> oneBotGameLoop (makeMove st mv) botPlayer
            _ -> putStrLn "Invalid move, try again." >> oneBotGameLoop st botPlayer

botGameLoop :: State -> IO ()
botGameLoop st = do
  putStrLn $ showBoard st
  case checkBigBoard (board st) of
    Just (Just p) -> putStrLn $ "Player " ++ show p ++ " wins!"
    Just Nothing  -> putStrLn "Game is a draw!"
    Nothing       -> do
      putStrLn $ "Current: " ++ show (current st)
      putStrLn $ "Play in sub-board: " ++ maybe "any" (show . (+1)) (nextIndex st)
      putStr $ "AI (" ++ show (current st) ++ ")"
      let aiMove = findBestMove st 4 (current st == X) -- Adjust depth for speed/performance
      putStrLn (if eval aiMove < 100 then " is thinking..." else " is deeply troubled...")
      putStrLn $ "AI (" ++ show (current st) ++ ") plays: " ++ show (fst (move aiMove) + 1) ++ " " ++ show (snd (move aiMove) + 1) ++ " | eval: " ++ show (eval aiMove)
      botGameLoop (makeMove st (move aiMove))

-- main
main:: IO();
main = do
  putStrLn "Enter the number of human players {0, 1, 2}: "
  line <- getLine
  case line of
    "2" -> gameLoop startingState
    "1" -> do
      putStrLn "Who do you want to play?: "
      line <- getLine
      case toLower line of
        "x" -> oneBotGameLoop startingState X
        "o" -> oneBotGameLoop startingState O
        "y" -> oneBotGameLoop startingState O
        _   -> putStrLn "Invalid input, please enter X or O" >> main
    "0" -> botGameLoop startingState
    _   -> putStrLn "Invalid input, please enter 0, 1, or 2." >> main