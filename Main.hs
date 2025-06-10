module Main where
import State
import Board
import MiniMax ( StateMove(move, eval), findBestMove )
import System.IO (hFlush, stdout)

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

-- główna pętla PvP
gameLoop :: State -> IO ()
gameLoop st = do
  putStrLn $ showBoard st
  case checkBigBoard (board st) of
    Just (Just p) -> putStrLn $ "Player " ++ show p ++ " wins!"
    Just Nothing  -> putStrLn "Game is a draw!"
    Nothing       -> do
      putStrLn $ "Current: " ++ show (current st)
      putStrLn $ "Play in small board: " ++ maybe "any" (show . (+1)) (nextIndex st)
      putStrLn "Enter move as: small board(1-9) cell(1-9), e.g. '5 3':"
      line <- getLine
      case parseMove line of
        Just mv | checkMove st mv -> gameLoop (makeMove st mv)
        _ -> putStrLn "Invalid move, try again." >> gameLoop st

-- główna pętla PvE
oneBotGameLoop :: State -> Player -> IO ()
oneBotGameLoop st botPlayer = do
  putStrLn $ showBoard st
  case checkBigBoard (board st) of
    Just (Just p) -> putStrLn $ "Player " ++ show p ++ " wins!"
    Just Nothing  -> putStrLn "Game is a draw!"
    Nothing       -> do
      putStrLn $ "Current: " ++ show (current st)
      putStrLn $ "Play in small board: " ++ maybe "any" (show . (+1)) (nextIndex st)
      if current st == botPlayer
        then do
          putStr "AI is thinking..."
          hFlush stdout
          let aiMove = findBestMove st 4 (current st == X)
          putStrLn $ case botPlayer of
                X -> if eval aiMove > 100 then " and feels confident." else if eval aiMove < -100 then " and is deeply troubled..." else " "
                O -> if eval aiMove < -100 then " and feels confident." else if eval aiMove > 100 then " and is deeply troubled..." else " "
          putStrLn $ "AI plays: " ++ show (fst (move aiMove) + 1) ++ " " ++ show (snd (move aiMove) + 1) ++ " " ++ show (eval aiMove)
          oneBotGameLoop (makeMove st (move aiMove)) botPlayer 
        else do
          putStrLn "Enter move as: small board(1-9) cell(1-9), e.g. '5 3':"
          line <- getLine
          case parseMove line of
            Just mv | checkMove st mv -> oneBotGameLoop (makeMove st mv) botPlayer
            _ -> putStrLn "Invalid move, try again." >> oneBotGameLoop st botPlayer

-- EvE
botGameLoop :: State -> Int -> IO ()
botGameLoop st turn = do
  putStrLn $ showBoard st
  case checkBigBoard (board st) of
    Just (Just p) -> putStrLn $ "Player " ++ show p ++ " wins!"
    Just Nothing  -> putStrLn "Game is a draw!"
    Nothing       -> do
      putStrLn $ "Current: " ++ show (current st)
      putStrLn $ "Play in small board: " ++ maybe "any" (show . (+1)) (nextIndex st)
      putStr $ "AI (" ++ show (current st) ++ ") is thinking... " -- ++ show (current st == X)
      hFlush stdout
      case turn of
        0 -> do
          putStrLn " "
          botGameLoop (makeMove st (0, 0)) (turn+1)
        1 -> do
          putStrLn " "
          botGameLoop (makeMove st (0, 1)) (turn+1)
        2 -> do
          putStrLn " " 
          botGameLoop (makeMove st (1, 1)) (turn+1)
        3 -> do
          putStrLn " "
          botGameLoop (makeMove st (1, 2)) (turn+1)
        _ -> do
              let aiMove = findBestMove st 5 (current st == X)
              putStrLn $ case current st of
                        X -> if eval aiMove > 100 then " and feels confident." else if eval aiMove < -100 then " and is deeply troubled..." else " "
                        O -> if eval aiMove < -100 then " and feels confident." else if eval aiMove > 100 then " and is deeply troubled..." else " "
              putStrLn $ "AI (" ++ show (current st) ++ ") plays: " ++ show (fst (move aiMove) + 1) ++ " " ++ show (snd (move aiMove) + 1) ++ " | eval: " ++ show (eval aiMove)
              botGameLoop (makeMove st (move aiMove)) (turn+1)

toLowerChar :: Char -> Char
toLowerChar c
    | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + offset)
    | otherwise            = c
  where
    offset :: Int
    offset = fromEnum 'a' - fromEnum 'A'

toLower :: String -> String
toLower = map toLowerChar

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
        "x" -> oneBotGameLoop startingState O
        "o" -> oneBotGameLoop startingState X
        "y" -> oneBotGameLoop startingState X
        _   -> putStrLn "Invalid input, please enter X or O" >> main
    "0" -> botGameLoop startingState 0
    _   -> putStrLn "Invalid input, please enter 0, 1, or 2." >> main