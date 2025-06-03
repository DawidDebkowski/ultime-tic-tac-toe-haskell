import Data.List (intercalate)
import Data.Maybe (isJust, fromMaybe)

-- | Player representation
data Player = X | O deriving (Eq, Show)

-- | Each cell is either Empty or occupied by a Player
data Cell = Empty | Taken Player deriving (Eq)

instance Show Cell where
  show Empty        = "."
  show (Taken X)    = "X"
  show (Taken O)    = "O"

-- | A 3x3 tic-tac-toe board (sub-board)
type SubBoard = [Cell]  -- length 9

-- | The global board is 3x3 of sub-boards, plus meta-state of wins
type Board = [SubBoard]  -- length 9

type Meta = [Maybe Player]  -- which sub-board is won by whom

data GameState = GameState
  { board      :: Board      -- all 9 sub-boards
  , meta       :: Meta       -- status of each sub-board
  , current    :: Player     -- who plays now
  , nextIndex  :: Maybe Int  -- which sub-board index the next move must play in
  }

-- | Initialize empty game state
initialState :: GameState
initialState = GameState
  { board     = replicate 9 (replicate 9 Empty)
  , meta      = replicate 9 Nothing
  , current   = X
  , nextIndex = Nothing
  }

-- | Pretty-print a sub-board
showSub :: SubBoard -> [String]
showSub sb = [ row r | r <- [0..2] ]
  where row r = unwords [ show (sb !! (r*3 + c)) | c <- [0..2] ]

-- | Pretty-print full ultimate board
showBoard :: GameState -> String
showBoard st = intercalate "\n" . concat $ [ blockRows r | r <- [0..2] ]
  where
    bs = board st
    blockRows br = [ intercalate " | " [ showSub (bs !! (br*3 + bc)) !! r | bc <- [0..2] ]
                   | r <- [0..2]
                   ]
                   ++ [ if br<2 then replicate 22 '-' else "" ]

-- | All winning triples in a 3x3
wins :: [[Int]]
wins = rows ++ cols ++ dias
  where
    rows = [[0,1,2],[3,4,5],[6,7,8]]
    cols = [[0,3,6],[1,4,7],[2,5,8]]
    dias = [[0,4,8],[2,4,6]]

-- | Check if a sub-board is won or drawn
checkSub :: SubBoard -> Maybe (Maybe Player)
checkSub sb
  | any threeX wins = Just (Just X)
  | any threeO wins = Just (Just O)
  | Empty `notElem` sb = Just Nothing  -- draw
  | otherwise = Nothing                -- still playable
  where
    threeX = all ((== Taken X) . (sb !!))
    threeO = all ((== Taken O) . (sb !!))

-- | Update meta from board
updateMeta :: Board -> Meta
updateMeta bs = map (>>= id) detailed
  where
    detailed = map checkSub bs

-- | Check overall game win on meta
checkMeta :: Meta -> Maybe (Maybe Player)
checkMeta m
  | any winX wins = Just (Just X)
  | any winO wins = Just (Just O)
  | all isJust m  = Just Nothing
  | otherwise     = Nothing
  where
    winX = all ((== Just X) . (m !!))
    winO = all ((== Just O) . (m !!))

-- | Convert user input to indices
parseMove :: String -> Maybe (Int,Int)
parseMove s = case words s of
  [b, c]
    | all valid [b, c]
    -> let bi = read b - 1; ci = read c - 1 in Just (bi, ci)
  _ -> Nothing
  where
    valid t = length t == 1 && head t `elem` ['1'..'9']

-- | Check if move is valid
validMove :: GameState -> (Int,Int) -> Bool
validMove st (bi,ci) = inRange bi && inRange ci
  && maybe True (==bi) (nextIndex st)
  && meta st !! bi == Nothing
  && board st !! bi !! ci == Empty
  where inRange x = x >= 0 && x < 9

-- | Apply a move
makeMove :: GameState -> (Int,Int) -> GameState
makeMove st (bi,ci) = st'
  where
    bs'   = updateBoard (board st) bi ci (Taken (current st))
    meta' = updateMeta bs'
    next  = if meta' !! ci == Nothing then Just ci else Nothing
    st'   = GameState bs' meta' (other (current st)) next

updateBoard :: Board -> Int -> Int -> Cell -> Board
updateBoard bs bi ci val = take bi bs ++ [newSb] ++ drop (bi+1) bs
  where
    sb = bs !! bi
    newSb = take ci sb ++ [val] ++ drop (ci+1) sb

other :: Player -> Player
other X = O
other O = X

-- | Main game loop
gameLoop :: GameState -> IO ()
gameLoop st = do
  putStrLn $ showBoard st
  case checkMeta (meta st) of
    Just (Just p) -> putStrLn $ "Player " ++ show p ++ " wins!"
    Just Nothing  -> putStrLn "Game is a draw!"
    Nothing       -> do
      putStrLn $ "Current: " ++ show (current st)
      putStrLn $ "Play in sub-board: " ++ maybe "any" (show . (+1)) (nextIndex st)
      putStrLn "Enter move as: sub-board(1-9) cell(1-9), e.g. '5 3':"
      line <- getLine
      case parseMove line of
        Just mv | validMove st mv -> gameLoop (makeMove st mv)
        _ -> putStrLn "Invalid move, try again." >> gameLoop st

-- | Kick off the game
main :: IO ()
main = gameLoop initialState