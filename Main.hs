module Main (other) where

-- parsowanie ruchu
parseMove :: String -> Maybe(Int, Int)
parseMove s = case words s of 
	[b,c]
		| all valid [b,c]
		-> let smallBoardIndex = read b-1; cellIndex = c-1 in Just (smallBoardIndex, cellIndex)
		_ -> Nothing
	where
		valid t = lenght t == 1 && head t 'elem' ['1'..'9']

-- zmiana gracza
other :: Player -> Player
other X = O
other O = X

-- glowna petla

-- main