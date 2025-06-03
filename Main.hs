module Main (other) where

-- parsowanie ruchu

-- zmiana gracza
other :: Player -> Player
other X = O
other O = X

-- glowna petla

-- main