module Direction where

data Direction = N | S | E | W deriving (Eq)

instance Show Direction where
    show a = 
        case a of 
            N -> "north"
            S -> "south"
            E -> "east"
            W -> "west"