module Player where

import Item
import Room

data Player = Player { inventory :: [ItemName]
                      , maxWeight :: Integer
                      , location :: RoomName } deriving (Show, Eq)


addItem :: ItemName -> Player -> Player
-- add an item to a player's inventory
addItem itemName player = Player (itemName : inventory player) (maxWeight player) (location player)


removeItem :: ItemName -> Player -> Player
-- remove an item from a player's inventory
removeItem itemName player = Player (foldl (\newLst element -> if element /= itemName then element : newLst else newLst) [] (inventory player)) (maxWeight player) (location player)


newLocation :: RoomName -> Player -> Player
-- change the player's location
newLocation roomName player = player {location = roomName}

isCarryingAnything :: Player -> Bool
-- True if that player's inventory has something in it, and False otherwise
isCarryingAnything player = length (inventory player) > 0 

you = Player [] 100 Restuarant