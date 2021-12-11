module GameState where

import Data.List
import Data.Char
import Data.Maybe
import Control.Exception
import qualified Data.Map as M

import Item
import Room
import Player
import Direction

type GameMap = M.Map RoomName Room
type Point = (Int, Int)
type GameGrid = M.Map Point Room

data GameState = GameState { message :: Maybe String
                            , gmap :: GameMap
                            , universe :: Universe
                            , player :: Player
                            , grid :: GameGrid } deriving (Show)

mkMap :: [Room] -> GameMap
-- take a list of Rooms and construct a GameMap
mkMap lst = M.fromList $ map (\room -> (rname room, room)) lst

gameMap :: GameMap 
-- load all rooms into a GameMap
gameMap = mkMap allRooms

data KeyError = KeyError
  deriving Show

instance Exception KeyError

getObjectUniv :: ItemName -> Universe -> Item
-- get an item from a universe
getObjectUniv iname u
  = case M.lookup iname u of
      Just obj -> obj
      Nothing -> throw KeyError

getObject :: ItemName -> GameState -> Item
-- get an item from a gamestate
getObject iname st = getObjectUniv iname (universe st)

getRoomMap :: RoomName -> GameMap -> Room
-- get room by roomName from gamemap
getRoomMap rname mp
  = case M.lookup rname mp of
      Just room -> room
      Nothing -> throw KeyError

getRoom :: RoomName -> GameState -> Room
-- get room by roomName from gamestate
getRoom name st = getRoomMap name (gmap st)

setRoomMap :: RoomName -> Room -> GameMap -> GameMap
-- replace an existed room in the gamemap
setRoomMap oldRoomName newRoom oldGameMap =
    M.adjust (\_ -> newRoom) oldRoomName oldGameMap

setMessage :: String -> GameState -> GameState
-- set the message of a gamestate
setMessage newMessage gameState =
    if length newMessage /= 0
        then gameState { message = Just newMessage }
    else gameState { message = Nothing }

currentInventory :: GameState -> [ItemName]
-- get the inventory of the input gamestate's player.
currentInventory gameState = inventory $ player gameState

currentRoom :: GameState -> Room
-- get the room the player is located at in the input gamestate
currentRoom gameState = getRoom (location $ player gameState) gameState

nearbyObjects :: GameState -> [ItemName]
-- get the item names in the room where the player is
nearbyObjects gameState = objects $ currentRoom gameState

rawTakeItem :: ItemName -> GameState -> GameState
-- helper function for takeItem, take item without checking the logic
rawTakeItem itemName gameState = 
  let curRoomName = location $ player gameState
      newRoom = Room.removeItem itemName $ currentRoom gameState
      newRoomMap = setRoomMap curRoomName newRoom $ gmap gameState
      newPlayer = Player.addItem itemName $ player gameState
      tmpGameState = gameState { gmap = newRoomMap
                                , player = newPlayer }
      newMessage = "You take the " ++ (show itemName) ++ "."
  in setMessage newMessage tmpGameState

takeItem :: ItemName -> GameState -> GameState
-- perform takeItem after checking the logic
takeItem iname gs = let filteredTmpResult = Right gs >>= alreadyHaveTakeCheck iname 
                                                     >>= inRoomTakeCheck iname
                                                     >>= weightCheck iname
                    in case filteredTmpResult of 
                        Left errorMessage -> gs {message = Just errorMessage}
                        Right gs -> rawTakeItem iname gs

rawDropItem :: ItemName -> GameState -> GameState
-- helper function for dropItem, drop item without checking the logic
rawDropItem itemName gameState = 
  let curRoomName = location $ player gameState
      newRoom = Room.addItem itemName $ currentRoom gameState
      newRoomMap = setRoomMap curRoomName newRoom $ gmap gameState
      newPlayer = Player.removeItem itemName $ player gameState
      tmpGameState = gameState { gmap = newRoomMap
                                , player = newPlayer }
      newMessage = "You drop the " ++ (show itemName) ++ "."
  in setMessage newMessage tmpGameState

dropItem :: ItemName -> GameState -> GameState
-- perform dropItem after checking the logic
dropItem iname gs = let filteredTmpResult = Right gs >>= anywhereDropCheck iname 
                                                     >>= inRoomDropCheck iname
                    in case filteredTmpResult of 
                        Left errorMessage -> gs {message = Just errorMessage}
                        Right gs -> rawDropItem iname gs

inventoryWeight :: GameState -> Integer
-- returns total weight of all inventory item of the player who defined in the GameState
inventoryWeight gameState =  sum $ map weight items
  where
    items = map (\iname -> getObject iname gameState) $ currentInventory gameState

-- Error Handling with Either
type Error a = Either String a

alreadyHaveTakeCheck :: ItemName -> GameState -> Error GameState
-- if the player has carried the item returns Left error message; otherwise Right GameState
alreadyHaveTakeCheck iname gs
  | elem iname (inventory $ player gs)
  = Left ("You are already carrying the " ++ (show iname) ++ "." :: String)
  | otherwise = Right gs

inRoomTakeCheck :: ItemName -> GameState -> Error GameState
-- if the item is in the same room as player return gameState, if not return error message
inRoomTakeCheck iname gs
  | elem iname (nearbyObjects gs)
  = Right gs
  | otherwise = Left ("There is no " ++ (show iname) ++ " in this room." :: String)

weightCheck :: ItemName -> GameState -> Error GameState
-- check if adding the new item to player inventory leads to exceeding max weight
weightCheck iname gs
  | weight (getObject iname gs) + inventoryWeight gs > (maxWeight $ player gs)
  = Left "That's too much weight for you to carry."
  | otherwise = Right gs

anywhereDropCheck :: ItemName -> GameState -> Error GameState
-- check wether the item is either in player's inventory or in current room
anywhereDropCheck iname gs
  | (elem iname $ inventory $ player gs) || (elem iname $ nearbyObjects gs)
  = Right gs
  | otherwise =  Left ("What do you mean, drop the \"" ++ (show iname) ++ "\"?" :: String)

inRoomDropCheck :: ItemName -> GameState -> Error GameState
-- check whether the item is in the current room
inRoomDropCheck iname gs
  | elem iname $ nearbyObjects gs
  = Left ("You aren't carrying the " ++ (show iname) ++ "." :: String)
  | otherwise = Right gs

roomHasObjects :: GameState -> Bool
-- returns true if the room has any objects, false otherwise
roomHasObjects gs = hasObjects $ currentRoom gs

destinationName :: Direction -> Room -> Maybe RoomName
-- output the room name that player would end up in going the input direction
destinationName direction room = 
  let matched = filter (\(currentDirection, _) -> currentDirection == direction) $ exits room
      getRoomHelper [(direction, room)] = room
  in if length matched == 0
        then Nothing
     else Just $ getRoomHelper matched

move :: Direction -> GameState -> GameState
-- move the player to the input direction
move direction gs = case destinationName direction $ currentRoom gs of
                    Nothing -> setMessage "There is no exit in that direction" gs
                    Just newRoom -> gs { message = Just $ "you go " ++ (show direction) ++ ".",
                                         player = newLocation newRoom $ player gs }

haveWonGame :: GameState -> Bool
-- True if the gamestate is in an winning condition, False otherwise
haveWonGame gs = rname (currentRoom gs) == TreasuresGallery && elem Crown (currentInventory gs)

displayExit :: Direction -> Room -> String
-- provide string representation of the exit in that direction
displayExit direction room = 
  let stringDirection direction = case direction of 
                                    N -> "|"
                                    S -> "|"
                                    E -> "-"
                                    W -> "-"
  in case elem direction $ getExitsDirections room of
       True -> stringDirection direction
       False -> " "

roomLetter :: Maybe RoomName -> Room -> String
-- display the room in string 
roomLetter mbrname room = 
  let 
    isSameRoom roomname room = roomname == rname room
    getCapital room = [Data.Char.toUpper $ (show $ rname room) !! 0]
    roomletterHelperForJust roomname room =
      if isSameRoom roomname room then "*" ++ getCapital room
      else " " ++ getCapital room
  in case mbrname of 
    Nothing -> " " ++ getCapital room
    Just roomname -> roomletterHelperForJust roomname room

showGridList :: GameGrid -> [[Room]]
-- break a GameGrid up into list of lists by the order in the grid
showGridList gamegrid = 
  let 
    isSameRow (row1, col1) (row2, col2) = row1 == row2
    groupedPoints = Data.List.groupBy isSameRow $ Data.List.sort $ M.keys gamegrid
    getRoomByPoint point = Data.Maybe.fromJust $ M.lookup point gamegrid
  in map (map getRoomByPoint) groupedPoints

showRow :: Maybe RoomName -> [Room] -> String
-- return the string representation of a row of rooms
showRow mbrname rooms = 
  let 
    firstLine = "  " ++ concat (intersperse "   " $ Data.List.map (displayExit N) rooms)
    fromRoomToString room = (displayExit W room) ++ 
                            (roomLetter mbrname room) ++ 
                            (displayExit E room)
    secondLine = concat (intersperse "" $ Data.List.map fromRoomToString rooms)
    thirdLine = "  " ++ concat (intersperse "   " $ Data.List.map (displayExit S) rooms)
  in firstLine ++ "\n" ++ secondLine ++ "\n" ++ thirdLine ++ "\n"

showGrid :: Maybe RoomName -> GameGrid -> String
-- return a string representation of a GameGrid
showGrid mbrname gamegrid = 
  let gridList = showGridList gamegrid
  in concat $ Data.List.map (showRow mbrname) gridList
