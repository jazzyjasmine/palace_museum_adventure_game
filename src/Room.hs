module Room where

import Data.List
import Item
import Direction

type Exit = (Direction, RoomName)

data RoomName
  = BronzewareGallery
  | SculptureGallery
  | GiftShop
  | CeramicsGallery
  | TreasuresGallery
  | FurnitureGallery
  | ImperialGarden
  | Restuarant
  deriving (Eq, Ord)

data Room = Room { rname :: RoomName
                  , desc :: String
                  , exits :: [Exit]
                  , objects :: [ItemName] } deriving (Show, Eq)

instance Show RoomName where
    show a = 
        case a of 
            BronzewareGallery -> "bronzeware gallery"
            SculptureGallery -> "sculpture gallery"
            GiftShop -> "gift shop"
            CeramicsGallery -> "ceramics gallery"
            TreasuresGallery -> "treasures gallery"
            FurnitureGallery -> "furniture gallery"
            ImperialGarden -> "imperial garden"
            Restuarant -> "restuarant"

-- Initiate rooms
bronzewareGallery = Room BronzewareGallery
                    "You are in bronzeware gallery!" 
                    [(E, SculptureGallery)]
                    [Vessel, Cauldron]

sculptureGallery = Room SculptureGallery
                  "You are in sculpture gallery!"
                  [(W, BronzewareGallery)
                  ,(E, GiftShop)]
                  [Figurine, Brick]

giftShop = Room GiftShop
          "You are in gift shop!"
          [(W, SculptureGallery)
          ,(S, FurnitureGallery)]
          [Mug, Postcard]

ceramicsGallery = Room CeramicsGallery
                  "You are in ceramics gallery!"
                  [(S, ImperialGarden)
                  ,(E, TreasuresGallery)]
                  [Vase, Plate]

treasuresGallery = Room TreasuresGallery
                  "You are in treasures gallery!"
                  [(W, CeramicsGallery)
                  ,(E, FurnitureGallery)]
                  [Pearl, Bracelet]

furnitureGallery = Room FurnitureGallery
                   "You are in furniture gallery!"
                   [(N, GiftShop)
                   ,(W, TreasuresGallery)]
                   [Throne, Wardrobe]

imperialGarden = Room ImperialGarden
                 "You are in imperial garden!"
                 [(N, CeramicsGallery)
                 ,(E, Restuarant)]
                 [Cobblestone, Acorn, Crown]

restuarant = Room Restuarant
             "You are in a Chinese restuarant!"
             [(W, ImperialGarden)]
             [Bun, Dumplings]

roomNames = map rname allRooms

addItem :: ItemName -> Room -> Room
-- return a new Room with the input ItemName added to its objects record field
addItem itemName room = room { objects = itemName : (objects room) }

removeItem :: ItemName -> Room -> Room
-- return a new Room with the input ItemName removed from its objects record field
removeItem itemName room = room { objects = delete itemName (objects room) }

allRooms :: [Room]
-- list all rooms
allRooms = [bronzewareGallery,
            sculptureGallery,
            giftShop,
            ceramicsGallery,
            treasuresGallery,
            furnitureGallery,
            imperialGarden,
            restuarant]

hasObjects :: Room -> Bool
-- True if the room has any object, False otherwise
hasObjects room = length (objects room) /= 0

hasExits :: Room -> Bool
-- check if a room has exits. True if it has any, False otherwise.
hasExits room = length (exits room) /= 0

getExitsDirections :: Room -> [Direction]
-- get the Directions of all exits in the room
getExitsDirections room = map (\(direction, newRoom) -> direction) (exits room)