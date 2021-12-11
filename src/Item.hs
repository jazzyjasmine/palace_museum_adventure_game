module Item where

import qualified Data.Map as M

type Universe = M.Map ItemName Item

data ItemName
  = Vessel
  | Cauldron
  | Figurine
  | Brick
  | Mug
  | Postcard
  | Vase
  | Plate
  | Pearl
  | Bracelet
  | Throne
  | Wardrobe
  | Cobblestone
  | Acorn
  | Crown
  | Bun
  | Dumplings
  deriving (Eq, Ord)

data Item = Item { iname :: ItemName, weight :: Integer } deriving (Show, Eq)

instance Show ItemName where
    show a = 
        case a of 
            Vessel -> "vessel"
            Cauldron -> "cauldron"
            Figurine -> "figurine"
            Brick -> "brick"
            Mug -> "mug"
            Postcard -> "postcard"
            Vase -> "vase"
            Plate -> "plate"
            Pearl -> "pearl"
            Bracelet -> "bracelet"
            Throne -> "throne"
            Wardrobe -> "wardrobe"
            Cobblestone -> "cobblestone"
            Acorn -> "acorn"
            Crown -> "crown"
            Bun -> "bun"
            Dumplings -> "dumplings"

mkUniverse :: [Item] -> Universe
-- take a list of items and construct a Universe
mkUniverse lst = M.fromList (map (\item -> (iname item, item)) lst)

-- initiate items
vessel = Item Vessel 50 
cauldron = Item Cauldron 80
figurine = Item Figurine 20
brick = Item Brick 10
mug = Item Mug 5
postcard = Item Postcard 1
vase = Item Vase 15
plate = Item Plate 5
pearl = Item Pearl 1
bracelet = Item Bracelet 2
throne = Item Throne 120
wardrobe = Item Wardrobe 200
cobblestone = Item Cobblestone 2
acorn = Item Acorn 1
crown = Item Crown 8
bun = Item Bun 3
dumplings = Item Dumplings 3 

univ = mkUniverse [vessel,
                   cauldron,
                   figurine,
                   brick,
                   mug,
                   postcard,
                   vase,
                   plate,
                   pearl,
                   bracelet,
                   throne,
                   wardrobe,
                   cobblestone,
                   acorn,
                   crown,
                   bun,
                   dumplings]

itemNames = M.keys univ
