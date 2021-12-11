module Example where 

import Data.Array.IO
import Control.Monad
import Data.Maybe
import Data.List 
import System.Random
import qualified Data.Map as M

import Item
import Direction
import Room
import Player
import GameState

newtype Set = Set [Point] deriving (Show, Eq)

choose :: [a] -> IO a
-- output a random element from any list
choose lst = do
  let randomIndex = randomRIO (0, (length lst) - 1)
  randomIndexPure <- randomIndex
  return (lst !! randomIndexPure)

shuffle :: [a] -> IO [a]
-- shuffle a list
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

mkSet :: (Int, Int) -> Set
-- take a point and return that same point enclosed in a singleton set
mkSet point = Set [point]

zeroExits :: Room -> Room
-- take a room and return the same room with its exits deleted
zeroExits room = room { exits = [] }

exampleGrid :: [Room] -> IO GameGrid
-- take a list of rooms and build a randomly-arranged square grid
exampleGrid rooms = do
  let 
    numberOfRooms = length rooms
    indexes = [0..numberOfRooms - 1]
    rowScope = ceiling $ sqrt $ fromIntegral numberOfRooms
    indexesToGamegrid rms = map (\index -> 
                                ((div index rowScope, mod index rowScope), rms !! index)) indexes
  shuffledRooms <- shuffle rooms
  return $ M.fromList $ indexesToGamegrid shuffledRooms

neighbors :: Point -> Point -> Bool
-- True if two points are neighbors (vertically or horizontally), False otherwise
neighbors (r1, c1) (r2, c2) = (r1 == r2 && abs(c1 - c2) == 1) || 
                              (c1 == c2 && abs(r1 - r2) == 1)

possibleLinks :: Set -> Set -> [(Point, Point)]
-- return a list of all pairs of points that are neighbors of each other
possibleLinks (Set sp1) (Set sp2) = [ (point1, point2) | point1 <- sp1,
                                                         point2 <- sp2,
                                                         neighbors point1 point2 ]

orientation :: Point -> Point -> Maybe Direction
-- return the direction moving from the first grid point to the second grid point 
orientation (r1, c1) (r2, c2) = 
  if r1 == r2 && c1 < c2 then Just E else
  if r1 == r2 && c1 > c2 then Just W else
  if c1 == c2 && r1 < r2 then Just S else
  if c1 == c2 && r1 > r2 then Just N else Nothing

addExit :: Room -> Direction -> Room -> Room
-- add an exit going in the input direction from source room to the destination room
addExit desroom direction sourceroom = 
  let sourceExits = exits sourceroom
  in sourceroom { exits = (direction, rname desroom) : sourceExits }

nodeLink :: Point -> Point -> GameGrid -> Maybe GameGrid
-- link the exits in the rooms located at the two input points
nodeLink p1 p2 gamegrid = 
  let 
    pointsOfGamegrid = M.keys gamegrid
    p1DirectionWithoutJust = fromJust $ orientation p1 p2
    p2DirectionWithoughJust = fromJust $ orientation p2 p1
    p1Room = fromJust $ M.lookup p1 gamegrid
    p2Room = fromJust $ M.lookup p2 gamegrid
    isAlreadyLinked = case destinationName p1DirectionWithoutJust p1Room of 
                        Nothing -> False
                        Just _ -> True
    newP1Room = addExit p2Room p1DirectionWithoutJust p1Room
    newP2Room = addExit p1Room p2DirectionWithoughJust p2Room
  in if not (neighbors p1 p2) || not (elem p1 pointsOfGamegrid) || not (elem p2 pointsOfGamegrid)
        then Nothing
     else if isAlreadyLinked 
            then Just $ gamegrid
          else Just $ M.adjust (\_ -> newP2Room) p2 $ M.adjust (\_ -> newP1Room) p1 gamegrid

setNeighbors :: Set -> Set -> Bool
-- True if there are any neighboring points, False otherwise
setNeighbors set1 set2 = length (possibleLinks set1 set2) /= 0

setUnion :: Set -> Set -> Set
-- union two sets into a new set
setUnion (Set sp1) (Set sp2) = Set $ Data.List.union sp1 sp2

setLink :: Set -> Set -> GameGrid -> IO (Maybe GameGrid)
-- return a gamegrid with two exits linking rooms located at a pair of neighboring points
setLink s1 s2 gamegrid = do
  selectedPair <- choose $ possibleLinks s1 s2
  let nodeLinkPair (p1, p2) gg = nodeLink p1 p2 gg
  if not $ setNeighbors s1 s2 
    then return Nothing
  else return $ nodeLinkPair selectedPair gamegrid

updateSetList :: Set -> Set -> [Set] -> [Set]
-- delete the two input sets and add the union of the two input sets
updateSetList s1 s2 slist = 
  let 
    unionSet = setUnion s1 s2 
    deleteSet (Set points1) (Set points2) setlist = filter 
                                      (\(Set points) -> 
                                        sort points1 /= sort points &&
                                        sort points2 /= sort points) setlist
  in unionSet : (deleteSet s1 s2 slist)

oneRound :: [Set] -> GameGrid -> IO (Maybe GameGrid, Set, Set)
-- perform a single round of Kruskal's algorithm
oneRound slist gg = do
  let twoSetsWithNeighbor = [ (s1, s2) | s1 <- slist,
                                         s2 <- slist,
                                         setNeighbors s1 s2 ]
      getFirst (fst, snd) = fst
      getSecond (fst, snd) = snd
  selectedSetPair <- choose $ twoSetsWithNeighbor
  newgg <- setLink (getFirst selectedSetPair) (getSecond selectedSetPair) gg
  return $ (newgg, getFirst selectedSetPair, getSecond selectedSetPair)

generateMap :: [Set] -> GameGrid -> IO GameGrid
-- keep performing oneRound until there is only one set left in the list
generateMap slist gg = 
  if length slist == 1 then do return gg
  else do
    (Just newGamegrid, set1, set2) <- oneRound slist gg
    generateMap (updateSetList set1 set2 slist) newGamegrid

randomMap :: IO GameGrid
-- construct a randomly-generated map out of the allRooms constant
randomMap = do
  let emptyRooms = Data.List.map zeroExits allRooms
  gamegrid <- exampleGrid emptyRooms
  generateMap (Data.List.map mkSet (M.keys gamegrid)) gamegrid 

gridToMap :: GameGrid -> GameMap
-- get a room lookup table from a gamegrid
gridToMap gg = 
  let 
    ggList = M.toList gg
    rooms = Data.List.map (\(k, v) -> v) ggList
  in 
    mkMap rooms

class Example a where 
  example :: IO a 

instance Example GameState where 
  example = do
    gg <- randomMap
    let gm = gridToMap gg
    return $ GameState Nothing gm univ you gg
