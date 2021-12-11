module Command where

import Text.Parsec hiding (parse, runParser, (<|>))
import qualified Text.Parsec as P 
import Text.Parsec.String (Parser)

import Data.Char
import Data.List

import Item
import Direction

parse :: Parser a -> String -> Either ParseError a 
parse prsr = P.parse prsr ""

(<|>) :: Parser a -> Parser a -> Parser a
prsr1 <|> prsr2 = (P.<|>) (try prsr1) prsr2

data Command
  = Inventory
  | Look
  | Drop [ItemName]
  | Take [ItemName]
  | Move Direction
  | Map
  | Exit
  deriving (Eq, Show)

type Conjunction = [Command]

itemNameP :: Parser ItemName
-- parse string representing an ItemName to an inhabitant of the ItemName datatype
itemNameP = foldl (<|>) (parserFail "") $ fmap singleItemP itemNames

singleItemP :: ItemName -> Parser ItemName
-- take in an ItemName and return a Parser ItemName
singleItemP iname = pure iname 
                    <* optional spacesP
                    <* (string $ show iname)
                    <* optional spacesP

spacesP :: Parser String
-- parse whitespaces
spacesP = many1 $ char ' '

nounPhrase_stub :: Parser [ItemName]
-- take an alphabetic string off the front of the input and puts it into a list
nounPhrase_stub = fmap (\x -> [x]) itemNameP

nounPhrase :: Parser [ItemName]
-- parse a comma-separated list of nouns
nounPhrase = sepBy1 itemNameP (char ',')

inventoryP :: Parser Command
-- accept the string "inventory" and rejects everything else
inventoryP = pure Inventory
            <* optional spacesP
            <* (string "inventory")
            <* optional spacesP

takeP :: Parser Command
-- parse the word "take" plus a noun phrase into a Command
takeP = pure Take
        <* optional spacesP
        <* (string "take")
        <* char ' '
        <* optional spacesP
        <*> nounPhrase
        <* optional spacesP

exitP :: Parser Command
-- parse the command to quit the game
exitP = pure Exit
        <* optional spacesP
        <* string "exit"
        <|> string "quit"
        <* optional spacesP
        
dropP :: Parser Command
-- parse the word "drop" plus a noun phrase into a Command
dropP = pure Drop
        <* optional spacesP
        <* (string "drop")
        <* char ' '
        <* optional spacesP
        <*> nounPhrase
        <* optional spacesP

lookP :: Parser Command
-- accept the string "look" and reject everything else
lookP = pure Look
            <* optional spacesP
            <* (string "look")
            <* optional spacesP

directionP :: Parser Direction
-- accept a single lowercase word denoting a direction
directionP = foldl (<|>) (parserFail "") $ fmap singleDirectionP [N, S, E, W]

singleDirectionP :: Direction -> Parser Direction
-- take in a Direction and return a Parser Direction
singleDirectionP direction = pure direction
                            <* optional spacesP
                            <* (string $ show direction)
                            <* optional spacesP

moveP :: Parser Command
-- parse a move command
moveP = pure Move
        <* optional spacesP
        <*> directionP
        <* optional spacesP

mapP :: Parser Command
-- accept the string "map" and rejects everything else
mapP = pure Map
        <* optional spacesP
        <* (string "map")
        <* optional spacesP

commandP :: Parser Command
-- parse commands of the game
commandP = optional spacesP
           *> takeP
           <|> moveP
           <|> inventoryP
           <|> lookP
           <|> exitP
           <|> takeP
           <|> dropP
           <|> mapP
           <* optional spacesP 

conjunctionP :: Parser Conjunction
-- parse a list of commands of the game
conjunctionP = sepBy1 commandP (string "and") <* eof

parseInput :: String -> Maybe Conjunction
-- parse the user's input
parseInput input = case parse conjunctionP input of
                    Right conjunction -> Just conjunction
                    Left _ -> Nothing