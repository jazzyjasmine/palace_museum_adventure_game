module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item

type GameIO a = StateT GameState IO a

effectChange :: (GameState -> GameState) -> GameIO ()
-- output a value of type GameIO that performs the input transition
effectChange transition = do 
    state <- get  
    put $ transition state

prompt :: GameIO ()
-- print the string "->" to the console
prompt = lift $ putStr "->" >> hFlush stdout

printMessage :: GameIO ()
-- print message if there is valid message in the gamestate 
printMessage = do
    state <- get
    case message state of 
        Just msg -> do
            lift $ putStrLn msg 
            effectChange (setMessage "")  
        Nothing -> pure ()

printDescription :: GameIO ()
-- print a description of the room where the player is in the current game state.
printDescription = do
    state <- get
    lift $ putStrLn $ desc $ currentRoom state

printObjects :: GameIO ()
-- print the objects of the room where the player currently locates
printObjects = do
    state <- get
    let fixedOutput = "You see the following objects:"
    case roomHasObjects state of 
        False -> pure ()
        True -> do
            lift $ mapM_ putStrLn $ (fixedOutput : (map show $ nearbyObjects state))

printExits :: GameIO ()
-- print the exits of the room where the player currently is
printExits = do
    state <- get
    let fixedOutput = "There are exits in the following directions:"
        thisRoom = currentRoom state
    case hasExits thisRoom of
        False -> pure ()
        True -> do
            lift $ mapM_ putStrLn $ (fixedOutput : (map show $ getExitsDirections thisRoom))

printInventory :: GameIO ()
-- print the content of the player's inventory
printInventory = do 
    state <- get
    let emptyInventoryNote = "You aren't carrying anything."
        nonemptyInventoryNote = "You are carrying the following objects:"
    case isCarryingAnything $ player state of 
        False -> do
            lift $ putStrLn emptyInventoryNote
        True -> do
            lift $ mapM_ putStrLn $ (nonemptyInventoryNote : (map show $ currentInventory state))

displayMap :: GameIO ()
-- print the map of the current game state, with a star located wherever the player is
displayMap = do
    state <- get
    lift $ putStrLn $ showGrid (Just (rname $ currentRoom state)) (grid state)

actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
-- perform action over a list
actionOverList func inames = do 
    let newFunc iname = effectChange (func iname) >> printMessage
    sequence_ $ fmap newFunc inames

finishGame :: GameIO ()
-- performs the action of printing a success message to the screen, then quits the program.
finishGame = do 
    lift $ mapM_ putStrLn [ "You successfully brought the crown into the treasure gallery!",
                            "Congrats! You win the game!" ]
    lift $ exitSuccess

exit :: GameIO ()
-- print the message "Goodbye!", then exits the game with a zero exit status
exit = do
    lift $ putStrLn "Goodbye!"
    lift $ exitSuccess

checkGameOver :: GameIO ()
-- check whether the current game state is the winning state. 
checkGameOver = do
    state <- get
    case haveWonGame state of 
        False -> pure ()
        True -> do 
            finishGame

syntaxError :: GameIO ()
-- print the message "I don't understand that" 
syntaxError = do 
    lift $ putStrLn "I don't understand that."

opening :: GameIO ()
-- print the welcome message.
opening = do
    let msg = "Welcome to the Palace Museum Adventure! \n"
              ++ "Get the crown in the Imperial Garden"
              ++ " and bring it to the Treasures Gallery!"
    lift $ putStrLn msg

performCommand :: Command -> GameIO ()
-- take any Command as an input, and execute the action
performCommand command = case command of 
    Inventory -> printInventory
    Look -> printDescription >> printObjects >> printExits
    Drop inames -> actionOverList dropItem inames
    Take inames -> actionOverList takeItem inames
    Move direction -> effectChange (move direction) >> printMessage
    Map -> displayMap
    Exit -> exit

performConjunction :: Conjunction -> GameIO ()
-- perform every command in a Conjunction in order.
performConjunction conjunction = sequence_ $ fmap performCommand conjunction

parseConjunction :: String -> GameIO ()
-- parse an input string and run the commands
parseConjunction input = case parseInput input of 
    Just conjunction -> performConjunction conjunction
    Nothing -> syntaxError

repl :: GameIO ()
-- performs one round of prompting, executing commands and checking if game is over
repl = do 
    prompt
    input <- lift $ getLine
    parseConjunction input
    checkGameOver