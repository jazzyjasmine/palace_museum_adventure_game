# The Palace Museum Adventure

Dec 10 2021

## Rulebook

### Background

The game is based on the Palace Museum (Forbidden City), a former Chinese imperial palace complex in Beijing, China. I chose this as the background of the game to memorize the joy I spent in Beijing last year.

Eight rooms from the Palace Museum were selected to build the map of this game. Each room contains some items for you to discover. They also exist in real world! 

### How to start

1. Open your console
2. Navigate to the palace_museum_adventure_game folder
3. ```stack build```
4. ```stack exec palace_museum_adventure_game```

### How to win

The crown of treasures gallery is missing. The thief mistakenly dropped it on the Imperial Garden when they escaped. You need to go to the garden to retrieve the crown, take it with you, and bring it back to the treasures gallery. 

You don't need to drop the crown in the treasure gallery to win. As long as you show up in the treasure gallery with the crown in your inventory, you win!

### Rooms

At the beginning, a game map is randomly generated and you are in a Chinese restuarant in the Palace Museum. There are a total of eight rooms in the game (including the restuarant):

- Bronzeware Gallery
- Sculpture Gallery
- Gift Shop
- Ceramics Gallery
- Treasures Gallery
- Furniture Gallery
- Imperial Garden
- Restuarant

In the game map, the above rooms will be represented by their capital initials. For example, ```F``` in the game map refers to the Furniture Gallery, ```G``` in the game map refers to the Gift Shop.

### Actions and commands

For each room you are located in, you can check the items and exits of that room, take items (after you successfully take an item, the item appears in your inventory and disappears in the room), drop items to the room (after you successfully drop an item, the item appears in your current room and disappears in your inventory), and go to another room by moving to the direction of an exit. It is guaranteed that all eight rooms are accessible.

When taking items, note that the max weight you can take is 100, i.e. the sum of the item weights in your inventory should never exceed 100. For example, the weight of a wardrobe is 200, which means you can never take this item.

Here are the actions you can take during the game and the corresponding commands:

| Action | Command |
| ------ | ------ |
| check the game map | map |
| check the items and direction of exits of the room you are in | look |
| check the items in your inventory | inventory |
| move to a certain direction | north/south/east/west |
| take one item | take [item] |
| take two or more items | take [item1],[item2],[item3] |
| drop one item | drop [item] |
| drop two or more items | drop [item1],[item2],[item3] |
| end the game | quit |

Note:
- All commands should be in lower case.
- Type a command after the prompt "->", then press "Enter" to fire it up.
- For multiple actions, use "and" to connect the commands. For example: ```take bun and north```, ```north and south and west and east```
- When checking the map, your current location is the room with a ```*``` left to its initial. Other rooms only have their initials shown on the map, without the ```*```.
