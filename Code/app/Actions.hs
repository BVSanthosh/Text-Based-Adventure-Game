module Actions where

import World
import Data.List

-- Development of this module was done by mmc23.

-- Parses a String input into an appropriate Action method if one exists.
actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions "fix"     = Just fix
actions _         = Nothing

-- Parses a String input into an appropriate Command method if one exists.
commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

-- Parses a String input into an appropriate Direction type if one exists.
directions :: String -> Maybe Direction
directions "north"   = Just North
directions "south"   = Just South
directions "east"    = Just East
directions "west"    = Just West
directions "out"     = Just Out
directions "in"      = Just In
directions _         = Nothing


{- Given a direction and a room to move from, return the room id in that direction, if it exists.

Finds if the direction has a linked exit and returns its relevant room ID if so. Otherwise returns nothing.
If attempted to be called with an invalid Direction, automatically fails.
-}

move :: Maybe Direction -> Room -> Maybe String
move (Just dir) rm = case find (\exit -> exit_dir exit == dir) (exits rm) of
                               Just exit -> Just (room exit)
                               Nothing -> Nothing
move _ rm  = Nothing


{- Return True if the object appears in the room. 

Checks the ID of each object in the room to see if any match the input ID and returns true if so.
-}

objectHere :: String -> Room -> Bool
objectHere o rm = any (\object -> obj_name object == o) (objects rm)


{- Given an object id and a room description, return a new room description
   without that object 

Uses a filter to remove any objects from the room with the same ID as the one that was input.
-}

removeObject :: String -> Room -> Room
removeObject o rm = rm { objects = filter (\object -> obj_name object /= o) (objects rm) }


{- Given an object and a room description, return a new room description with that object added 

Creates a new room with the new object appended to its list of objects
-}

addObject :: Object -> Room -> Room
addObject o rm = rm { objects = objects rm ++ [o] }


{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') 

Uses a filter to get the desired object only and then uses head to return it.
Although head is not total, due to the assumption the object is in the list it can be used safely here.
-}

findObj :: String -> [Object] -> Object
findObj o ds = let objs = filter (\object -> obj_name object == o) ds
               in head objs


{- Use 'findObj' to find an object in a room description 

Passes the room's objects list to findObj.
-}

objectData :: String -> Room -> Object
objectData o rm = findObj o (objects rm)


{- Given a game state and a room id, replace the old room information with new data. If the room id does not already exist, add it. 

Uses a filter to remove any existing room with the input ID and stores the new data under that ID in its place.
-}

updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = gd {world = updatedWorld}
                            where updatedWorld = filter (\(id, room) -> id /= rmid) (world gd) ++ [(rmid, rmdata)]


{- Given a game state and an object id, find the object in the current room and add it to the player's inventory 

Uses objectData with the current room to get the Object's data and adds it to the inventory.
-}

addInv :: GameData -> String -> GameData
addInv gd obj = gd {inventory = inventory gd ++ [objectData obj (getRoomData gd)]}


{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. 
   
Uses a filter to remove any objects with the same ID as the input one.
-}

removeInv :: GameData -> String -> GameData
removeInv gd obj = gd {inventory = filter (\object -> obj_name object /= obj) (inventory gd) }


{- Does the inventory in the game state contain the given object? 

Checks the inventory to see if any object ID's match the input ID and returns true if so.
-}

carrying :: GameData -> String -> Bool
carrying gd obj = any (\object -> obj_name object == obj) (inventory gd)

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

Uses move to find if the movement is valid and if it is, updates the game state to match.
Useless or locked rooms cannot be entered.   
-}

go :: Action
go dir state = case move (directions dir) (getRoomData state) of
                    Just "useless" -> (state, "You go to enter the room but decide not to since it's so useless!")
                    Just "locked" -> (state, "That door is locked!")
                    Just x -> (state {location_id = x}, "You enter the " ++ x)
                    Nothing -> (state, "No exit that way!")


{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

Checks if the object is in the room and if it is, updates the state step by step adding the object to inventory, removing it from the room and then updating the room in state to match.
Objects marked as heavy cannot be picked up.
-}

get :: Action
get obj state
  -- Object not present case
  | not isObjectHere = (state, "That object isn't here!")

  -- Successful get case
  | not isObjectHeavy = (updatedState, "You pick up the " ++ obj)

  -- Object is too heavy case
  | isObjectHeavy = (state, "That object is too heavy to pick up!")

  | otherwise = (state, "You can't pick that up!")
  where
      room = getRoomData state

      -- CHecks that the object is present in the room and whether it is heavy or not
      isObjectHere = objectHere obj room
      isObjectHeavy = isHeavy (objectData obj room)

      -- Updating the inventory to add the object
      holdingObjectState = addInv state obj

      -- Updating the room to remove the object
      roomWithoutItem = removeObject obj room

      -- Updating the state to have the new information for the room
      updatedState
        = updateRoom holdingObjectState (location_id state) roomWithoutItem


{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.

Checks if the object is in inventory and if it is, updates the state step by step removing the object from inventory, adding it from the room and then updating the room in state to match.
-}
put :: Action
put obj state = if carrying state obj then (updatedState, "You put down the " ++ obj)
                else (state, "You're not holding that!")
                where room = getRoomData state
                      -- Updating the inventory to remove the object
                      notHoldingState = removeInv state obj

                      -- Updating the room to add the object
                      roomWithItem = addObject (findObj obj (inventory state)) room

                      -- Updating the state to have the new information for the room
                      updatedState = updateRoom notHoldingState (location_id state) roomWithItem


{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's inventory! 

Checks if the object is in either the room or inventory and returns its description if it is.
-}

examine :: Action
examine obj state = case find (\object -> obj_name object == obj) (objects room ++ inventory state) of

                         -- Case for examining the TV and learning the safe code
                         Just object -> if obj_name object == "tv" then (state {knowCombination = True}, obj_desc object)

                         -- Case for examining the monster and losing the game 
                                        else if obj_name object == "monster" then (state {isDead = True}, obj_desc object)
                                        
                         -- Case for a valid examine               
                                        else (state, obj_desc object)
                         Nothing -> (state, "That object isn't here!")
                    where
                       room = getRoomData state


{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".

Checks if the requirements are met and updates the game state appropriately if so.
-}

pour :: Action
pour obj state
-- Already poured state
  | poured state = (state, "The coffee has already been poured!")

-- Have coffee but not a mug state
  | not gotObjects && obj == "coffee" = (state, "You need something to pour the coffe into!")

-- Successful pour state
  | gotObjects && obj == "coffee" = (updatedState, "You pour the coffee into the mug.")
  | otherwise = (state, "You need something to pour and something to pour it into!")
  where
      gotObjects = carrying state "mug" && carrying state "coffee"
      updatedState
        = state
            {poured = True,
             inventory = inventory (removeInv state "mug") ++ [fullmug]}


{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.
   Also, put the empty coffee mug back in the inventory!

Checks if the requirements are met and updates the game state appropriately if so.
-}

drink :: Action
drink obj state
   | obj /= "mug" && obj /= "coffee" = (state, "You can't drink out of that!")
   | caffeinated state = (state, "You've already had your coffee!")
   | not (poured state) = (state, "There's no coffee in the mug!")
   | hasFullMug && (obj == "mug" || obj == "coffee") = (updatedState, "You drink the coffee and feel full of energy!")
   | otherwise = (state, "You can't drink that!")
   where
      hasFullMug = carrying state "mug" && poured state
      updatedState
        = state
            {caffeinated = True,
             inventory = inventory (removeInv state "mug") ++ [mug]}


{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

Checks if the requirements are met and updates the game state appropriately if so.
Handles opening of the front door, basement door, safe and cupboard.
-}

open :: Action
open obj state
-- Invalid target case
   | obj /= "door" && obj /= "cupboard" && obj /= "safe" && obj /= "basement" = (state, "You can't open that!")

-- Front Door Cases
-- Not caffeinated case
   | not (caffeinated state) && obj == "door" = (state, "You're feeling too tired to leave... if only there was some coffee.")

-- Successful opening case
   | canOpenDoor && obj == "door" = (openDoorState, "You open the front door!")


-- Basement Door Cases
-- No key case
   | not (carrying state "basement_key") && obj == "basement" = (state, "You don't have the key!")

-- Basement already opened case
   | basementIsOpened && obj == "basement" = (state, "The basement is already open!")

-- Successful opening case
   | canOpenBasement &&  obj == "basement" = (openBasementState, "You open the basement door!")

-- Diner Safe Cases
-- Don't know combination case
   | not (knowCombination state) && obj == "safe" = (state, "You don't know the combination!")

-- Successful opening case
   | canOpenDinerStorage && obj == "safe" = (openStorageAndKeyState, "You open the safe and find the basement key!")

-- Kitchen Cupboard Cases
-- No key case
   | not (carrying state "storage_key") && obj == "cupboard" = (state, "You don't have the key!")

-- Successful opening case
   | canOpenKitchenStorage && obj == "cupboard" = (openStorageAndCoffeeState, "You open the cupboard and find the coffee pot!")

-- General failure case
   | otherwise = (state, "You fail to open the " ++ obj)
   where
      room = getRoomData state


      -- Opening hallway door section
      canOpenDoor = location_id state == "hall" && caffeinated state

      openDoorState
        = updateRoom state "hall" (Room openedhall openedexits (objects (getRoomData state)))

      -- Opening basement door section
      canOpenBasement = location_id state == "hall" && carrying state "basement_key"

      basementIsOpened = move (Just South) room == Just "basement"

      openBasementState
        = updateRoom state "hall" (Room openedBasementHall openedBasementExits (objects (getRoomData state)))

      -- Opening safe section
      canOpenDinerStorage = location_id state == "diner" && knowCombination state

      openDinerStorageRoom = addObject dinerStorageUnlocked (removeObject "safe" room)

      openStorageAndKeyState = updateRoom state "diner" (addObject basementKey openDinerStorageRoom)

      -- Opening kitchen storage section
      canOpenKitchenStorage = location_id state == "kitchen" && carrying state "storage_key"

      openKitchenStorageRoom = addObject kitchenStorageUnlocked (removeObject "cupboard" room)

      openStorageAndCoffeeState = updateRoom state "kitchen" (addObject coffeepot openKitchenStorageRoom)


{- Fix the statue. Only allowed if the player has the hand

Checks if the requirements are met and updates the game state appropriately if so.
-}

fix :: Action
fix obj state
  | carrying state "hand" && obj == "statue" = (fixedStatueAndKeyState, "You fix the statue and free the key!")
  | otherwise = (state, "You can't fix that!")
  where
      room = getRoomData state

      fixedStatueRoom
        = addObject fixedStatue (removeObject "statue" room)
      fixedStatueAndKeyState
        = updateRoom (state {inventory = inventory (removeInv state "hand")}) "diner" (addObject storageKey fixedStatueRoom)


{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")

