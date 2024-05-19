-- The following module was coded by kb267
{-# LANGUAGE TemplateHaskell #-}
module Main where
import Actions
import Parsing
import World
import SaveRW
import Test.QuickCheck
import Data.Maybe (isJust)
import Control.Monad.IO.Class
import Test.QuickCheck.Monadic
import Test.QuickCheck.All
import System.Directory

-- Helper convesrion to convert directions to string for move and go tests
directionsToString :: Maybe Direction -> String
directionsToString (Just North) = "north"
directionsToString (Just South) = "south"
directionsToString (Just West) = "west"
directionsToString (Just East) = "east"
directionsToString (Just Out) = "out"
directionsToString (Just In) = "in"
directionsToString Nothing = "";

-- Helper conversion for the interacting with heavy objects test
stringToRoom :: String -> Room
stringToRoom "bedroom" = bedroom
stringToRoom "kitchen" = kitchen
stringToRoom "basement" = basement
stringToRoom "hall" = hall
stringToRoom "hall extension" = hallExtension
stringToRoom "diner" = diner
stringToRoom _ = bedroom

-- Helper states to test state/gameData related functionalities
testState1,testState2,testState3,testState4,testState5 :: GameData
testState1 = GameData "kitchen" gameworld [coffeepot] False False False False False
testState2 = GameData "hall" gameworld [] False False False False False
testState3 = GameData "basement" gameworld [coffeeBeans] False False False False False
testState4 = GameData "diner" gameworld [] False False False False False
testState5 = GameData "hall extension" gameworld [] False False False False False

-- Helper function to test object related functionalities
printObjName :: Object -> String
printObjName obj = obj_name obj


-- Making testable data-types 
-- This was inspired by the code found at: 
-- Vasconcelos, P. (2021). Property Testing using QuickCheck. [online] Fc.up.pt. Available at: https://www.dcc.fc.up.pt/~pbv/aulas/tapf/handouts/quickcheck.html [Accessed 9 Feb. 2022].
instance Arbitrary Room where
    arbitrary = elements [bedroom,kitchen,hall,hallExtension,diner,basement]

instance Arbitrary Direction where
    arbitrary = elements [North,South,East,West,In,Out]

instance Arbitrary GameData where
    arbitrary = elements [initState, testState1,testState2,testState3,testState4,testState5]

instance Arbitrary Object where
    arbitrary = elements [mug, fullmug, coffeepot, tv, dinerStorage, dinerStorageUnlocked, basementKey, brokenStatue, fixedStatue, brokenStatuePart, storageKey, kitchenStorage, kitchenStorageUnlocked, coffeeBeans, monster]
-- code ends here

-- South to the bedroom is no exit
-- Checking the move function specififc case!
prop_move_specific_1 :: Property
prop_move_specific_1 =
    move (Just South) bedroom === Nothing

-- No room has two exits in the same way
-- e.g. the kitchen does not have two south exits 
prop_move :: Room -> Room -> Maybe Direction -> Property
prop_move room room1 dir=
    (room /= room1) ==>
        isJust(move dir room) && isJust(move dir room1) ==>
            move dir room /= move dir room1

-- Checking that locked rooms do not let the state be changed 
prop_go :: Maybe Direction -> Maybe Direction -> GameData -> Property
prop_go dir dir1 state=
    (dir /= dir1 && isJust dir && isJust dir1) ==>
        snd(go (directionsToString dir) state)  == "That door is locked!" ==>
             location_id (fst(go (directionsToString dir) state))  == location_id state

-- Checking that the location updates if the move is legal
prop_location :: Maybe Direction -> GameData -> Property
prop_location dir state=
    (isJust dir && fst(go (directionsToString dir) state) /= state) ==>
           location_id (fst (go (directionsToString dir) state)) /= location_id state

-- Heavy objects cannot be picked up
prop_InteractHeavyObjects :: Object -> GameData -> Property
prop_InteractHeavyObjects object state =
    (isHeavy object && objectHere (printObjName object) (stringToRoom (location_id state))) ==>
        get (printObjName object) state == (state, "That object is too heavy to pick up!")

-- Tests of the save file functions saves the game data correctly
prop_SaveWrite :: GameData -> Property
prop_SaveWrite dat = monadicIO $ do
    run (addSavePoint dat "temp.sv")
    test <- run (readFile "temp.sv")
    assert (dataToString dat == test)
    run (removeFile "temp.sv")

-- Now that we are assured addSavePoint works we use that to test readSavePoint
prop_SaveRead :: GameData -> Property
prop_SaveRead dat = monadicIO $ do
    run (addSavePoint dat "temp.sv")
    test <- run (readSavePoint "temp.sv")
    assert (dat == test)
    run (removeFile "temp.sv")


prop_addObject :: Object -> Room -> Property
prop_addObject object room =
    notElem object (objects room) ==>
    objects (addObject object room) /= objects room


prop_removeObject :: Object -> Room -> Property
prop_removeObject object room =
    object `elem` objects room ==>
    objects (removeObject (obj_name object) room) /= objects room


prop_checkObj :: Object -> Room -> Bool
prop_checkObj object room =
    addObject object room /= removeObject (obj_name object) room

prop_getObj :: Object -> GameData -> Property
prop_getObj object state =
    snd (get (obj_name object) state) == ("You pick up the "++obj_name object) ==>
    inventory (fst(get (obj_name object) state)) /= inventory state


prop_putObj :: Object -> GameData -> Property
prop_putObj object state =
    carrying state (obj_name object) ==>
    inventory (fst(put (obj_name object) state)) /= inventory state


-- The following section of code was inspired by the following article:
-- Guillaume Chérel (2017). Complete minimal example for using quickCheckAll. [online] Stack Overflow. Available at: https://stackoverflow.com/questions/42669087/complete-minimal-example-for-using-quickcheckall [Accessed 9 Feb. 2022].
return []
check :: IO Bool
check = $quickCheckAll

main :: IO Bool
main = check
-- Code ends here

-- Code ends here