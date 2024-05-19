module World where

-- Changes made to the following section of the code was done by kb267
data Object = Obj
  { obj_name :: String,
    obj_longname :: String,
    obj_desc :: String,
    isHeavy :: Bool -- heavy objects can't be picked up
  }
  deriving (Eq, Read, Show)

printObjData :: Object -> String
printObjData obj = obj_longname obj

data Exit = Exit
  { exit_dir :: Direction,
    exit_desc :: String,
    room :: String
  }
  deriving (Eq, Read, Show)

data Room = Room
  { room_desc :: String,
    exits :: [Exit],
    objects :: [Object]
  }
  deriving (Eq, Read, Show)

data GameData = GameData
  { location_id :: String, -- where player is
    world :: [(String, Room)],
    inventory :: [Object], -- objects player has
    poured :: Bool, -- coffee is poured
    caffeinated :: Bool, -- coffee is drunk
    finished :: Bool, -- set to True at the end
    knowCombination :: Bool, -- TV has been watched
    isDead :: Bool -- monster has been examined
  }
  deriving (Show, Read, Eq)

-- Code ends here

won :: GameData -> Bool
won gd = location_id gd == "street"

-- Changes made to the following section of the code was done by kb267
printRoomData :: Room -> IO ()
printRoomData (Room desc exits objs) =
  putStrLn
    ( desc ++ "\n" ++ concatMap exit_desc exits
        ++ showInv objs
    )
  where
    showInv [] = ""
    showInv xs = "You can see: " ++ showInv' xs
    showInv' [x] = printObjData x
    showInv' (x : xs) = printObjData x ++ ", " ++ showInv' xs

-- Code ends here

-- Things which do something to an object and update the game state
type Action = String -> GameData -> (GameData, String)

-- Things which just update the game state
type Command = GameData -> (GameData, String)

-- Directions
data Direction = North | South | East | West | Out | In deriving (Show, Eq, Read)

data Function
  = Go Direction
  | Get Object
  | Put Object
  | Drop Object
  | Pour Object
  | Examine Object
  | Drink Object
  | Open [String]
  | Fix Object
  | Inventory
  | Quit
  deriving (Show, Eq, Read)

mug, fullmug, coffeepot, tv, dinerStorage, dinerStorageUnlocked, basementKey, brokenStatue, fixedStatue, brokenStatuePart, storageKey, kitchenStorage, kitchenStorageUnlocked, coffeeBeans, monster :: Object
mug = Obj "mug" "a coffee mug" "A coffee mug" False
fullmug = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee" False
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee" False

-- The following objects were added by kb267 and tweaked by mmc23
tv = Obj "tv" "a TV playing a video-tape" "The video-tape reminds you of the safe password!" True
dinerStorage = Obj "safe" "a combination safe you keep your basement key in" "The safe is locked. You remember recording the password." True
dinerStorageUnlocked = Obj "safe" "an open safe" "The safe is now open. You've taken the contents out." True
basementKey = Obj "basement_key" "a key to the basement (Use \"basement_key\" to interact)" "A key that can open your basement's door." False
brokenStatue = Obj "statue" "a broken statue holding something" "The kitchen cupboard key is stuck in the hands of the broken statue. If you fixed it you could get the key." True
fixedStatue = Obj "statue" "a fixed statue" "Fixing the statue allowed you to get the key out of it!" True
brokenStatuePart = Obj "hand" "a stone hand" "A stone hand that looks similar to the other one on the statue in your diner." False
storageKey = Obj "storage_key" "a key to the kitchen cupboard (Use \"storage_key\" to pick up)" "A key that can open your kitchen's cupboard." False
kitchenStorage = Obj "cupboard" "the cupboard you keep your coffee pot in" "The cupboard is locked." True
kitchenStorageUnlocked = Obj "cupboard" "an open cupboard" "The cupboard is now unlocked. You took your coffee pot out of it." True

-- Monster object that can end the game if examined
monster = Obj "monster" "a big, angry monster that glares when you get too close" "When you try get a closer look at it, the monster suddenly growls and lunges at you! You have died!" True

-- Red herring objects
coffeeBeans = Obj "beans" "fresh coffee beans" "The coffee beans you store in the basement. Luckily you already brewed a pot this morning and left it in the cupboard." False

-- The code ends here


-- Changes made to the following section of the code was done by kb267
bedroom, kitchen, hall, street, hallExtension, diner, basement :: Room
bedroom =
  Room
    "You are in your bedroom."
    [Exit North "To the north is a kitchen. " "kitchen"]
    [mug]
kitchen =
  Room
    "You are in the kitchen. The back door is closed."
    [ Exit South "To the south is your bedroom. " "bedroom",
      Exit East "To the east is a hallway. " "hall"
    ]
    [kitchenStorage]
hall =
  Room
    "You are in the hallway. The front door is closed. The basement door is locked."
    [ Exit North "To the north is the front door. " "locked",
      Exit West "To the west is a kitchen. " "kitchen",
      Exit East "To the east is the hallway extension. " "hall extension",
      Exit South "To the south is the door to the basement. " "locked"
    ]
    []

-- Code ends here
-- The following rooms were added by kb267 and tweaked by mmc23
hallExtension =
  Room
    "You are in the later part of the hallway."
    [ Exit West "To the west is the main hallway. " "hall",
      Exit North "To the north is a useless room. " "useless",
      Exit East "To the east is a useless room. " "useless",
      Exit South "To the south is the diner. " "diner"
    ]
    []

diner =
  Room
    "You are in the diner."
    [Exit North "To the north is the hallway." "hall extension"]
    [tv, dinerStorage, brokenStatue]

basement =
  Room
    "You are in the basement."
    [Exit North "To the north is the stairs to hallway." "hall"]
    [brokenStatuePart, coffeeBeans, monster]

-- Code ends here

-- New data about the hall for when we open the door

openedBasementHall = "You are in the hallway. The front door is closed. The basement door is unlocked."

openedBasementExits =
  [ Exit North "To the north is the front door. " "locked",
    Exit West "To the west is a kitchen. " "kitchen",
    Exit East "To the east is the hallway extension. " "hall extension",
    Exit South "To the south is the door to the basement. " "basement"
  ]

openedhall = "You are in the hallway. The front door is open. The basement door is unlocked."

openedexits =
  [ Exit West "To the west is a kitchen. " "kitchen",
    Exit Out "You can go outside. " "street",
    Exit East "To the east is the hallway extension. " "hall extension",
    Exit South "To the south is the door to the basement. " "basement"
  ]

street =
  Room
    "You have made it out of the house."
    [Exit In "You can go back inside if you like. " "hall"]
    []

-- Changes made to the following section of the code was done by kb267
gameworld =
  [ ("bedroom", bedroom),
    ("kitchen", kitchen),
    ("hall", hall),
    ("street", street),
    ("hall extension", hallExtension),
    ("diner", diner),
    ("basement", basement)
  ]

gameObjects = [mug, fullmug, coffeepot, tv, dinerStorage, dinerStorageUnlocked, basementKey, brokenStatue, fixedStatue, brokenStatuePart, storageKey, kitchenStorage, kitchenStorageUnlocked, coffeeBeans, monster]

-- Code ends here

initState :: GameData
initState = GameData "bedroom" gameworld [] False False False False False

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))
