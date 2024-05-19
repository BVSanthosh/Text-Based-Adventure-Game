-- The following module was coded by kb267

module SaveRW where

import Actions
import System.IO
import World
import Control.Exception

-- Save file name
saveFileName = "save.sv"

-- Save Error handler
saveError :: IOError -> IO ()
saveError e = 
  putStrLn "Oh Oh! Cannot save the game please try again!"

-- Load Error handler
loadError :: IOError -> IO String
loadError e =
  do
    putStrLn "Oh Oh! Cannot load the game! \n Resetting the save point to initial point."
    addSavePoint initState saveFileName
    return (dataToString initState) 

-- Reads save from save.sv
readSavePoint :: String -> IO GameData
readSavePoint svName = do
  fileContent <- readFile svName `catch` loadError
  let lineList = lines fileContent
  let gameData = restoreGameData lineList
  putStr (forceRead fileContent)
  return gameData

forceRead [] = ""
forceRead (x:xs) = forceRead xs

-- Restores the game data from save before being sent back
restoreGameData lineList =
  GameData
    { location_id = lineList !! 0,
      world = read (lineList !! 1) :: [(String,Room)],
      inventory = [findObj oName gameObjects  | oName <- read (lineList !! 2)],
      poured = read (lineList !! 3),
      caffeinated = read (lineList !! 4),
      finished = read (lineList !! 5),
      knowCombination = read (lineList !! 6),
      isDead = read (lineList !! 7)
    }


-- Adds a save point in save.sv
addSavePoint :: GameData -> String -> IO ()
addSavePoint dat svName =
  do
    writeFile svName string `catch` saveError
    putStrLn "Save complete"
  where
    string = dataToString dat

-- Converts data to String for save purposes
dataToString :: GameData -> String
dataToString dat = location_id dat ++ "\n" ++ show (world dat) ++ "\n" ++ show ([obj_name o | o <- inventory dat]) ++ "\n" ++ show (poured dat) ++ "\n" ++ show (caffeinated dat) ++ "\n" ++ show (finished dat) ++ "\n" ++ show (knowCombination dat) ++ "\n" ++ show (isDead dat)

-- Code ends here