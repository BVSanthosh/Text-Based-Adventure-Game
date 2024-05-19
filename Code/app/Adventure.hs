{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Main where

import World
import Actions
import SaveRW

import Control.Monad
import System.IO
import System.Exit

import System.Console.Haskeline
import Control.Monad.IO.Class (MonadIO(liftIO))

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

losemessage = "Unfortunately, you never made it out of the house...\n" ++
              "Luckily, we implemented a save feature!"             

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case actions cmd of
                            Just go -> go arg state
                            Just open -> open arg state
                            Just drink -> drink arg state
                            Just pour -> pour arg state
                            Just examine -> examine arg state
                            Just put -> put arg state
                            Just get -> get arg state
                            Just fix -> fix arg state
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case commands cmd of
                            Just inventory -> inv state
                            Just quit -> quit state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

-- Basic repl code designed by sb409
-- repl :: GameData -> IO GameData
-- repl state | finished state = return state
-- repl state = do 
--                 save <- saveIt state 
--                 printRoomData (getRoomData state)
--                 putStr "What now? "
--                 hFlush stdout
--                 cmd <- getLine
--                 let (state', msg) = process state (words cmd)
--                 putStrLn msg
--                 if (won state') then do putStrLn winmessage
--                                         return state'
--                                else repl state'

-- main :: IO ()
-- main = do 
--          load <- loadIt
--          repl load
--          return ()

saveIt :: GameData -> IO ()
saveIt dat = addSavePoint dat saveFileName 

loadIt :: IO GameData
loadIt = readSavePoint saveFileName

-- The following section of code was added by kb267 and was inspired by the following:
-- The basic repl code designed by sb409 commented above for proof
-- Haskell.org. (2022). System.Console.Haskeline. [online] Available at: https://hackage.haskell.org/package/haskeline-0.8.2/docs/System-Console-Haskeline.html#g:1 [Accessed 5 Feb. 2022].

main :: IO ()
main = do
   load <- loadIt
   --runInputT defaultSettings (loop load)
   runInputT defaultSettings (loop initState)
   where
       loop :: GameData -> InputT IO ()
       loop state= do
           liftIO (printRoomData (getRoomData state))
           minput <- getInputLine "What now? "
           case minput of
               Nothing -> return ()
               Just "quit" -> return () -- Don't know if we should keep this
               Just "save" -> do
                   liftIO (saveIt state)
                   loop state
               Just "reset" -> do
                   liftIO(saveIt initState)
                   loop state
               Just "load" -> do
                   state' <- liftIO loadIt
                   loop state'
               Just input -> do
                           let (state', msg) = process state (words input)
                           outputStrLn msg
                           -- Won game case, game still continues but the correct message is output
                           if won state' then do
                                         outputStrLn winmessage
                                         loop state'
                               -- Lost game case, loop is exited with an appropriate message          
                               else if isDead state' then do 
                                                   outputStrLn losemessage
                                   -- Continuing game loop normally if neither end condition has been triggered                
                                   else loop state'

-- Code ends here
                              