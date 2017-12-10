module Control.Concurrent.NanoErl.Broadcast
  ( GroupProcess
  , GroupRef
  , spawnGroup
  , (!*)
  , broadcastExcept
  ) where

import           Control.Concurrent.NanoErl
import           Control.Monad
import           Data.IORef
import           Data.List                  ((\\))


-- | A ref holding all processes in the group
type GroupRef message = IORef [Pid message]

-- | A group process takes its group ref (for broadcasts) and its own Pid (for receiving)
type GroupProcess message = GroupRef message -> Process message


-- | Spawn a group of actors and return the group's ref (as passed to the actors in the group)
spawnGroup :: [GroupProcess message] -> IO (GroupRef message)
spawnGroup as = do
  gref <- newIORef [] :: IO (GroupRef message)
  -- XXX it's not safe to just start all the actors with an empty group
  pids <- mapM (\a -> spawn (a gref)) as
  writeIORef gref pids
  return gref


-- | Broadcast a message to everyone in the group (including ourselves)
(!*) :: GroupRef message -> message -> IO ()
(!*) gref msg = do
  pids <- readIORef gref
  mapM_ (! msg) pids


-- | Broadcast a message to everyone in the group except a list of Pids
broadcastExcept :: GroupRef message -> [Pid message] -> message -> IO ()
broadcastExcept gref notpids msg = do
  pids <- readIORef gref
  mapM_ (! msg) (pids \\ notpids)
