module Control.Concurrent.NanoErl.Examples.BroadcastPingPong where

import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.NanoErl
import           Control.Concurrent.NanoErl.Broadcast
import           Control.Monad
import           System.Random


data Message = PingMessage Int | PongMessage Int


-- bcast ping
pingActor :: GroupProcess Message
pingActor gref mypid = forever $ do
  threadDelay 1000000
  r <- randomRIO (0, 9)
  broadcastExcept gref [mypid] (PingMessage r)


-- bcast pong (dies on dienum)
pongActor :: Int -> GroupProcess Message
pongActor dienum gref mypid =
  receive mypid $ \msg ->
  case msg of
    -- ping messages: die or pong back
    (PingMessage num) ->
      if num == dienum
      then do
        putStrLn $ "pong actor " ++ show mypid ++ " die (" ++ show num ++ ")"
        kill mypid -- XXX
      else do
        putStrLn $ "pong actor " ++ show mypid ++ " survive (" ++ show num ++ ")"
        r <- randomRIO (0, 9)
        gref !* PongMessage r
        pongActor dienum gref mypid
    -- do nothing on pong messages
    (PongMessage _) -> do
      putStrLn $ "pong actor " ++ show mypid ++ " ignoring pong message"
      pongActor dienum gref mypid

main :: IO ()
main = do
  -- generate some random numbers and construct pong actors
  rs <- mapM (\_ -> randomRIO (0,9)) [1..10]
  let pongers = map pongActor rs

  runNanoErl $ do
    _gref <- spawnGroup (pingActor:pongers)
    putStrLn "group spawned"

  return ()
