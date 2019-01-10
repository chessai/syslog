module UdpServer
  ( serveLog
  , printHandler
  ) where

import Control.Monad (forever)
import Data.Bits ()
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString (recvFrom)
import qualified Data.ByteString.Char8 as BC8

type HandlerFunc = SockAddr -> String -> IO ()

serveLog ::
     String
  -> HandlerFunc
  -> IO ()
serveLog port handlerFunc = withSocketsDo $ do
  -- look up the port. either raises an exception or returns
  -- a nonempty list.
  serveraddr:_ <- getAddrInfo
    (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
    Nothing
    (Just port)

  -- create a socket
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

  -- bind it to the address we're listening to
  bind sock (addrAddress serveraddr)

  -- loop forever, processing incoming data.
  processMessages sock
  where
    processMessages sock = forever $ do
      -- receive one UDP packet, maximum length 1024 bytes,
      -- and save its content into msg and its source
      -- IP and port into addr
      (msg, addr) <- recvFrom sock 1024
      -- Handle it
      handlerFunc addr (BC8.unpack msg)

printHandler :: HandlerFunc
printHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ msg
