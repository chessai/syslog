{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module UdpClient
  ( SyslogHandle(..)
  , openlog
  , syslog
  , closelog
  ) where

import Data.Bits (shiftL, (.|.))
import Network.Socket (Socket, SockAddr, HostName, getAddrInfo, socket, defaultProtocol, addrFamily, SocketType(..), close, addrAddress)
import Network.Socket.ByteString (sendTo)
import Syslog.Types (Facility(..), Priority(..), facilityToCode)
import qualified Data.ByteString.Char8 as BC8

data SyslogHandle = SyslogHandle
  { slSocket :: Socket
  , slProgram :: String
  , slAddress :: SockAddr
  } deriving (Eq, Show)

openlog ::
     HostName
  -> String
  -> String
  -> IO SyslogHandle
openlog hostname port progname = do
  -- look up the hostname and port. Either raises an exception
  -- or returns a nonempty list. First element in that list
  -- is supposed to be the best option.
  serverAddr:_ <- getAddrInfo Nothing (Just hostname) (Just port)

  -- establish a socket for communication
  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol

  pure (SyslogHandle sock progname (addrAddress serverAddr))

syslog ::
     SyslogHandle
  -> Facility
  -> Priority
  -> String
  -> IO ()
syslog SyslogHandle{..} fac pri msg = sendStr sendMsg
  where
    code = makeSyslogCode fac pri
    sendMsg = "<" ++ show code ++ ">" ++ slProgram ++ ": " ++ msg
    -- send until everything is done
    sendStr :: String -> IO ()
    sendStr = \case
      [] -> pure ()
      omsg -> do {
          sent <- sendTo slSocket (BC8.pack omsg) slAddress
        ; sendStr (drop sent omsg)
      }

closelog :: SyslogHandle -> IO ()
closelog SyslogHandle{..} = close slSocket

makeSyslogCode :: Facility -> Priority -> Int
makeSyslogCode fac pri =
  let facCode = facilityToCode fac
      priCode = fromEnum pri
  in (facCode `shiftL` 3) .|. priCode
