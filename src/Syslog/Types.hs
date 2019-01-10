{-# LANGUAGE LambdaCase #-}

module Syslog.Types
  ( Priority(..)
  , Facility(..)
  , facilityToCode
  , codeToFacility
  ) where

-- | A 'Priority' defines the relative importance of the log message.
data Priority
  = DEBUG     -- ^ Debug messages
  | INFO      -- ^ Information
  | NOTICE    -- ^ Normal runtime conditions
  | WARNING   -- ^ General warnings
  | ERROR     -- ^ General errors
  | CRITICAL  -- ^ Servere situations
  | ALERT     -- ^ Take immediate action
  | EMERGENCY -- ^ System is unusable
  deriving (Eq, Ord, Show, Read, Enum)

data Facility
  = KERN     -- ^ Kernel messages
  | USER     -- ^ General userland messages
  | MAIL     -- ^ E-Mail system
  | DAEMON   -- ^ Daemon (server process) messages
  | AUTH     -- ^ Authentication or security messages
  | SYSLOG   -- ^ Internal syslog messages
  | LPR      -- ^ Printer messages
  | NEWS     -- ^ Usenet news
  | UUCP     -- ^ UUCP messages
  | CRON     -- ^ Cron messages
  | AUTHPRIV -- ^ Private authentication messages
  | FTP      -- ^ FTP messages
  | LOCAL0
  | LOCAL1
  | LOCAL2
  | LOCAL3
  | LOCAL4
  | LOCAL5
  | LOCAL6
  | LOCAL7
  deriving (Eq, Show, Read)

facilityToCode :: Facility -> Int
facilityToCode = \case
  KERN -> 0
  USER -> 1
  MAIL -> 2
  DAEMON -> 3
  AUTH -> 4
  SYSLOG -> 5
  LPR -> 6
  NEWS -> 7
  UUCP -> 8
  CRON -> 9
  AUTHPRIV -> 10
  FTP -> 11
  LOCAL0 -> 16
  LOCAL1 -> 17
  LOCAL2 -> 18
  LOCAL3 -> 19
  LOCAL4 -> 20
  LOCAL5 -> 21
  LOCAL6 -> 22
  LOCAL7 -> 23

codeToFacility :: Int -> Maybe Facility
codeToFacility = \case
  0 -> Just KERN
  1 -> Just USER
  2 -> Just MAIL
  3 -> Just DAEMON
  4 -> Just AUTH
  5 -> Just SYSLOG
  6 -> Just LPR
  7 -> Just NEWS
  8 -> Just UUCP
  9 -> Just CRON
  10 -> Just AUTHPRIV
  11 -> Just FTP
  16 -> Just LOCAL0
  17 -> Just LOCAL1
  18 -> Just LOCAL2
  19 -> Just LOCAL3
  20 -> Just LOCAL4
  21 -> Just LOCAL5
  22 -> Just LOCAL6
  23 -> Just LOCAL7
  _  -> Nothing
