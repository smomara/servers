module SpeedDaemon.Types where

import Control.Concurrent.Async (Async)
import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Word (Word16, Word32)
import Network.Simple.TCP (Socket)

type Str = ByteString
type Plate = ByteString
type Road = Word16
type Mile = Word16
type Speed = Word16
type Timestamp = Word32
type Interval = Word32
type Day = Word32

data Camera = Camera
  { cameraRoad :: Road
  , cameraMile :: Mile
  , cameraLimit :: Speed
  }
  deriving (Show)

newtype Dispatcher = Dispatcher
  { dispatcherRoads :: [Road]
  }
  deriving (Show)

data Observation = Observation
  { obsPlate :: Plate
  , obsRoad :: Road
  , obsMile :: Mile
  , obsTimestamp :: Timestamp
  }
  deriving (Show)

data Ticket = Ticket
  { ticketPlate :: Plate
  , ticketRoad :: Road
  , ticketMile1 :: Mile
  , ticketTimestamp1 :: Timestamp
  , ticketMile2 :: Mile
  , ticketTimestamp2 :: Timestamp
  , ticketSpeed :: Speed -- Speed is 100x miles per hour
  }
  deriving (Show)

data Message
  = ErrorMsg Str
  | PlateMsg Plate Timestamp
  | TicketMsg Ticket
  | WantHeartbeat Interval
  | Heartbeat
  | IAmCamera Camera
  | IAmDispatcher Dispatcher
  deriving (Show)

data TicketDay = TicketDay
  { tdPlate :: Plate
  , tdDay :: Day
  } deriving (Eq, Ord)

data Server = Server
  { cameras :: TVar (Map Road [CameraClient])
  , dispatchers :: TVar (Map Road [DispatcherClient])
  , observations :: TVar (Map Plate [Observation])
  , tickets :: TVar (Map Road [Ticket])
  , ticketDays :: TVar (Set TicketDay)
  }

data ClientState = ClientState
  { clientSocket :: Socket
  , clientInterval :: TVar (Maybe Interval)
  , clientHeartbeatThread :: TVar (Maybe (Async ()))
  , clientBuffer :: TVar ByteString
  }

data CameraClient = CameraClient
  { ccState :: ClientState
  , ccCamera :: Camera
  }

data DispatcherClient = DispatcherClient
  { dcState :: ClientState
  , dcDispatcher :: Dispatcher
  }
