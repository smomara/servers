# Concurrent Speed Cameras: A Haskell Implementation

How I built a concurrent server to catch speeding cars

---

## Introduction

Haskell's approach to concurrent programming offers a unique perspective on managing
shared state and coordinating multiple processes.
While the language's pure functional nature might seem at odds with the inherently
stateful nature of concurrent system,
Haskell provides some powerful abstractions that make concurrent programming
both safer and more intuitive than traditional approaches.

One of my favorite things about Haskell is how its monadic abstractions handle
composable error cases. Consider parsing a binary protocol: in many languages,
you'd need to handle errors at each step, leading to deeply nested error checking.
In Haskell, we can use monads to separate the error handling from the main logic flow:

```python
# traditional imperative approach
def parse_message(bytes):
    if len(bytes) < 1:
        raise ValueError("incomplete message")

    msg_type = bytes[0]
    if msg_type == 0x80: # IAmCamera
        if len(bytes) < 7: raise ValueError("incomplete camera info"
        road = int.from_bytes(bytes[1:3], byteorder='big')
        mile = int.from_bytes(bytes[3:5], byteorder='big')
        limit = int.from_bytes(bytes[5:7], byteorder='big')
    # ...
```

```Haskell
-- Haskell's monadic approach
parseMessage :: Get Message
parseMessage = do
    msgType <- getWord8                   -- Fails automatically if no bytes
    case msgType of
        0x80 -> IAmCamera <$> parseCamera -- Composition handles errors
        0x20 -> PlateMsg <$> parseStr <*> getWord32be
        -- more message types ...
 where
  parseCamera = Camera
    <$> getWord16be -- road
    <*> getWord16be -- mile
    <*> getWord16be -- limit
```

The Haskell version lets us focus on the successful path through the code while the
`Get` monad handles all error cases implicitly. While implicit control flow in
traditional languages can often make things harder to reason about, Haskell's
type system and the design of monads make it easy to ensure correctness at
compile time and maintain clarity in our code.
This pattern of composing operations while handling errors elegantly becomes
very powerful when dealing with concurrent system.

### The Speed Daemon Challenge

The Speed Daemon challenge requires building a TCP server that coordinates
multiple cameras ticket dispatchers to catch speeding drivers. At its core,
it's a concurrent state management problem with some interesting twists.

#### The Binary Protocol

The server communicates using a compact binary protocol with specific message types

```haskell
-- Core message types (hex values)
0x80 -> IAmCamera     -- Camera registration
0x81 -> IAmDispatcher -- Dispatcher registration
0x20 -> Plate         -- License plate observation 
0x21 -> Ticket        -- Speed violation ticket
0x40 -> WantHeartbeat -- Request heartbeat messages
0x41 -> Heartbeat     -- Server heartbeat response
0x10 -> Error         -- Protocol error notification
```

Each message type has a specific binary format.
For example, a camera registration (`IAmCamera`) consists of:
* 1 byte: message type (0x80)
* 2 bytes: road number (u16, big-endian)
* 2 bytes: mile marker (u16, big-endian)
* 2 bytes: speed limit (u16, big-endian)

Strings are length-prefixed with a single bytes, limiting them to 255 characters.

Certain message types can only be sent from the client to the server and vice versa.
For example, the client can never ask the server for a heartbeat and
the server can never tell the client it's a dispatcher or camera.

#### Core Requirements

The server must handle three key components:

1. **Cameras**: Each camera sits at a fixed point on a road and reports:
  * Its location (road number and mile marker)
  * The road's speed limit
  * License plate observations with timestamps

2. **Dispatchers**: Each dispatcher:
  * Registers to handle tickets for specific roads
  * Receives tickets for speed violations

3. **Speed Violation Detection**: The server must:
  * Calculate average speeds between any two observations on the same road
  * Generate tickets when speed exceeds the limit by >= 0.5 mph
  * Enforce the "one ticket per car per day" rule
  * Queue tickets when no dispatcher is available for the road where the offense occurred

#### The Concurrency Challenge

What makes this problem interesting is its concurrent nature,
with scale requirements and complex state management.

--

## Architecture

The server's architecture centers around a few key design choices that leverages
Haskell's strengths in managing concurrent state and handling errors:

### Core State Management

The server's state is managed through a collection of
`TVar`s containing thread-safe maps:

```haskell
data Server = Server
  { cameras      :: TVar (Map Road [CameraClient])     -- Active cameras by road
  , dispatchers  :: TVar (Map Road [DispatcherClient]) -- Active dispatchers 
  , observations :: TVar (Map Plate [Observation])     -- All observations by plate
  , tickets      :: TVar (Map Road [Ticket])           -- Pending tickets by road
  , ticketDays   :: TVar (Set TicketDay)               -- Track daily violations
  }
```

This design allows quick lookups for road and plate queries.
It also creates granularity between the different maps, minimizing
contention between different operations.

### Client Management

Each client connection runs in its own thread using `bracket` for resource cleanup:

```haskell
handleClient :: Server -> Socket -> IO ()
handleClient server sock = bracket
  (mkClientState sock)        -- 1. Initialize client state
  (handleClientMessages)      -- 2. Process messages
  (cleanupHeartbeat)          -- 3. Guaranteed cleanup
```

The client state tracks:
* Socket connection
* Heartbeat configuration
* Message parsing buffer
* Client type (camera / dispatcher)
