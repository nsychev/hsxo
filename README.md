# hsxo

Multithreaded Tic-Tac-Toe in Haskell.

## How to run?

You have to install [Stack](https://docs.haskellstack.org/en/stable/README/) beforehand.

1. Build:

   ```bash
   stack build
   ```

   (you can pass `install` here and get rid of `stack exec` below)

2. Run server:

    ```bash
    stack exec hsxo-server port 
    ```

    The default value for port is 4242.

3. Run client:

    ```bash
    stack exec hsxo-client host [port]
    ```

4. Run tests:

    ```bash
    stack test
    ```

## Protocol outline

Server and client communicate using length-prefixed protobuf over TCP.

There are several types of messages defined in [Hsxo.Message](hsxo/src/Hsxo/Message.hs):

* When connected, server sends to client `HelloServer` message, containing server software version.
* If client refuses the version, one should gracefully shuts down the connection.
* Otherwise, client sends `HelloClient` message.
* Then on each move, server sends `GameState` message, and client should reply with `GameMove` message.
  * The first `GameState` contains either all empty cells or exactly one cell occupied by server depending on who moves first (selected by server randomly). 
* When game ended — irrespective of whether it was a win, a draw or an invalid move, server provides `GameResult` field in `GameState` and then immediately closes the connection.
