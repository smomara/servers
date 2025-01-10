# Protohackers Servers

Solutions for the [Protohackers](https://protohackers.com) network programming
challenges, implemented in Haskell.

## Challenges

- [0: Smoke Test](./src/SmokeTest.hs) - Simple TCP echo server
- [1: Prime Time](./src/PrimeTime.hs) - JSON service for checking prime numbers
- [2: Means to an End](./src/MeansToAnEnd.hs) - TCP server for price data analysis
- [3: Budget Chat](./src/BudgetChat.hs) - Simple TCP Chat Server
- [5: Mob In The Middle](./src/MobInTheMiddle.hs) - Nefarious Proxy for Budget Chat

## Running

```bash
cabal run server-selector -- <challenge> <port>
```
