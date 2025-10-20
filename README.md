# stm-chat - concurrent chat server in Haskell

## Quick Start
1. Build: `stack build`
2. Run demo `stack run`

## Architecture
- Core server uses STM (`TVar`, `TChan`) for concurrency.
- (WIP) Client adapters: CLI / TCP / WebSocket.


## Experiments
- Compare STM vs MVar
- Shareded vs monolithic room table
- Backpressure policies
