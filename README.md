# DEX Core Contract

The core execution layer for a decentralized exchange, managing swaps,
liquidity pools, and pricing logic in a trustless on-chain environment.

## Key Functions
- `add-liquidity` — Deposit token pairs into a liquidity pool
- `remove-liquidity` — Withdraw pooled assets proportionally
- `swap` — Exchange one token for another using pool pricing
- `get-price` — Calculate current swap price based on reserves
- `get-pool` — Retrieve liquidity pool state and balances

Designed to serve as the foundation for AMMs, trading frontends, and
liquidity incentive protocols.
