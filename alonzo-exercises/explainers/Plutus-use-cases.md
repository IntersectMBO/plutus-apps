# Plutus use cases
Typical high level use cases for Plutus implementations include, but are not restricted to:

- **Oracles** – fully functional oracles that bring off-chain data on the chain to interact with and feed smart contracts, In addition, oracles create a centralized and trusted off-chain data feed for Plutus applications (for example, to interact with price feeds from various centralized exchanges).

- **DEX token swaps** – creating an implementation that allows users to swap between supported tokens on a decentralized exchange. Users can create liquidity pools (or add funds to existing ones) providing coins necessary for swapping. In return, they can earn small fees for all transactions that use their funds. Users can also contribute to liquidity pools for any supported token, and therefore gain commissions in the form of exchange fees for doing so. When liquidity is provided to a pool, the user receives a liquidity token representing that deposit. The contract should calculate fees (e.g., 0.3%) which are then dispersed to liquidity providers dependent on each provider's share of the liquidity pool.

- **Lending and borrowing** – creating a lending protocol that enables users to lend and borrow cryptocurrencies of their choice in a trustworthy way, while offering stable and variable interest rates. Users can participate as depositors or borrowers. To transact, lenders have to deposit their funds into liquidity pools, so that borrowers can borrow from such liquidity pools. Depositors receive interest-bearing tokens (known as aTokens) in return. Each pool is set aside as reserves to safeguard against volatility.
NFTS, minting, and buying and selling NFTs – building core functionality for minting, sending, and receiving NFTs into a wallet, with other open scenarios and extensions possible.

- **Decentralized finance (DeFi) tools** – creating multipurpose dashboards (either web-based or mobile), that integrate with smart contracts to bring value to native token traders. These products can have multiple functional dashboards to show balances of tokens and liquidity pools, and so on. They can also bundle together multiple functions such as swapping and providing liquidity into a single transaction, making DeFi adoption easier.

- **Crypto-backed stable coins** – creating a new stable coin implementation based on chain collateral using the Atala identity system on Cardano. This can include transfer restrictions, asset freezing, as so on.

## Plutus Partners Program
In preparation for Alonzo, we launched the Plutus Partner Program. This program summoned an all-star team of software companies to tackle smart contract programming challenges, including the above-listed use cases. The goal of the program is to enable smart contract implementations – constructed using the Plutus language – that serve as instructive examples for incoming external decentralized DApp developers. These can later serve as canonical examples for the wider Cardano community too. We will share details of these implementations soon.
