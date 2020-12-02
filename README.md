# aeconnector 🔌
###### Overview

Aeconnetor is an extension set which connects [hyperchains](https://github.com/aeternity/hyperchains-whitepaper) to the real world blockchain implementations. 
Designed with idea to be friendly as possible for a new developers and to allow them provide their own implementations for existed blockchains. 

An each connector represents a *gateway* into the external blockchain (so called parent chain) which allows to fetch the top, particular block data and to place so called *commitments* on a chain.

In fact commitments are payloaded transactions where each transaction procudes a historical entry which lands on blockchain and allows to Hyperchains to track it's state by secure way and decentrilized way. 

In comparision to the common miners which have to spend a large amount of electricity you have to spend a small amount of transaction fee by sending transactions for yourself or to make regular payments as *payment gateway* (see detailed [payment gateway guide](https://github.com/aeternity/aeconnector/wiki/Payment-gateway)).

In this approach your available balance acts as a main *fuel* ⛽️ , so be sure that you have it enough on the parent chain before you are going to earn rewards for delegate's activity. 

The current supply contains the next implementations 🔌 (go through the links to see the detailed configuration guide):

- [x] [bitcoin (RPC)](https://github.com/aeternity/aeconnector/wiki/Bitcoin-connector) (full node)
- [ ] [ethereum (ws)](https://github.com/aeternity/aeconnector/wiki/Ethereum-connector) (Geth)
- [ ] [aeternity (Erlang RPC)](https://github.com/aeternity/aeconnector/wiki/Aeternity-connector) (full node)

In ability to be accepted as "connector" supplied implementation has to satisfy *aeconnector* beahaviour and to pass acceptance control (see [developers guide](https://github.com/aeternity/aeconnector/wiki/Developers-guide))

###### Connectors setup 

<p align="center">
  <img src="/doc/src/HyperchainsConnectorsMain.png">
  <br>
    <em>Main connector's setup #1 </em>
</p>


<p align="center">
  <img src="/doc/src/HyperchainsConnectorsMainReplica.png">
  <br>
    <em>Main + replica connector's setup #2 </em>
</p>

<p align="center">
  <img src="/doc/src/HyperchainsConnectorsElectionHistory.png">
  <br>
    <em>Main (election) + history connector's setup #3 </em>
</p>






