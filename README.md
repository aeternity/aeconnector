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

###### Connector's setup 

Hyperchains can manage connector's deployment strategies and supports 3 integration scenarios (Bitcoin, Ethereum, Aeternity blockchains are listed for example):

### Setup #1

The monolith setup schema which is used for the MVP of Hyperchains (the main connector which acts as election engine and keeps synched view):   

<p align="center">
  <img src="/doc/src/HyperchainsConnectorsMain.png">
  <br>
    <em>Monolith connector setup #1 </em>
</p>

### Setup #2
 
Replica supported setup. Was designed to switch attached blockchain in the case of "service denied" (decision can be made automatically): 

<p align="center">
  <img src="/doc/src/HyperchainsConnectorsMainReplica.png">
  <br>
    <em>Replica connector setup #2 </em>
</p>

### Setup #3

History keeper setup (please note that currently in development process). This setup allows to decouple *election engine* and *security provider* roles and assumes the usage of the most scalable and faster blockchain as election engine and the older one as a security provider:

<p align="center">
  <img src="/doc/src/HyperchainsConnectorsElectionHistory.png">
  <br>
    <em>History keeper (election/history) connector setup #3 </em>
</p>






