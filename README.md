# aeconnector 🔌
###### Overview
Parent chains interface provider (Hyperchains). The implementation of Hyperchains connector behaviour. 
The current supply interacts with the next parent chains:

- [x] bitcoin
- [ ] ethereum
- [ ] aeternity

###### Developers guide
Each intersted developer can implement their own parent chain connector. Hyperchains operates via the next listed interface:

```
%% Execute commitment call:
-callback send_tx(Delegate::binary(), Commitment::binary(), PoGF::binary()) -> ok.
```
> NOTE: Delegate is an account address on the particular parent chain.
> Commitment, PoGF are decoded Hyperchains log entries

```
%% Request of the current top block:
-callback get_top_block() -> TopBlock:aehc_parent_block:parent_block().
```
> NOTE: TopBlock is created via parent_block/4 constructor

```
%% Request block by hash:
-callback get_block_by_hash(Hash:binary()) -> Block:aehc_parent_block:parent_block().
```
> NOTE: Block is created via parent_block/4 constructor  

```
%% Perform delegate readiness check:
-callback dry_send_tx(Delegate::binary(), Commitment::binary(), PoGF::binary()) -> ok.
```
> NOTE: Delegate, Commitment, PoGF are arguments from send_tx/3.
> The callback has designed to perform preliminary check before system is ready (account balance, contract existence, etc.).
> The particular implementation depends on connector's developer;

```
-spec commitment(Delegate::binary(), KeyblockHash::binary()) ->
    commitment().
```
> NOTE: Delegate is an account address on the particular parent chain (public key).
> KeyblockHash is the hash value commited block

```
-spec aehc_connector:parent_block(Height::non_neg_integer(), Hash::binary(), PrevHash::binary(), Commitments::[commitment()]) ->
    parent_block().
```
> NOTE: Height, Hash, PrevHash are block attributes from the particular parent chain.
> Commitments are validated log entries from the returned block. Each commitment is made via commitment/2.
> Connector's developer is responsible to provide authorization of the commitments list

<p align="center">
  <img src="/HyperchainsConnectorsComponent.png">
</p>
