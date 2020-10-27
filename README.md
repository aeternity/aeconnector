# aeconnector 🔌
###### Overview
Parent chains interface provider (Hyperchains). The implementation of Hyperchains connector behaviour. 
The current supply interacts with the next parent chains:

- [x] bitcoin
- [ ] ethereum
- [ ] aeternity

<p align="center">
  <img src="/HyperchainsConnectorsComponent.png">
</p>

###### Developers guide
Each intersted developer can implement their own parent chain connector. Hyperchains operates via the next listed interface:


```
%% Execute commitment call:
-callback send_tx(Delegate::binary(), Commitment::binary(), PoGF::binary()) -> ok.
```

```
%% Request of the current top block:
-callback get_top_block() -> aehc_parent_block:parent_block().
```

```
%% Request block by hash:
-callback get_block_by_hash(binary()) -> aehc_parent_block:parent_block().
```

```
%% Perform delegate readiness check:
-callback dry_send_tx(binary(), binary(), binary()) -> ok.
```
