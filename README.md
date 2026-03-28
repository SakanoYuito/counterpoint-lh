# counterpoint-lh

Compile-time verification of first-species counterpoint rules using LiquidHaskell

## 実装したもの（第一類）

- 声部間の音程は協和音程になる
- 並達5度・8度の禁止


## Dependencies

| ツール | バージョン |
|---|---|
| GHC | 9.12.2 |
| LiquidHaskell | 0.9.12.2.1 |
| Z3 | 4.x |
| cabal-install | 3.x |

## Build

```bash
cabal v2-build
```