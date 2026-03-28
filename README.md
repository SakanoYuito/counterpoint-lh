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

## Play

midi で出せる

```bash
# abcmidi が必要
brew install abcmidi

# MIDI ファイルを生成
cabal run play 2>/dev/null > /tmp/counterpoint.abc
abc2midi /tmp/counterpoint.abc -o /tmp/counterpoint.mid
open /tmp/counterpoint.mid
```

または出力テキストを [https://www.ne.jp/asahi/music/marinkyo/ml/abcjs-redaktilo.html.ja](https://www.ne.jp/asahi/music/marinkyo/ml/abcjs-redaktilo.html.ja) に貼り付けるとブラウザ上で楽譜表示＋再生できる
