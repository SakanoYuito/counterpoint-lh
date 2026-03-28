module Main where

import Data.Char (toUpper)
import Counterpoint
import Examples

-- ----------------------------------------------------------------
-- ABC 記譜法へのコンバータ
--   ABC は軽量なテキスト形式の楽譜。
--   出力を https://editor.drawthedots.com/ に貼り付けると再生できる。
--   または: brew install abcmidi && cabal run play | abc2midi - -o out.mid && open out.mid
-- ----------------------------------------------------------------

pitchAcc :: Pitch -> String
pitchAcc Cis = "^"; pitchAcc Dis = "^"; pitchAcc Fis = "^"
pitchAcc Gis = "^"; pitchAcc Ais = "^"; pitchAcc _   = ""

pitchBase :: Pitch -> Char
pitchBase C   = 'c'; pitchBase Cis = 'c'
pitchBase D   = 'd'; pitchBase Dis = 'd'
pitchBase E   = 'e'; pitchBase F   = 'f'; pitchBase Fis = 'f'
pitchBase G   = 'g'; pitchBase Gis = 'g'
pitchBase A   = 'a'; pitchBase Ais = 'a'
pitchBase H   = 'b'

-- Note を ABC 記法の文字列に変換
-- （中央C = C4 = 'c'、C3 = 'C'、C5 = "c'"）
noteToABC :: Note -> String
noteToABC (Note p oct) = acc ++ [letter] ++ octStr
  where
    acc    = pitchAcc p
    base   = pitchBase p
    letter = if oct >= 4 then base else toUpper base
    octStr
      | oct >= 5  = replicate (oct - 4) '\''
      | oct == 3  = ""
      | oct <  3  = replicate (3 - oct) ','
      | otherwise = ""

-- Counterpoint からカントゥスとコントラポイントの音列を分離
voices :: Counterpoint -> ([Note], [Note])
voices Fine         = ([], [])
voices (b :| rest)  = (cf b : v1, cp b : v2)
  where (v1, v2) = voices rest

-- Counterpoint を ABC 記譜法の文字列に変換
toABC :: String -> Counterpoint -> String
toABC title piece = unlines
  [ "X:1"
  , "T:" ++ title
  , "M:C"          -- common time (4/4)
  , "L:1/1"        -- 全音符単位
  , "Q:1/1=60"     -- テンポ BPM=60
  , "K:C"
  , "%%MIDI program 73"   -- flute (General MIDI #74)
  , "V:1 name=\"Flute\""
  , unwords (map noteToABC v1) ++ " |]"
  , "%%MIDI program 68"   -- oboe  (General MIDI #69)
  , "V:2 name=\"Oboe\""
  , unwords (map noteToABC v2) ++ " |]"
  ]
  where (v1, v2) = voices piece

-- 出力したい曲をここに並べる
pieces :: [(String, Counterpoint)]
pieces =
  [ ("validMixedMotion",      validMixedMotion)
  , ("validParallelThirds",   validParallelThirds)
  , ("validParallelSixths",   validParallelSixths)
  , ("validContraryToFifth",  validContraryToFifth)
  , ("validContraryToOctave", validContraryToOctave)
  , ("validObliqueCfPedal",   validObliqueCfPedal)
  ]

main :: IO ()
main = mapM_ printPiece pieces
  where
    printPiece (name, cp) = do
      putStrLn $ "=== " ++ name ++ " ==="
      putStr   $ toABC name cp
      putStrLn ""
