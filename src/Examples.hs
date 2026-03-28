{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--reflection"     @-}
{-@ LIQUID "--ple"            @-}

-- ================================================================
-- 1声種対位法 テストケース集
--
-- * valid_* : コンパイルが通ること (SAFE) を確認
-- * bad_*   : コメントアウトを外すと UNSAFE になることを確認
-- ================================================================
module Examples where

import Counterpoint

-- ----------------------------------------------------------------
-- [OK] 並行進行（不完全協和音へ）
-- ----------------------------------------------------------------

-- 並行3度：長3度・短3度の連続はすべて合法
valid_parallel_thirds :: Counterpoint
valid_parallel_thirds =
       makeBar C 4 E 4   -- 長3度 (4半音)
    :| makeBar D 4 F 4   -- 短3度 (3半音)  同方向・不完全協和 -> OK
    :| makeBar E 4 G 4   -- 短3度 (3半音)
    :| makeBar F 4 A 4   -- 長3度 (4半音)
    :| makeBar G 4 H 4   -- 長3度 (4半音)
    :| Fine

-- 並行6度：同様に合法
valid_parallel_sixths :: Counterpoint
valid_parallel_sixths =
       makeBar C 4 A 4   -- 長6度 (9半音)
    :| makeBar D 4 H 4   -- 長6度 (9半音)  同方向・不完全協和 -> OK
    :| makeBar E 4 C 5   -- 短6度 (8半音)
    :| Fine

-- ----------------------------------------------------------------
-- [OK] 反行進行（完全協和音へ）
-- ----------------------------------------------------------------

-- 完全5度に反行で解決（最も推奨される進行）
valid_contrary_to_fifth :: Counterpoint
valid_contrary_to_fifth =
       makeBar D 4 A 4   -- 完全5度 (CF=D4, CP=A4)
    :| makeBar G 4 C 4   -- 完全5度 CF上行・CP下行 -> 反行 -> OK
    :| Fine

-- 完全8度に反行で解決
valid_contrary_to_octave :: Counterpoint
valid_contrary_to_octave =
       makeBar E 4 C 4   -- 長3度
    :| makeBar G 3 G 4   -- 完全8度 CF下行・CP上行 -> 反行 -> OK
    :| Fine

-- ----------------------------------------------------------------
-- [OK] 斜行進行（一方の声部が保留）
-- ----------------------------------------------------------------

-- CF を C4 に固定したままCP が動く（ペダルポイント風）
valid_oblique_cf_pedal :: Counterpoint
valid_oblique_cf_pedal =
       makeBar C 4 C 4   -- 同音 (0半音)
    :| makeBar C 4 E 4   -- 長3度  CF静止・CP上行 -> 積=0 -> 斜行 -> OK
    :| makeBar C 4 G 4   -- 完全5度 CF静止 -> 斜行 -> OK
    :| makeBar C 4 A 4   -- 長6度  CF静止 -> 斜行 -> OK
    :| makeBar C 4 C 4   -- 同音   CF静止 -> 斜行 -> OK
    :| Fine

-- ----------------------------------------------------------------
-- [OK] 混合進行（反行・並行・斜行の組み合わせ）
-- ----------------------------------------------------------------

valid_mixed_motion :: Counterpoint
valid_mixed_motion =
       makeBar C 5 C 4   -- 完全8度
    :| makeBar G 4 E 4   -- 短3度  反行
    :| makeBar A 4 F 4   -- 長3度  並行（不完全->OK）
    :| makeBar H 4 D 4   -- 長6度  反行
    :| makeBar C 5 C 4   -- 完全8度 反行
    :| Fine

-- ----------------------------------------------------------------
-- [NG] 並行完全5度（コメントアウトを外すと UNSAFE）
-- ----------------------------------------------------------------

-- G4-C4 -> A4-D4：どちらも完全5度、どちらも上行 -> 並行5度禁止
-- bad_parallel5ths :: Counterpoint
-- bad_parallel5ths =
--        makeBar G 4 C 4   -- 完全5度 (7半音)
--     :| makeBar A 4 D 4   -- 完全5度・並行進行 -> UNSAFE
--     :| Fine

-- 下行でも同様に禁止
-- bad_parallel5ths_down :: Counterpoint
-- bad_parallel5ths_down =
--        makeBar A 4 D 4   -- 完全5度
--     :| makeBar G 4 C 4   -- 完全5度・並行下行 -> UNSAFE
--     :| Fine

-- ----------------------------------------------------------------
-- [NG] 並行完全8度
-- ----------------------------------------------------------------

-- C5-C4 -> D5-D4：どちらも完全8度、どちらも上行 -> 並行8度禁止
-- bad_parallel8ths :: Counterpoint
-- bad_parallel8ths =
--        makeBar C 5 C 4   -- 完全8度 (12半音, mod12=0)
--     :| makeBar D 5 D 4   -- 完全8度・並行進行 -> UNSAFE
--     :| Fine

-- ----------------------------------------------------------------
-- [NG] 直行（隠れた）完全5度・8度
--   同方向進行で完全協和音に達するのも禁止
-- ----------------------------------------------------------------

-- C4-A3 (短3度) -> G4-C4 (完全5度)：両声部とも上行 -> 直行5度
-- bad_direct_fifth :: Counterpoint
-- bad_direct_fifth =
--        makeBar C 4 A 3   -- 短3度
--     :| makeBar G 4 C 4   -- 完全5度・同方向進行 -> UNSAFE
--     :| Fine

-- E4-G3 (長6度) -> C5-C4 (完全8度)：両声部とも上行 -> 直行8度
-- bad_direct_octave :: Counterpoint
-- bad_direct_octave =
--        makeBar E 4 G 3   -- 長6度 (9半音)
--     :| makeBar C 5 C 4   -- 完全8度・同方向進行 -> UNSAFE
--     :| Fine

-- ----------------------------------------------------------------
-- [NG] 縦の音程が不協和（makeBar の型制約で弾かれる）
--   Counterpoint の制約より手前、Bar の構築時点でエラーになる
-- ----------------------------------------------------------------

-- 完全4度 (5半音) は不協和扱い -> makeBar の型エラー
-- bad_P4 :: Counterpoint
-- bad_P4 =
--        makeBar G 4 C 4   -- 完全5度（合法）
--     :| makeBar C 5 G 4   -- 完全4度 (5半音) -> 型エラー
--     :| Fine

-- 長2度 (2半音) -> 型エラー
-- bad_M2 :: Counterpoint
-- bad_M2 =
--        makeBar C 5 C 4
--     :| makeBar D 5 C 4   -- 長2度 (14半音, mod12=2) -> 型エラー
--     :| Fine

-- 減5度=トリトーン (6半音) -> 型エラー
-- bad_tritone :: Counterpoint
-- bad_tritone =
--        makeBar C 5 C 4
--     :| makeBar Fis 4 C 4  -- トリトーン (6半音) -> 型エラー
--     :| Fine
