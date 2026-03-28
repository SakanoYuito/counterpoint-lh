{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--reflection" @-}
{-@ LIQUID "--ple" @-}

module Counterpoint where

import Prelude hiding (head)

{-@ type OctaveRange = {n:Int | (-1) <= n && n <= 9} @-}

-- Pitch
data Pitch
  = C
  | Cis
  | D
  | Dis
  | E
  | F
  | Fis
  | G
  | Gis
  | A
  | Ais
  | H
  deriving (Eq, Ord, Show, Enum, Bounded)

{-@ reflect pitchToInt @-}
pitchToInt :: Pitch -> Int
pitchToInt p = case p of
  C -> 0
  Cis -> 1
  D -> 2
  Dis -> 3
  E -> 4
  F -> 5
  Fis -> 6
  G -> 7
  Gis -> 8
  A -> 9
  Ais -> 10
  H -> 11

-- A Note is a pitch class with an octave number,
-- which must be representable as a MIDI note number in the range [0..120].
{-@ data Note = Note { pitch :: Pitch, octave :: OctaveRange } @-}
data Note = Note
  { pitch :: Pitch,
    octave :: Int
  }
  deriving (Eq, Show)

-- midiOf (note) returns the MIDI number of the note, where C4 is 60, C5 is 72, etc.
{-@ reflect midiOf @-}
midiOf :: Note -> Int
midiOf (Note pc oct) = pitchToInt pc + 12 * (oct + 1)

-- interval (n1 n2) returns the absolute interval in semitones between two notes
{-@ reflect interval @-}
interval :: Note -> Note -> Int
interval n1 n2 = abs (midiOf n1 - midiOf n2)

-- signedInterval (n1 n2) returns the signed interval in semitones from n1 to n2
{-@ reflect signedInterval @-}
signedInterval :: Note -> Note -> Int
signedInterval n1 n2 = midiOf n2 - midiOf n1

-- simpleInterval (n1 n2) returns the interval in semitones between two notes,
-- reduced to the range [0..11]
{-@ reflect simpleInterval @-}
simpleInterval :: Note -> Note -> Int
simpleInterval n1 n2 = interval n1 n2 `mod` 12

-- isConsonant (n1 n2) returns True if the interval between n1 and n2 is a consonance
{-@ reflect isConsonant @-}
isConsonant :: Note -> Note -> Bool
isConsonant n1 n2 = case simpleInterval n1 n2 of
  0 -> True -- Unison (octave)
  3 -> True -- Minor third
  4 -> True -- Major third
  7 -> True -- Perfect fifth
  8 -> True -- Minor sixth
  9 -> True -- Major sixth
  _ -> False

-- isPerfectConsonance (n1 n2) returns True if the interval between n1 and n2 is
-- a perfect consonance (unison/octave or perfect fifth)
{-@ reflect isPerfectConsonance @-}
isPerfectConsonance :: Note -> Note -> Bool
isPerfectConsonance n1 n2 = case simpleInterval n1 n2 of
  0 -> True -- Unison (Octave)
  7 -> True -- Perfect fifth
  _ -> False

-- isValidBar (bar) returns True if the bar is valid,
-- i.e., if the two notes are consonant
{-@ reflect isValidBar @-}
isValidBar :: Bar -> Bool
isValidBar (Bar cf cp) = isConsonant cf cp

-- A Bar consists of a cantus firmus (cf) and a counterpoint (cp),
-- where the two notes must be consonant
-- ↓ The refinement type ensures that any Bar is valid by construction
{-@ data Bar = Bar { cf :: Note, cp :: {v:Note | isConsonant cf v} } @-}
data Bar = Bar
  { cf :: Note,
    cp :: Note
  }
  deriving (Eq, Show)

-- For example, the following bar is invalid and should be rejected by the type system:
-- invalidBar = Bar (Note C 4) (Note D 4)

-- isParallel (b1 b2) returns True if the motion from b1 to b2 is parallel
{-@ reflect isParallel @-}
isParallel :: Bar -> Bar -> Bool
isParallel (Bar cf1 cp1) (Bar cf2 cp2) =
  signedInterval cf1 cf2 * signedInterval cp1 cp2 > 0

-- isValidMotion (b1 b2) returns True if the motion from b1 to b2 is valid,
-- i.e., if it does not involve parallel perfect consonances
{-@ reflect isValidMotion @-}
isValidMotion :: Bar -> Bar -> Bool
isValidMotion n1@(Bar cf1 cp1) n2@(Bar cf2 cp2) =
  not (isPerfectConsonance cf2 cp2)
    || not (isParallel n1 n2)

-- A Counterpoint is a sequence of Bars, where each Bar must be valid and
-- the motion between consecutive Bars must be valid as well.
{-@ data Counterpoint = Fine | (:|) { bar :: Bar, rest :: {v:Counterpoint | (not (isEmpty v)) ==> isValidMotion bar (head v)}} @-}
data Counterpoint
  = Fine
  | (:|) {bar :: Bar, rest :: Counterpoint}
  deriving (Eq, Show)

infixr 5 :|

-- head (v) returns the first Bar of the counterpoint
-- The refinement type ensures that head is only called on non-empty counterpoints
{-@ reflect head @-}
{-@ head :: {v:Counterpoint | not (isEmpty v)} -> Bar @-}
head :: Counterpoint -> Bar
head (b :| _) = b
head Fine = error "Empty counterpoint has no head" -- unreachable

-- isEmpty (v) returns True if the counterpoint is empty
{-@ reflect isEmpty @-}
isEmpty :: Counterpoint -> Bool
isEmpty Fine = True
isEmpty _ = False

{-@ reflect makeBar @-}
{-@ makeBar :: p1:Pitch -> o1:OctaveRange -> p2:Pitch -> o2:{n:OctaveRange | isConsonant (Note p1 o1) (Note p2 n)} -> Bar @-}
makeBar :: Pitch -> Int -> Pitch -> Int -> Bar
makeBar p1 o1 p2 o2 = Bar (Note p1 o1) (Note p2 o2)