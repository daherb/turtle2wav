module Shapes(cube) where

import Data.WAVE
import Data.Int

defaultHeader :: WAVEHeader
defaultHeader = WAVEHeader {
  waveNumChannels = 2 ,
  waveFrameRate = 44100,
  waveBitsPerSample = 32,
  waveFrames = Nothing
  }

cube :: WAVE
cube = WAVE {
  waveHeader = defaultHeader,
  waveSamples = []
  }

packStereoChannels :: [WAVESample] -> [WAVESample] -> [[WAVESample]]
packStereoChannels [] _ = []
packStereoChannels _ [] = []
packStereoChannels (l:ls) (r:rs) = [l,r]:packStereoChannels ls rs

wave :: WAVE
wave =
  let
    left = [doubleToSample (sin (fromIntegral x)) | x <- [1 .. waveFrameRate defaultHeader * 5 ]] :: [Int32]
    right = left
    samples = packStereoChannels left right
    header = defaultHeader { waveFrames = Just $ length samples }
  in
    WAVE header samples
