{-# LANGUAGE LambdaCase #-}

module ThinkMorse.LED where

import Control.Concurrent (threadDelay)
import Data.Text qualified as T

data Instruction = On Int | Off Int

morseToInstructions :: Int -> T.Text -> [Instruction]
morseToInstructions speed = map (g . f) . T.unpack
 where
  f :: Char -> Instruction
  f = \case
    '.' -> On 100
    '-' -> On 300
    ' ' -> Off 500
    '/' -> Off 900
    _ -> undefined

  g :: Instruction -> Instruction
  g (On n) = On $ speed * n
  g (Off n) = Off $ speed * n

runInstruction :: String -> Instruction -> IO ()
runInstruction led =
  \case
    (On n) -> do
      writeFile path "1"
      threadDelay n
      writeFile path "0"
      threadDelay 100000
    (Off n) -> threadDelay n
 where
  path = "/sys/class/leds/" ++ led ++ "/brightness"
