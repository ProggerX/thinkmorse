module ThinkMorse where

import Data.Text qualified as T
import Options.Applicative
import ThinkMorse.Encoder
import ThinkMorse.LED

data App = App
  { text :: T.Text
  , led :: String
  , speed :: Int
  }

app :: Parser App
app =
  App
    <$> strOption
      ( long "text"
          <> short 't'
          <> help "text to encode."
          <> metavar "MESSAGE"
      )
    <*> strOption
      ( long "led"
          <> short 'l'
          <> help "Led to transmit"
          <> showDefault
          <> value "tpacpi::lid_logo_dot"
          <> metavar "LED"
      )
    <*> option
      auto
      ( long "speed"
          <> short 's'
          <> showDefault
          <> value 1500
          <> metavar "INT"
      )

main :: IO ()
main = run =<< execParser opts
 where
  opts =
    info
      (app <**> helper)
      ( fullDesc
          <> header "thinkmorse -- a program to transmit morse messages using laptop leds"
          <> progDesc "Transmit a message"
      )

run :: App -> IO ()
run (App text led speed) = mapM_ (runInstruction led) $ morseToInstructions speed $ encodeMorse text
