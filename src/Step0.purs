module Step0 where

import Prelude
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Effect (Effect)
import Effect.Class.Console (log)

type MyShow
  = { showString :: String -> String
    , showInt :: Int -> String
    , showBoolean :: Boolean -> String
    }

prog :: ReaderT MyShow Effect Unit
prog = do
  { showString, showInt, showBoolean } <- ask
  lift $ log $ showString "a"
  lift $ log $ showInt 1
  lift $ log $ showBoolean true
  pure unit

step0 :: Effect Unit
step0 =
  runReaderT prog
    { showString: show
    , showInt: show
    , showBoolean: const "Fooled you!"
    }
