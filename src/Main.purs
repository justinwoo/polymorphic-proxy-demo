module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Identity (Identity(..))
import Type.Proxy (Proxy(..))

two :: Int
two = 2

-- class GetName a where
--   getName :: Proxy a -> String

-- instance getNameInt :: GetName Int where
--   getName _ = "Int"

-- mkProxy :: forall a. a -> Proxy a
-- mkProxy _ = Proxy

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log $ getName (Proxy :: Proxy Int)
--   log $ getName (mkProxy two)

class GetName a where
  getName :: forall f. f a -> String

instance getNameInt :: GetName Int where
  getName _ = "Int"

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ getName (Proxy :: Proxy Int)
  log $ getName (Identity two)
