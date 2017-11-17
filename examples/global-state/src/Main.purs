module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, newRef, writeRef, Ref)
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Free (Free, liftF, foldFree)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

type State = { on :: Boolean }

data Query a = ToggleState a

data MyAlgebra a = Log String a | Write String a 
type MyMonad = Free MyAlgebra

output :: String -> MyMonad Unit
output msg = liftF (Log msg unit)

ui :: Ref String -> H.Component HH.HTML Query Unit Void MyMonad
ui r =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Toggle Button" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text (if state.on then "On" else "Off") ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void MyMonad
  eval (ToggleState next) = do
    H.modify (\state -> { on: not state.on })
    H.lift $ output "State was toggled"
    _ <- H.lift $ liftF (Write "New value" r)
    pure next

ui' :: forall eff. Ref String -> H.Component HH.HTML Query Unit Void (Aff (HA.HalogenEffects (console :: CONSOLE | eff)))
ui' r = H.hoist (foldFree evalMyAlgebra) (ui r)
  where
    evalMyAlgebra :: MyAlgebra ~> Aff (HA.HalogenEffects (console :: CONSOLE | eff))
    evalMyAlgebra (Log msg next) = do
      log msg
      pure next
    evalMyAlgebra (Write msg next) = do
      _ <- liftEff $ writeRef r msg
      pure next



main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  s <- liftEff $ newRef "Hello"
  runUI (ui' s) unit body
