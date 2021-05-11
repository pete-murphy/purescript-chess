module App where

import Prelude
import Data.Foldable as Traversable
import React.Basic.DOM as R
import React.Basic.DOM.Events as DOM.Events
import React.Basic.Events as Events
import React.Basic.Hooks (Component, (/\))
import React.Basic.Hooks as Hooks

mkApp :: Component Unit
mkApp = do
  Hooks.component "App" \_ -> Hooks.do
    text /\ setText <- Hooks.useState' ""
    pure do
      R.div_
        [ R.input
            { onChange:
                Events.handler
                  DOM.Events.targetValue
                  (Traversable.traverse_ setText)
            }
        ]
