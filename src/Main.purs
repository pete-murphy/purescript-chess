module Main where

import Prelude

import App (mkApp)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception as Exception
import React.Basic.DOM as DOM
import Web.DOM.NonElementParentNode as NonElementParentNode
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  document <- HTML.window >>= Window.document
  container <- NonElementParentNode.getElementById "root" (HTMLDocument.toNonElementParentNode document)
  case container of
    Nothing -> Exception.throw "Root element not found." 
    Just c -> do
      app <- mkApp
      DOM.render (app unit) c
    
  