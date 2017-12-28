{-# LANGUAGE OverloadedStrings #-}

-- | Packages page view.

module HL.View.Packages where

import HL.Types
import HL.View
import HL.View.Template

packagesFromMarkdown :: Html () -> FromLucid App
packagesFromMarkdown md = template
  []
  "Packages"
  ( \_ -> container_
    ( row_
      ( span12_
        [class_ "col-sm-12"]
        ( do
          h1_ (toHtml ("Packages" :: String))
          md
        )
      )
    )
  )

