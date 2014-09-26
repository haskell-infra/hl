{-# LANGUAGE OverloadedStrings #-}
-- | View library.

module HL.V
  (module HL.V
  ,module V)
  where

import HL.Foundation as V (Route(..),App,Human(..),Slug(..))
import HL.Static as V
import HL.Types as C

import Control.Monad as V
import Data.Text as V (Text)
import Prelude as V hiding (span,head,min,max,id,div)
import Text.Blaze.Bootstrap as V
import Text.Blaze.Extra as V
import Text.Blaze.Html5 as V hiding (map,title,style,header)
import Text.Blaze.Html5.Attributes as V hiding (span,summary,style,form,cite,label)
import Yesod.Blaze as V

-- Latest Bootstrap requires extra work to get the old grid layout
-- behaviour of spans.

todo :: Html -> Html
todo = span ! class_ "muted"
