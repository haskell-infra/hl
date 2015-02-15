{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Model for wiki.

module HL.Model.Wiki where

import HL.Controller


import Control.Exception.Lifted (catch)
import Control.Spoon
import Data.Conduit
import Data.Maybe
import Data.Monoid
import Data.Text (unpack)
import Network.HTTP.Conduit
import Prelude hiding (readFile)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Readers.MediaWiki
import Text.XML
import Text.XML.Cursor

-- | Get the MediaWiki markup of a wiki page and then convert it to
-- HTML.
getWikiPage :: Text -> IO (Either Text (Text,Pandoc))
getWikiPage article =
  do request <- parseUrl ("http://wiki.haskell.org/api.php?action=query&\
                          \prop=revisions&rvprop=content&format=xml&titles=" <>
                          unpack article)
     withManager
       (\manager ->
          do response <- http request manager
             doc <- catch (fmap Just (responseBody response $$+- sinkDoc def))
                          (\(_::UnresolvedEntityException) -> return Nothing)
             case doc >>= parse of
               Nothing -> return (Left "Unable to parse XML from wiki.haskell.org.")
               Just (title,pan) ->
                 return
                   (fromMaybe (Left ("Unable to parse XML from wiki.haskell.org! \
                                     \And the parser gave us an impure exception! \
                                     \Can you believe it?"))
                              (showSpoon (Right (title,pan)))))
  where
    parse doc =
      do let cursor = fromDocument doc
         title <- listToMaybe (getTitle cursor)
         text <- listToMaybe (getText cursor)
         return (title,readMediaWiki def (unpack text))
    name n =
      Name {nameLocalName = n
           ,nameNamespace = Nothing
           ,namePrefix = Nothing}
    getText cursor =
      element (name "api") cursor >>=
      descendant >>=
      element (name "query") >>=
      descendant >>=
      element (name "pages") >>=
      descendant >>=
      element (name "page") >>=
      descendant >>=
      element (name "revisions") >>=
      descendant >>=
      element (name "rev") >>=
      descendant >>=
      content
    getTitle cursor =
      element (name "api") cursor >>=
      descendant >>=
      element (name "query") >>=
      descendant >>=
      element (name "pages") >>=
      descendant >>=
      element (name "page") >>=
      attribute (name "title")

-- | Make a spoon using the Show instance.
showSpoon :: Show a => a -> Maybe a
showSpoon a =
  (fmap (const a)
        (spoon (length (show a))))
