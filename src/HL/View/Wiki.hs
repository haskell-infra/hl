{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Wiki page view.

module HL.View.Wiki where

import           HL.View
import           HL.View.Code
import           HL.View.Template

import           Control.Monad.Identity
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.List (isPrefixOf)
import           Data.Monoid
import           Data.Text (unpack,pack)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder
import           Language.Haskell.HsColour.CSS (hscolour)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.HTML.TagStream.Text
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers.HTML

-- | Wiki view.
wikiV :: (Route App -> Text) -> Either Text (Text,Pandoc) -> FromLucid App
wikiV urlr result =
  template
    ([WikiHomeR] ++
     [WikiR n | Right (n,_) <- [result]])
    (case result of
       Left{} -> "Wiki error!"
       Right (t,_) -> t)
    (\_ ->
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (case result of
                  Left err ->
                    do h1_  "Wiki page retrieval problem!"
                       p_ (toHtml err)
                  Right (t,pan) ->
                    do h1_ (toHtml t)
                       toHtmlRaw (renderHtml (writeHtml writeOptions (cleanup urlr pan)))))))
  where cleanup url = highlightBlock . highlightInline . relativize url
        writeOptions = def { writerTableOfContents = True }

-- | Make all wiki links use the wiki route.
relativize :: (Route App -> Text) -> Pandoc -> Pandoc
relativize url = walk links
  where links asis@(Link is (ref,t))
          | isPrefixOf "http://" ref || isPrefixOf "https://" ref = asis
          | otherwise = Link is (unpack (url (WikiR (pack ref))),t)
        links x = x

-- | Highlight code blocks and inline code samples with a decent
-- Haskell syntax highlighter.
highlightBlock :: Pandoc -> Pandoc
highlightBlock = walk codes
  where codes (CodeBlock ("",["haskell"],[]) t) =
          RawBlock "html" (hscolour False 0 t)
        codes x = x

-- | Highlight code blocks and inline code samples with a decent
-- Haskell syntax highlighter.
highlightInline :: Pandoc -> Pandoc
highlightInline = walk codes
  where codes (Code ("",["haskell"],[]) txt) =
          RawInline "html" (preToCode (hscolour False 0 txt))
        codes (Code x txt) = Code x (unpack (decodeEntities (pack txt)))
        codes x = x

-- | Decode entities because for some reason MediaWiki syntax allows
-- entities and decodes them inside a <code></code> block.
decodeEntities :: Text -> Text
decodeEntities t =
  runIdentity (fmap (toStrict . toLazyText . mconcat)
                    (CL.sourceList [t]
                     $= tokenStream
                     $= CL.map (showToken (\x ->x))
                     $$ CL.consume))
