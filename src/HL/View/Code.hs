{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Code highlighting.

module HL.View.Code where

import HL.View

import Data.Text (unpack)
import Language.Haskell.HsColour.CSS (hscolour)

-- | Some syntax-highlighted code.
haskellPre :: Text -> Html ()
haskellPre = toHtmlRaw . hscolour False 0 . unpack

-- | Some syntax-highlighted code.
rejectedHaskellPre :: Text -> Text -> Html ()
rejectedHaskellPre msg =
  wrap .
  toHtmlRaw .
  hscolour False 0 .
  unpack
  where wrap :: Html () -> Html ()
        wrap inner =
          div_ [class_ "rejected-code"]
               (do span_ [class_ "rejected-note"]
                         (toHtml msg)
                   inner)

-- | Some syntax-highlighted code.
haskellCode :: Text -> Html ()
haskellCode = toHtmlRaw . preToCode . hscolour False 0 . unpack

-- | Convert a <pre> tag code sample to <code>.
preToCode :: [Char] -> [Char]
preToCode = codeEl . stripCPre . stripPre
  where stripPre ('<':'p':'r':'e':'>':xs) = xs
        stripPre xs = xs
        stripCPre (reverse -> ('<':'/':'p':'r':'e':'>':xs)) = reverse xs
        stripCPre xs = xs
        codeEl xs = "<code>" ++ xs ++ "</code>"
