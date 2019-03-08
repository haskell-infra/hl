{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Yesod foundation.

module HL.Foundation
  (module HL.Static
  ,App(..)
  ,Route(..)
  ,Handler
  ,Widget
  ,resourcesApp
  ,Slug(..)
  ,Human(..)
  ,Mode(..))
  where

import Control.Concurrent.MVar.Lifted
import HL.Static
import HL.Types

import Data.Monoid
import Network.Wai.Logger
import System.Log.FastLogger
import Yesod
import Yesod.Caching
import Yesod.Core.Types
import Yesod.Slug
import Yesod.Static

-- | Generate boilerplate.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | Don't log anything to stdout.
instance Yesod App where
  makeLogger _ =
    do set <- newFileLoggerSet 1000 "/dev/null"
       (date,_) <- clockDateCacher
       return (Logger {loggerSet = set
                      ,loggerDate = date})

instance MonadCaching (HandlerT App IO) where
  withCacheDir cont =
    do dirVar <- fmap appCacheDir getYesod
       withMVar dirVar cont

instance Human (Route App) where
  toHuman r =
    case r of
      CommunityR           -> "Community"
      IrcR                 -> "IRC"
      DocumentationR       -> "Documentation"
      HomeR                -> "Home"
      DonateR              -> "Donate"
      MailingListsR        -> "Mailing Lists"
      NewsR                -> "News"
      StaticR{}            -> "Static"
      DownloadsR           -> "Downloads"
      DownloadsForR os     -> "Downloads for " <> toHuman os
      PackagesR           -> "Packages"

instance Slug (Route App) where
  toSlug r =
    case r of
      CommunityR        -> "community"
      IrcR              -> "irc"
      DocumentationR    -> "documentation"
      HomeR             -> "home"
      DonateR           -> "donate"
      MailingListsR     -> "mailing-lists"
      NewsR             -> "news"
      StaticR{}         -> "static"
      DownloadsR        -> "downloads"
      DownloadsForR{}   -> "downloads"
      PackagesR        -> "packages"



