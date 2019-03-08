{-# OPTIONS -fno-warn-orphans -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Make the dispatcher.

module HL.Dispatch () where

import HL.Controller.Community
import HL.Controller.Documentation
import HL.Controller.Donate
import HL.Controller.Downloads
import HL.Controller.Packages
import HL.Controller.Home
import HL.Controller.Irc
import HL.Controller.MailingLists
import HL.Controller.News
import HL.Foundation
import Yesod.Core.Dispatch

mkYesodDispatch "App" resourcesApp

