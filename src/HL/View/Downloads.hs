{-# LANGUAGE OverloadedStrings #-}

-- | Downloads page view.

module HL.View.Downloads where

import Data.Monoid
import HL.Types
import HL.View
import HL.View.Template

stackSection :: Html ()
stackSection = do
  h2_ "The stack build tool"
  p_ $ do
    a_ [href_ "https://github.com/commercialhaskell/stack#readme"] "stack"
    " is a modern build tool for Haskell code. It handles the management of your toolchain (including the GHC compiler and MSYS2 on Windows), building and registering libraries, and much more."
  p_ (a_ [href_ "https://github.com/commercialhaskell/stack/blob/master/GUIDE.md"] "Read the stack guide →")
  p_ "Get stack for:"
  ul_ $ do
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#windows"] "Windows"
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#os-x"] "OS X"
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#ubuntu"] "Ubuntu"
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#debian"] "Debian"
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#centos--red-hat--amazon-linux)"] "CentOS / Red Hat / Amazon Linux"
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#fedora"] "Fedora"
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#nixos"] "NixOS"
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#arch-linux"] "Arch Linux"
    li_ $ a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#linux"] "Linux (general)"
  p_ (a_ [href_ "https://github.com/commercialhaskell/stack/wiki/Downloads#upgrade"] "Upgrade instructions")

  h3_ "Installers including GHC"
  -- Note: once Haskell Platform begins shipping with stack and without global
  -- packages, it should take over this section.
  p_ "If you're going to be installing Haskell on multiple systems, an installer that includes GHC may be beneficial to decrease bandwidth. The following options are available:"
  ul_ $ do
    li_ $ do
      "Windows: "
      a_ [href_ "https://github.com/fpco/minghc/releases"] "MinGHC"
    li_ $ do
      "OS X (10.9 or later): "
      a_ [href_ "http://ghcformacosx.github.io/"] "GHC for Mac OS X"

hpSection :: Html ()
hpSection = do
  let hpRoot = "http://www.haskell.org/platform/"
  h3_ "Haskell Platform"
  p_ $ "The Haskell Platform is a convenient way to install the Haskell development tools and"
       <> " a collection of commonly used Haskell packages from Hackage."
  p_ $ "Get the Haskell Platform for:"
  ul_ $ do li_ $ a_ [href_ $ hpRoot <> "windows.html"] "Windows"
           li_ $ a_ [href_ $ hpRoot <> "mac.html"] "OS X"
           li_ $ a_ [href_ $ hpRoot <> "linux.html"] "Linux"

-- | Downloads view.
downloadsV :: FromLucid App
downloadsV =
  template [] "Downloads"
    (\url ->
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (do h1_ "Downloads"
                   stackSection
                   libraries
                   hr_ [style_ "height: 1px; background-color: black;"]
                   h2_ "Alternative Toolchain Downloads"
                   hpSection
                   h3_ "Compiler and base libraries"
                   p_ "Many now recommend just using the compiler and base libraries combined with package sandboxing, especially for new users interested in using frameworks with complex dependency structures."
                   p_ "Downloads are available on a per operating system basis:"
                   ul_ (forM_ [minBound .. maxBound]
                              (\os ->
                                 li_ (a_ [href_ (url (DownloadsForR os))]
                                         (toHtml (toHuman os)))))
                   ))))

-- | OS-specific downloads view.
downloadsForV :: OS -> Html () -> Html () -> FromLucid App
downloadsForV os autoInstall manualInstall =
  template
    [DownloadsR
    ,DownloadsForR os]
    ("Downloads for " <> toHuman os)
    (\_ ->
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (do h1_ (toHtml ("Downloads for " <> toHuman os))
                   autoInstall
                   when (os == Linux)
                        (do h2_ "Manual install"
                            p_ "To install GHC and Cabal manually, follow these steps."
                            manualInstall)))))

libraries :: Html ()
libraries =
  do h2_ "Libraries"
     p_ $ do
       "There are thousands of high quality libraries available for Haskell. You can browse them online on Hackage, Haskell's central open source package repository"
     p_ (a_ [href_ "https://hackage.haskell.org/packages/"] $ "Go to Hackage →")
