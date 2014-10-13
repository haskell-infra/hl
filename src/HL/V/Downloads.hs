{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Downloads page view.

module HL.V.Downloads where

import Data.Monoid
import HL.Types
import HL.V hiding (list)
import HL.V.Template

-- | Downloads view.
downloadsV :: FromBlaze App
downloadsV =
  template
    []
    "Downloads"
    (\url ->
       container
         (row
            (span12
               (do h1 "Downloads"
                   h2 "Compiler and base libraries"
                   p "Downloads are available on a per operating system basis:"
                   ul
                      (forM_ [minBound .. maxBound]
                             (\os -> li (a ! href (url (DownloadsForR os)) $
                                           (toHtml (toHuman os)))))
                   thirdParty))))

-- | OS-specific downloads view.
downloadsForV :: OS -> Html -> Html -> FromBlaze App
downloadsForV os autoInstall manualInstall =
  template
    [DownloadsR
    ,DownloadsForR os]
    ("Downloads for " <> toHuman os)
    (\_ ->
       container
         (row
            (span12

               (do h1 (toHtml ("Downloads for " <> toHuman os))
                   autoInstall
                   when (os == Linux)
                        (do h2 "Manual install"
                            p "To install GHC and Cabal manually, follow these steps."
                            manualInstall)))))

thirdParty =
  do h2 "Third party libraries"
     p (do "In Haskell, packages are managed with the Cabal package \
           \system built into GHC (and other compilers). "
           "For more specific details, see "
           (a ! href "https://www.haskell.org/cabal/users-guide/" $
              "The Cabal User Guide")
           ".")
     hackage
     stackage
     github

hackage =
  do h3 "Hackage"
     p (do "Hackage is a repository of packages to which anyone can freely \
           \upload at any time. The packages are available immediately and \
           \documentation will be generated and hosted there. It can be used by "
           code "cabal install"
           ".")
     p (do "In your "
           code ".cabal/config"
           " file, there will by default be a line like the following:")
     pre "remote-repo: hackage.haskell.org:http://hackage.haskell.org/packages/archive"
     p "You can install a package by merely running: "
     pre "$ cabal update \n\
         \$ cabal install the-package"
     p (a ! href "https://hackage.haskell.org/packages/" $ "Go to Hackage →")

stackage =
  do h3 "Stackage"
     p "Stackage is a stable repository of snapshots of package sets in \
       \which only packages which build and pass tests together are bundled \
       \together into a snapshot."
     p (do "To use, in your "
           code ".cabal/config"
           ".cabal/config file, you specify something like the following:")
     pre "remote-repo: stackage:http://www.stackage.org/stackage/<the-snapshot-hash>"
     p "(Note: any other remote-repo line should be removed or commented out)."
     p "After that, you can install a package into a sandbox by merely running: "
     pre "$ cabal update \n\
         \$ mkdir my-project
         \$ cd my-project
         \$ cabal sandbox init
         \$ cabal install the-package"
     p (a ! href "http://www.stackage.org/" $ "Go to Stackage →")

github =
  do h3 "From source control repositories"
     p "Installing from a source repository is also possible. For example, \
       \to clone and install the network package from source, you would run:"
     pre "$ git clone git@github.com:haskell/network.git\n\
         \$ cabal install network/"
     p "Or:"
     pre "$ git clone git@github.com:haskell/network.git\n\
         \$ cd network\n\
         \$ cabal install"
     p (a ! href "https://github.com/trending?l=haskell&since=monthly" $
          "Browse Github by Haskell repositories →")
