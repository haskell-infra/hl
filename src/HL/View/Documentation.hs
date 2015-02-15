{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

-- | Documentation page view.

module HL.View.Documentation where

import HL.View
import HL.View.Template

-- | Documentation view.
documentationV :: FromLucid App
documentationV =
  template
    []
    "Documentation"
    (\url ->
       container_
         (row_
            (span12_ [class_ "col-md-12"]
               (do h1_ "Documentation"
                   books
                   online
                   manuals
                   report url))))

books :: Html ()
books =
  do h2_ "Books"
     p_ "Latest books for learning Haskell:"
     links modern
     p_ "Older books:"
     links older
  where
    modern =
      [("Learn You a Haskell for Great Good!","http://learnyouahaskell.com/")
      ,("Real World Haskell","http://book.realworldhaskell.org/")
      ,("Beginning Haskell","http://www.apress.com/9781430262503")
      ,("Thinking Functionally with Haskell","http://www.cambridge.org/us/academic/subjects/computer-science/programming-languages-and-applied-logic/thinking-functionally-haskell")
      ,("Parallel and Concurrent Programming in Haskell","http://chimera.labs.oreilly.com/books/1230000000929")]
    older =
      [("Programming in Haskell","http://www.cs.nott.ac.uk/~gmh/book.html")
      ,("Haskell: The Craft of Functional Programming","http://www.cs.ukc.ac.uk/people/staff/sjt/craft2e/")]

online :: Html ()
online =
  do h2_ "Online Resources"
     p_ "Resources put together by the Haskell community at-large:"
     links resources
  where
    resources =
      [("University of Pennsylvania's CIS 194","https://www.seas.upenn.edu/~cis194/")
      ,("Chris Allen's List of Haskell Resources","https://github.com/bitemyapp/learnhaskell")
      ]


manuals :: Html ()
manuals = do h2_ "Manuals and Guides"
             p_ "Manuals and guides that cover common Haskell tooling:"
             links tools
  where tools = [("GHC User Guide","http://www.haskell.org/ghc/docs/latest/html/users_guide/")
                ,("Cabal User Guide","http://www.haskell.org/cabal/users-guide/")
                ,("Haddock User Guide","http://www.haskell.org/haddock/doc/html/index.html")
                ,("What I Wish I Knew When Learning Haskell","http://dev.stephendiehl.com/hask/#cabal")
                ,("Haskeleton: A Haskell Project Skeleton","http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/")]

report :: (Route App -> Text) -> Html ()
report _ =
  do h2_ "Language Report"
     p_ (do "The Haskell 2010 language report is available online "
            a_ [href_ "//haskell.org/onlinereport/haskell2010/"] "here"
            ". ")
     p_ (do "A PDF version is available "
            a_ [href_ "//haskell.org/definition/haskell2010.pdf"] "here"
            ".")
     p_ "It can also be downloaded as a darcs repository: "
     p_ (pre_ (code_ "$ darcs get http://darcs.haskell.org/haskell2010-report"))

links :: [(Text,Text)] -> Html ()
links items =
  ul_ (forM_ items
             (\(title,url) ->
                li_ (a_ [href_ url] (toHtml title))))
