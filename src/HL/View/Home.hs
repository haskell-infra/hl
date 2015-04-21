{-# LANGUAGE OverloadedStrings #-}

-- | Home/landing page.

module HL.View.Home where

import HL.View
import HL.View.Code
import HL.View.Home.Features
import HL.View.Template

-- | Home view.
homeV :: [(Text, Text, Text)] -> FromLucid App
homeV vids =
  skeleton
    "Haskell Language"
    (\_ _ ->
       linkcss "https://fonts.googleapis.com/css?family=Ubuntu:700")
    (\cur url ->
       do navigation True [] Nothing url
          header url
          try url
          community url vids
          features
          sponsors
          transition
          events
          div_ [class_ "mobile"] $
               (navigation False [] cur url))
    (\_ url ->
       scripts url
               [js_jquery_console_js
               ,js_tryhaskell_js
               ,js_tryhaskell_pages_js])

-- | Top header section with the logo and code sample.
header :: (Route App -> Text) -> Html ()
header url =
  div_ [class_ "header"] $
  (container_
     (row_ (do span6_ [class_ "col-md-6"]
                      (div_ [class_ "branding"]
                            (do branding
                                summation))
               span6_ [class_ "col-md-6"]
                      (div_ [class_ "branding"]
                            (do tag
                                sample)))))
  where branding =
          span_ [class_ "name",background url img_logo_png] "Haskell"
        summation =
          span_ [class_ "summary"] "A general-purpose purely-functional programming language"
        tag =
          span_ [class_ "tag"] "Declarative, statically typed code."
        sample =
          div_ [class_ "code-sample",title_ "This example is contrived in order to demonstrate what Haskell looks like, including: (1) where syntax, (2) enumeration syntax, (3) pattern matching, (4) consing as an operator, (5) list comprehensions, (6) infix functions. Don't take it seriously as an efficient prime number generator."]
               (haskellPre codeSample)

-- | Code sample.
-- TODO: should be rotatable and link to some article.
codeSample :: Text
codeSample =
  "primes = sieve [2..] \n\
  \  where sieve (p:xs) = \n\
  \          p : sieve [x | x <- xs, x `mod` p /= 0]"

-- | Try Haskell section.
try :: (Route App -> Text) -> Html ()
try _ =
  div_ [class_ "try",onclick_ "tryhaskell.controller.inner.click()"]
       (container_
          (row_ (do span6_ [class_ "col-md-6"] repl
                    span6_ [class_ "col-md-6",id_ "guide"]
                           (return ()))))
  where repl =
          do h2_ "Try it"
             div_ [id_ "console"]
                  (return ())

-- | Community section.
-- TOOD: Should contain a list of thumbnail videos. See mockup.
community :: (Route App -> Text) -> [(Text, Text, Text)] -> Html ()
community url vids =
  div_ [id_ "community-wrapper"]
       (do div_ [class_ "community",background url img_community_jpg]
                (do container_
                      [id_ "tagline"]
                      (row_ (span8_ [class_ "col-md-8"]
                                    (do h1_ "An open source community effort for over 20 years"
                                        p_ [class_ "learn-more"]
                                           (a_ [href_ (url CommunityR)] "Learn more"))))
                    container_
                      [id_ "video-description"]
                      (row_ (span8_ [class_ "col-md-8"]
                                    (do h1_ (a_ [id_ "video-anchor"] "<title here>")
                                        p_ (a_ [id_ "video-view"] "View the video now \8594")))))
           div_ [class_ "videos"]
                (container_ (row_ (span12_ [class_ "col-md-12"]
                                           (ul_ (forM_ vids vid))))))
  where vid :: (Text,Text,Text) -> Html ()
        vid (n,u,thumb) =
          li_ (a_ [class_ "vid-thumbnail",href_ u,title_ n]
                  (img_ [src_ thumb]))

-- | Information for people to help transition from the old site to the new locations.
transition :: Html ()
transition =
  div_ [class_ "transition"]
       (container_
          (row_ (span6_ [class_ "col-md-6"]
                        (do h1_ "Psst! Looking for the wiki?"
                            p_ (do "This is the new Haskell home page! The wiki has moved to "
                                   a_ [href_ "https://wiki.haskell.org"] "wiki.haskell.org.")))))

-- | Events section.
-- TODO: Take events section from Haskell News?
events :: Html ()
events =
  return ()

-- | List of sponsors.
sponsors :: Html ()
sponsors =
  div_ [class_ "sponsors"] $
    container_ $
      do row_ (span6_ [class_ "col-md-6"] (h1_ "Sponsors"))
         row_ (do span6_ [class_ "col-md-6"]
                         (p_ (do strong_ (a_ [href_ "https://www.datadoghq.com"] "DataDog")
                                 " provides powerful, customizable 24/7 metrics and monitoring \
                                 \integration for all of Haskell.org, and complains loudly for \
                                 \us when things go wrong."))
                  span6_ [class_ "col-md-6"]
                         (p_ (do strong_ (a_ [href_ "https://www.fastly.com"] "Fastly")
                                 "'s Next Generation CDN provides low latency access for all of \
                                 \Haskell.org's downloads and highest traffic services, including \
                                 \the primary Hackage server, Haskell Platform downloads, and more." )))
         row_ (do span6_ [class_ "col-md-6"]
                         (p_ (do strong_ (a_ [href_ "https://www.rackspace.com"] "Rackspace")
                                 " provides compute, storage, and networking resources, powering \
                                 \almost all of Haskell.org in several regions around the world."))
                  span6_ [class_ "col-md-6"]
                         (p_ (do strong_ (a_ [href_ "https://www.status.io"] "Status.io")
                                 " powers "
                                 a_ [href_ "https://status.haskell.org"] "https://status.haskell.org"
                                 ", and lets us easily tell you \
                                 \when we broke something." )))
         row_ (do span6_ [class_ "col-md-6"]
                         (p_ (do strong_ (a_ [href_ "http://www.galois.com"] "Galois")
                                 " provides infrastructure, funds, administrative resources and \
                                 \has historically hosted critical Haskell.org infrastructure, \
                                 \as well as helping the Haskell community at large with their work." ))
                  span6_ [class_ "col-md-6"]
                         (p_ (do strong_ (a_ [href_ "https://www.dreamhost.com"] "DreamHost")
                                 " has teamed up to provide Haskell.org with redundant, scalable object-storage \
                                 \through their Dream Objects service." )))
