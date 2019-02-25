{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Community page view.

module HL.View.Community where

import HL.View
import HL.View.Template

-- | Community view.
communityV :: FromLucid App
communityV =
  template
    []
    "Community"
    (\url ->
       container_
         (row_
            (span12_ [class_ "col-sm-12"]
               (do h1_ "Community"
                   p_
                     "Haskellers interact, talk and collaborate across several mediums \
                     \and around the world. There are places to learn, to teach, to ask questions, and to find contributors and collaborators."
                   h2_ "Online Communities and Social Resources"
                   p_ "Haskellers are active on a number of online areas, but the most busy are below:"
                   ul_ (online url)
                   h2_ "In-person Groups and Meetups"
                   p_ "There are a number of Haskell Users groups where haskellers meet to learn and code. Some are listed below:"
                   ul_ offline
                   h2_ "Conferences and Events"
                   p_ "There are a number of conferences and events featuring Haskell, some focusing on the academic side of things, and some on the commercial or hobbyist side. Here are just a few:"
                   h3_ "Academic Conferences"
                   ul_ academicConferences
                   h3_ "Non-Academic Conferences"
                   ul_ commercialConferences
                   h3_ "Hackathons"
                   p_ "Haskell Hackathons are a long tradition, with lots of learning and social exchange. In many ways they function as semi-structured conferences. Here are some of the most notable:"
                   ul_ hackathons
                   h2_ "Specific Interest Groups"
                   ul_ sigs
                   h2_ ""))))

online :: (Route App -> Text) -> Html ()
online url =
  do li_ (a_ [href_ (url MailingListsR)] "The Haskell mailing lists")
     li_ (a_ [href_ (url IrcR)] "IRC (online chat)")
     li_ (a_ [href_ "https://discourse.haskell.org"] "Haskell Community Discourse")
     li_ (a_ [href_ "http://stackoverflow.com/questions/tagged?tagnames=haskell"] "StackOverflow")
     li_ (a_ [href_ "https://www.facebook.com/groups/programming.haskell/"] "Facebook community")
     li_ (a_ [href_ "http://www.reddit.com/r/haskell"] "Reddit")
     li_ (a_ [href_ "http://www.haskell.org/haskellwiki/Haskell"] "Wiki")
     li_ (a_ [href_ "http://planet.haskell.org/"] "The blogosphere")
     li_ (a_ [href_ "https://haskellweekly.news/"] "Haskell Weekly e-mail newsletter")
     li_ (a_ [href_ "https://gitter.im/haskell-chat"] "Haskell Gitter Community Chat")
     li_ (a_ [href_ "https://wiki.haskell.org/Haskell_Communities_and_Activities_Report"] "Haskell Communities and Activities Report")

offline :: Html ()
offline =
  do li_ (a_ [href_ "http://www.meetup.com/ATX-Haskell/"] "Austin Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/Bay-Area-Haskell-Users-Group/"] "Bay Area Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/Boston-Haskell/"] "Boston Haskell")
     li_ (a_ [href_ "http://www.meetup.com/berlinhug/"] "Berlin Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/Brisbane-Functional-Programming-Group/"] "Brisbane Functional Programming Group")
     li_ (a_ [href_ "http://ChicagoHaskell.com/"] "Chicago Haskell")
     li_ (a_ [href_ "https://www.meetup.com/haskell-dublin-meetup/"] "Dublin Haskell Meetup")
     li_ (a_ [href_ "http://www.haskell-ita.it/"] "Italy Haskell Users Group")
     li_ (a_ [href_ "http://haskell.jp/"] "Japan Haskell Users Group (Haskell-jp)")
     li_ (a_ [href_ "http://www.meetup.com/NY-Haskell/"] "New York Haskell Users Group")
     li_ (a_ [href_ "http://www.meetup.com/London-Haskell/"] "London Haskell")
     li_ (a_ [href_ "http://seattlehaskell.org/"] "Seattle Area Haskell Users' Group (SeaHUG)")
     li_ (a_ [href_ "https://www.meetup.com/DelhiNCR-Haskell-And-Functional-Programming-Languages-Group/"] "Delhi NCR Haskell User Group")
     li_ (a_ [href_ "http://www.meetup.com/find/?allMeetups=true&keywords=Haskell&radius=Infinity"] "More Haskell meetups at meetup.com")

academicConferences :: Html ()
academicConferences =
  do li_ (a_ [href_ "https://www.haskell.org/haskell-symposium/"] "The Haskell Symposium")
     li_ (a_ [href_ "https://wiki.haskell.org/HaskellImplementorsWorkshop"] "Haskell Implementors' Workshop")
     li_ (a_ [href_ "http://www.icfpconference.org/"] "The International Conference on Functional Programming")
     li_ (a_ [href_ "http://popl.mpi-sws.org/"] "Symposium on Principles of Programming Languages")
     li_ (a_ [href_ "http://www.ifl-symposia.org/"] "International Symposia on Implementation and Application of Functional Languages")
     li_ (a_ [href_ "http://www.tifp.org/"] "Symposium on Trends in Functional Programming ")

commercialConferences :: Html ()
commercialConferences =
  do li_ (a_ [href_ "http://cufp.org/"] "Commercial Users of Functional Programming (Roving)")
     li_ (a_ [href_ "http://www.lambdajam.com/"] "LambdaJam (Chicago, IL, USA)")
     li_ (a_ [href_ "http://lambdajam.yowconference.com.au/"] "YOW! LambdaJam (Sydney, AU)")
     li_ (a_ [href_ "http://functionalconf.com/"] "Functional Conf (Bangalore, IN)")
     li_ (a_ [href_ "http://www.lambdacon.org/"] "LambdaCon (Bologna, IT)")
     li_ (a_ [href_ "http://bobkonf.de/"] "BOB (Berlin, DE)")
     li_ (a_ [href_ "http://www.iba-cg.de/hal9.html"] "HaL (Leipzig, Halle, DE)")
     li_ (a_ [href_ "https://skillsmatter.com/conferences/1907-haskell-exchange-2014"] "Skills Matter's Haskell eXchange (London, GB)")
     li_ (a_ [href_ "http://www.degoesconsulting.com/lambdaconf-2015/"] "LambdaConf (Boulder, CO, USA)")
     li_ (a_ [href_ "http://composeconference.org"] "Compose :: Conference (NY, NY, USA)")
     li_ (a_ [href_ "http://composeconference.org"] "Compose :: Melbourne (Melbourne, VIC, AU)")
     li_ (a_ [href_ "http://www.curry-on.org/"] "Curry On (Europe, roving)")

hackathons :: Html ()
hackathons =
  do li_ (a_ [href_ "https://wiki.haskell.org/BayHac"] "BayHac (San Francisco Bay Area, CA, USA)")
     li_ (a_ [href_ "https://wiki.haskell.org/Hac_Phi"] "Hac Phi (Philadelphia, PA, USA)")
     li_ (a_ [href_ "https://wiki.haskell.org/ZuriHac"] "ZuriHac (Zurich, CH)")
     li_ (a_ [href_ "https://wiki.haskell.org/HacFreiburg2017"] "HacFreiburg (Freiburg, De)")

sigs :: Html ()
sigs =
  do li_ (a_ [href_ "http://industry.haskell.org/"] "Industrial Haskell Group")
     li_ (a_ [href_ "http://commercialhaskell.com/"] "Commercial Haskell Group")
     li_ (a_ [href_ "http://www.datahaskell.org/"] "DataHaskell: data science, machine learning, numerical computation and related")
     li_ (a_ [href_ "http://lurk.org/groups/haskell-art/"] "Haskell Art")
