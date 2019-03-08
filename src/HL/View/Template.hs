{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Templates.

module HL.View.Template where

import HL.Types
import HL.View

import Data.Monoid
import Yesod.Static (Static)

-- | Render a template.
template
  :: [Route App]
  -> Text
  -> ((Route App -> Text) -> Html ())
  -> FromLucid App
template crumbs ptitle inner =
  templateWithBodyEnder crumbs
                        ptitle
                        inner
                        (\_ _ -> return ())

-- | Render a template, with some additional content just before
-- </body>.
templateWithBodyEnder :: [Route App]
                      -> Text
                      -> ((Route App -> Text) -> Html ())
                      -> FromLucid App
                      -> FromLucid App
templateWithBodyEnder crumbs ptitle inner bodyender =
  skeleton ptitle
           (\_ _ -> return ())
           (\cur url ->
              div_ [class_ "template"]
                   (do navigation True crumbs cur url
                       container_ (bread url crumbs)
                       inner url))
           bodyender

-- | Render the basic site skeleton.
skeleton
  :: Text
  -> FromLucid App
  -> FromLucid App
  -> FromLucid App
  -> FromLucid App
skeleton ptitle innerhead innerbody bodyender mroute url =
  doctypehtml_
    (do head_ headinner
        body_ [class_ ("page-" <> toSlug route) | Just route <- [mroute]]
              (do bodyinner
                  analytics))
  where
    headinner =
      do title_ (toHtml ptitle)
         meta_ [charset_ "utf-8"]
         meta_ [httpEquiv_ "X-UA-Compatible",content_ "IE edge"]
         meta_ [name_ "viewport",content_ "width=device-width, initial-scale=1"]
         meta_ [name_ "keywords",content_ "haskell,functional,pure,programming,lazy"]
         meta_ [name_ "description",
                content_ "The Haskell purely functional programming language home page."]
         link_ [rel_ "shortcut icon",href_ (url (StaticR img_favicon_ico))]
         linkcss "https://fonts.googleapis.com/css?family=Source+Sans+Pro|Raleway:700,900|Ubuntu+Mono:400"
         styles url
                [StaticR css_hl_min_css]
         innerhead mroute url
    bodyinner =
      do div_ [class_ "wrap"] (innerbody mroute url)
         footer url mroute
         scripts url
                 [js_jquery_js
                 ,js_bootstrap_min_js
                 ,js_home_js]
         bodyender mroute url
    -- TODO: pop this in a config file later.
    analytics =
      script_ "var _gaq = _gaq || [];\n\
              \_gaq.push(['_setAccount', 'UA-83290513-1']);\n\
              \_gaq.push(['_trackPageview']);\n\
              \(function() {\n\
              \ var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n\
              \ ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
              \ var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n\
              \})();\n"

-- | Make a list of scripts.
scripts :: (Route App -> Text) -> [Route Static] -> Html ()
scripts url =
  mapM_ (\route ->
           script_ [src_ (url (StaticR route))] (mempty :: Text))

-- | Make a list of style links.
styles :: (a -> Text) -> [a] -> Html ()
styles url =
  mapM_ (\route ->
           linkcss (url route))

-- | A link to CSSxs
linkcss :: Text -> Html ()
linkcss uri =
  link_ [rel_ "stylesheet",type_ "text/css",href_ uri]

-- | Main navigation.
navigation :: Bool -> [Route App] -> FromLucid App
navigation showBrand crumbs mroute url =
  nav_ [class_ "navbar navbar-default"]
      (div_ [class_ "container"]
          (do div_ [class_ "navbar-header"]
                   (do button_ [class_ "navbar-toggle collapsed", data_ "toggle" "collapse", data_ "target" "#haskell-menu"]
                               (do span_ [class_ "sr-only"] ""
                                   span_ [class_ "icon-bar"] ""
                                   span_ [class_ "icon-bar"] ""
                                   span_ [class_ "icon-bar"] "")
                       when showBrand brand)
              items))
  where items =
          div_ [class_ "collapse navbar-collapse", id_ "haskell-menu"]
               (ul_ [class_ "nav navbar-nav navbar-right"]
                    (mapM_ item [DownloadsR,PackagesR,CommunityR,DocumentationR,NewsR]))
          where item :: Route App -> Html ()
                item route =
                  li_ [class_ "active" | Just route == mroute || elem route crumbs]
                      (a_ [href_ (url route)]
                          (toHtml (toHuman route)))
        brand = a_ [class_ "navbar-brand",href_ (url HomeR)]
                   (do logo url)

-- | The logo character in the right font. Style it with an additional
-- class or wrapper as you wish.
logo :: (Route App -> Text) -> Html ()
logo url = span_ [class_ "logo"] (do img_ [src_ (url (StaticR img_haskell_logo_svg))])

-- | Breadcrumb.
bread :: (Route App -> Text) -> [Route App] -> Html ()
bread url crumbs =
  ol_ [class_ "breadcrumb"]
      (forM_ crumbs
             (\route ->
                li_ (a_ [href_ (url route)]
                        (toHtml (toHuman route)))))

-- | Set the background image for an element.
background :: (Route App -> Text) -> Route Static -> Attribute
background url route =
  style_ ("background-image: url(" <> url (StaticR route) <> ")")

-- | Footer across the whole site.
footer :: (Route App -> Text) -> Maybe (Route App) -> Html ()
footer url _r =
  div_ [class_ "footer"]
       (div_ [class_ "container"]
             (p_ hlCopy))
  where hlCopy =
          do container_
                       (row_ (do span3_ [class_ "col-sm-4 col-md-3"]
                                        (span_ [class_ "item"] "\169 2014\8211\&2019 haskell.org")
                                 span12_ [class_ "col-xs-12 visible-xs"] (br_ [])
                                 span8_ [class_ "col-sm-4 col-md-6 text-center"]
                                        (do br_ [class_ "visible-xs"]
                                            span_ [class_ "item"] "Got changes to contribute to the site? "
                                            br_ [class_ "visible-xs"]
                                            a_ [href_ "https://github.com/haskell-infra/hl"] "Fork or comment on Github"
                                            br_ [class_ "visible-xs"])
                                 span12_ [class_ "col-xs-12 visible-xs"] (br_ [])
                                 span3_ [class_ "col-sm-4 col-md-3 text-right"]
                                        ((do span_ "Proudly hosted by "
                                             a_ [href_ "https://www.rackspace.com/"]
                                                (img_ [src_ (url (StaticR img_rackspace_svg))
                                                      ,alt_ "rackspace"
                                                      ,height_ "20"
                                                      ,width_ "20"])))
                                 span12_ [class_ "col-sm-12"] (br_ [])
                       ))
