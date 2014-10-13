{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Features list.

module HL.V.Home.Features where

import HL.V hiding (list)
import HL.V.Code

import Data.List
import Data.Monoid
import Data.Ord

-- | Features section explains what's notable about Haskell as a
-- language.
features :: Html
features =
  div ! class_ "features" $
      (container
         (do h1 "Features"
             row (do span6 statically
                     span6 purefunc)
             row (do span6 inference
                     span6 concurrent)
             row (do span6 lazy
                     span6 packages)))

purefunc :: Html
purefunc =
  do h2 "Purely functional"
     p "Every function in Haskell is a function in the mathematical sense (i.e., \"pure\"). \
       \Even side-effecting IO operations are but a description of what to do, produced \
       \by pure code. There are no statements or instructions, only expressions which \
       \cannot mutate variables (local or global) nor access state like time or random \
       \numbers."
     p !. "expand" $ a "Click to expand"
     div !. "expandable" $ do
       p (do "The following function takes an integer and returns an integer. "
             "By the type it cannot do any side-effects whatsoever, it cannot\
             \ mutate any of its arguments.")
       haskellPre "square :: Int -> Int\n\
                  \square x = x * x"
       p (do "The following string concatenation is okay:")
       haskellPre "\"Hello: \" ++ \"World!\" "
       p (do "The following string concatenation is a type error:")
       rejectedHaskellPre "Type error" "\"Name: \" ++ getLine"
       p (do "Because "
             code "getLine"
             " has type "
             code "IO String"
             " and not "
             code "String"
             ", like "
             code "\"Name: \""
             " is. So by the type system you cannot mix and \
             \match purity with impurity.")

statically :: Html
statically =
  do h2 "Statically typed"
     p "Every expression in Haskell has a type which is determined at compile time. \
       \All the types composed together by function application have to match up. If \
       \they don't, the program will be rejected by the compiler. Types become not \
       \only a form of guarantee, but a language for expressing the construction \
       \of programs."
     p !. "expand" $ a "Click to expand"
     div !. "expandable" $ do
       p "All Haskell values have a type:"
       haskellPre "char = 'a'    :: Char\n\
                  \int = 123     :: Int\n\
                  \fun = isDigit :: Char -> Bool\n"
       p "You have to pass the right type of values to functions, or the compiler\
         \ will reject the program:"
       rejectedHaskellPre "Type error" "isDigit 1"
       p "You can decode bytes into text:"
       haskellPre "bytes = Crypto.Hash.SHA1.hash \"hello\" :: ByteString\n\
                  \text = decodeUtf8 bytes               :: Text\n"
       p "But you cannot decode Text, which is already a vector \
         \of Unicode points:"
       rejectedHaskellPre "Type error" "doubleDecode = decodeUtf8 (decodeUtf8 bytes)"

concurrent :: Html
concurrent =
  do h2 "Concurrent"
     p "Haskell lends itself well to concurrent programming due to its explicit \
       \handling of effects. Its flagship compiler, GHC, comes with a high-\
       \performance parallel garbage collector and light-weight concurrency \
       \library containing a number of useful concurrency primitives and \
       \abstractions."
     p !. "expand" $ a "Click to expand"
     div !. "expandable" $ do
       p "Easily launch threads and communicate with the standard library:"
       haskellPre "main = do\n\
                  \  done <- newEmptyMVar\n\
                  \  forkIO (do putStrLn \"I'm one thread!\"\n\
                  \             putMVar done \"Done!\")\n\
                  \  second <- forkIO (do delayThread 100000\n\
                  \                       putStrLn \"I'm another thread!\")\n\
                  \  killThread second\n\
                  \  msg <- takeMVar done\n\
                  \  putStrLn msg"
       p "Use an asynchronous API for threads:"
       haskellPre "do a1 <- async (getURL url1)\n\
                   \  a2 <- async (getURL url2)\n\
                   \  page1 <- wait a1\n\
                   \  page2 <- wait a2\n\
                   \  ..."
       p "Atomic threading with software transactional memory:"
       haskellPre "transfer :: Account -> Account -> Int -> IO ()\n\
                   \transfer from to amount =\n\
                   \  atomically (do deposit to amount\n\
                   \                 withdraw from amount)"
       p "Atomic transactions must be repeatable, so arbitrary IO is disabled in \
         \in the type system:"
       rejectedHaskellPre "Type error" "main = atomically (putStrLn \"Hello!\")"

inference :: Html
inference =
  do h2 "Type inference"
     p "You don't have to explicitly write out every type in a Haskell program. \
       \Types will be inferred by unifying every type bidirectionally. However, you \
       \can write out types if you choose, or ask the compiler to write them for you \
       \for handy documentation."
     p !. "expand" $ a "Click to expand"
     div !. "expandable" $ do
       p "This example has a type signature for every binding:"
       haskellPre "main :: IO ()\n\
                  \main = do line :: String <- getLine\n\
                  \          print (parseDigit line)\n\
                  \  where parseDigit :: String -> Maybe Int\n\
                  \        parseDigit ((c :: Char) : _) =\n\
                  \          if isDigit c\n\
                  \             then Just (ord c - ord '0')\n\
                  \             else Nothing"
       p "But you can just write:"
       haskellPre "main = do line <- getLine\n\
                  \          print (parseDigit line)\n\
                  \  where parseDigit (c : _) =\n\
                  \          if isDigit c\n\
                  \             then Just (ord c - ord '0')\n\
                  \             else Nothing"
       p "You can also use inference to avoid wasting time explaining \
         \what you want:"
       haskellPre "do ss <- decode \"[\\\"Hello!\\\",\\\"World!\\\"]\"\n\
                  \   is <- decode \"[1,2,3]\"\n\
                  \   return (zipWith (\\s i -> s ++ \" \" ++ show (i + 5)) ss is)\n\
                  \ => Just [\"Hello! 6\",\"World! 7\"]"
       p "Types give a parser specification for free, the following \
         \input is not accepted:"
       haskellPre "do ss <- decode \"[1,2,3]\"\n\
                  \   is <- decode \"[null,null,null]\"\n\
                  \   return (zipWith (\\s i -> s ++ \" \" ++ show (i + 5)) ss is)\n\
                  \ => Nothing"

lazy :: Html
lazy =
  do h2 "Lazy"
     p "Functions don't evaluate their arguments. This means that programs \
       \can compose together very well, with the ability to write control \
       \constructs (such as if/else) just by writing normal functions. The purity \
       \of Haskell code makes it easy to fuse chains of functions together, allowing \
       \for performance benefits."
     p !. "expand" $ a "Click to expand"
     div !. "expandable" $ do
       p "Define control structures easily:"
       haskellPre "when p m = if p then m else return ()\n\
                  \main = do args <- getArgs\n\
                  \          when (null args)\n\
                  \               (putStrLn \"No args specified!\") "
       p "If you notice a repeated expression pattern, like "
       haskellPre "if c then t else False"
       p "you can give this a name, like "
       haskellPre "and c t = if c then t else False"
       p "and then use it with the same effect as the orginal expression."
       p (do "Get code re-use by composing lazy functions. It's quite natural\
             \ to express the "
             code "any"
             " function by reusing the "
             code "map"
             " and "
             code "or"
             " functions:")
       haskellPre "any :: (a -> Bool) -> [a] -> Bool\n\
                  \any p = or . map p"
       p (do "Reuse the recursion patterns in "
             code "map"; ", "; code "filter"; ", "; code "foldr"; ", etc.")

packages :: Html
packages =
  do h2 "Packages"
     p "Free software contribution to Haskell is very active with a wide range \
       \of packages available on the public package servers."
     p !. "expand" $ a "Click to expand"
     div !. "expandable" $ do
       p "There are 6,954 packages freely available, here is a sample of the \
         \most common ones:"
       table !. "packages" $
         forM_ (alternating packages)
               (\((name1,desc1),(name,desc)) ->
                  tr (do td (a ! href ("https://hackage.haskell.org/package/" <> toValue name) $ toHtml name)
                         td (toHtml desc)
                         td !. "rhs" $ a ! href ("https://hackage.haskell.org/package/" <> toValue name1) $ toHtml name1
                         td !. "rhs" $ toHtml desc1))
  where package (name,desc) =
          li $ do a ! href (toValue name) $ toHtml name
                  " — "
                  toHtml desc
        packages :: [(Text,Text)]
        packages = [("base"             , "Prelude, IO, threads")
                   ,("bytestring"       , "Binary data")
                   ,("text"             , "Unicode text")
                   ,("network"          , "Networking")
                   ,("directory"        , "File/directory")
                   ,("parsec"           , "Parser library")
                   ,("attoparsec"       , "Fast parser")
                   ,("hspec"            , "RSpec-like tests")
                   ,("persistent"       , "Database ORM")
                   ,("monad-logger"     , "Logging")
                   ,("tar"              , "Tar archives")
                   ,("template-haskell" , "Meta-programming")
                   ,("time"             , "Date, time, etc.")
                   ,("snap"             , "Web framework")
                   ,("yesod"            , "Web framework")
                   ,("happstack"        , "Web framework")
                   ,("fsnotify"         , "Watch filesystem")
                   ,("containers"       , "Maps, graphs, sets")
                   ,("unix"             , "UNIX bindings")
                   ,("hint"             , "Interpret Haskell")
                   ,("OpenGL"           , "OpenGL graphics system")
                   ,("SDL"              , "SDL binding")
                   ,("pango"            , "Text rendering")
                   ,("criterion"        , "Benchmarking")
                   ,("statistics"       , "Statistical analysis")
                   ,("cairo"            , "Cairo graphics")
                   ,("glib"             , "GLib library")
                   ,("gtk"              , "Gtk+ library")
                   ,("resource-pool"    , "Resource pooling")
                   ,("test-framework"   , "Testing framework")
                   ,("mwc-random"       , "High-quality randoms")
                   ,("conduit"          , "Streaming I/O")
                   ,("stm"              , "Atomic threading")
                   ,("QuickCheck"       , "Property testing")
                   ,("cereal"           , "Binary parsing/printing")
                   ,("blaze-html"       , "Markup generation")
                   ,("http-client"      , "HTTP client engine")
                   ,("xml"              , "XML parser/printer")
                   ,("yaml"             , "YAML parser/printer")
                   ,("zlib"             , "zlib/gzip/raw")
                   ,("binary"           , "Serialization")
                   ,("pandoc"           , "Markup conversion")
                   ,("zip-archive"      , "Zip compression")
                   ,("tls"              , "TLS/SSL")
                   ,("text-icu"         , "Text encodings")
                   ,("warp"             , "Web server")
                   ,("async"            , "Asyn concurrency")
                   ,("vector"           , "Vectors")
                   ,("scientific"       , "Arbitrary-prec. nums")
                   ,("pipes"            , "Streaming IO")
                   ,("aeson"            , "JSON parser/printer")
                   ,("process"          , "Launch processes")
                   ,("syb"              , "Generic prog.")
                   ,("dlist"            , "Difflists")]

alternating (x:y:xs) = (x,y) : alternating xs
alternating _ = []
