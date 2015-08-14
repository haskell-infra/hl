-- | Devel web server.

module DevelMain where

import HL.Dispatch ()
import HL.Foundation

import Control.Concurrent
import Data.IORef
import Foreign.Store
import Network.Wai.Handler.Warp
import System.Directory
import System.Environment (getEnvironment)
import System.FilePath
import Yesod
import Yesod.Static

-- | Start the web server.
main :: IO (Store (IORef Application))
main =
  do st <- static "static"
     tmpDir <- getTemporaryDirectory
     let cacheDir = tmpDir </> "hl-cache"
     createDirectoryIfMissing True cacheDir
     cacheVar <- newMVar cacheDir
     app <- toWaiApp (App st cacheVar)
     ref <- newIORef app
     env <- getEnvironment
     let port = maybe 1990 read $ lookup "PORT" env
     tid <- forkIO
              (runSettings
                 (setPort port defaultSettings)
                 (\req cb ->
                    do handler <- readIORef ref
                       handler req cb))
     _ <- newStore tid
     ref' <- newStore ref
     _ <- newStore cacheVar
     return ref'

-- | Update the server, start it if not running.
update :: IO (Store (IORef Application))
update =
  do m <- lookupStore 1
     case m of
       Nothing -> main
       Just store ->
         do ref <- readStore store
            cacheVar <- readStore (Store 2)
            st <- static "static"
            app <- toWaiApp (App st cacheVar)
            writeIORef ref app
            return store
