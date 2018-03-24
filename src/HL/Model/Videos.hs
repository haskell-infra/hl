{-# LANGUAGE OverloadedStrings #-}

-- | Model for videos.

module HL.Model.Videos
  (getHomeVideos)
  where

import Data.Text (Text)

-- | Get videos for the home page.
getHomeVideos :: Monad m => m [(Text, Text, Text)]
getHomeVideos =
  return vids

-- | For now we manually encode them until I have time to think of a
-- better way. This interface will be seamlessly replaceable with a
-- config file or so (as far as the controller and view are
-- concerned).
vids :: [(Text,Text, Text)]
vids =
  [("Escape from the ivory tower: The Haskell journey, by Simon Peyton-Jones", "https://www.youtube.com/watch?v=re96UgMk6GQ", "https://i1.ytimg.com/vi/re96UgMk6GQ/mqdefault.jpg")
  ,("Haskell taketh away: limiting side effects for parallel programming, by Ryan Newton", "https://www.youtube.com/watch?v=lC5UWG5N8oY", "https://i1.ytimg.com/vi/lC5UWG5N8oY/mqdefault.jpg")
  ,("Production Haskell, by Reid Draper", "https://www.youtube.com/watch?v=AZQLkkDXy68", "https://i1.ytimg.com/vi/AZQLkkDXy68/mqdefault.jpg")
  ,("Haskell Amuse-Bouche, by Mark Lentczner", "https://www.youtube.com/watch?v=b9FagOVqxmI", "https://i1.ytimg.com/vi/b9FagOVqxmI/mqdefault.jpg")
  ,("Haskell is Not For Production and Other Tales, by Katie Miller", "https://www.youtube.com/watch?v=mlTO510zO78", "https://i1.ytimg.com/vi/mlTO510zO78/mqdefault.jpg")
  ,("Validation with Smart Constructors, from Haskell at Work", "https://www.youtube.com/watch?v=lxjIUWGMUqE", "https://i1.ytimg.com/vi/lxjIUWGMUqE/mqdefault.jpg")
  ]
