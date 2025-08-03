{-# LANGUAGE DeriveGeneric #-}

module SpotifyPlaylist 
  ( 
    Track(..)
  , PlaylistItem(..)
  , Playlist(..)
  , PlaylistData(..)
  , getPlaylistByName
  ) where

import Data.Time (Day)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (find)

data Track = Track {
  trackName :: Text,
  artistName :: Text,
  albumName :: Text,
  trackUri :: Text
} deriving (Generic, Show)

instance FromJSON Track

data PlaylistItem = PlaylistItem {
  track :: Track,
  episode :: Maybe Value,      -- Using Value since we don't know the type (null in the data)
  audiobook :: Maybe Value,
  localTrack :: Maybe Value,
  addedDate :: Day
} deriving (Generic, Show)

instance FromJSON PlaylistItem

data Playlist = Playlist {
  name :: Text,
  lastModifiedDate :: Day,
  items :: [PlaylistItem]
} deriving (Generic, Show)

instance FromJSON Playlist

data PlaylistData = PlaylistData {
  playlists :: [Playlist]
} deriving (Generic, Show)

instance FromJSON PlaylistData

parsePlaylistFileWithError :: FilePath -> IO (Either String PlaylistData)
parsePlaylistFileWithError filepath = eitherDecode <$> B.readFile filepath  :: IO (Either String PlaylistData)

-- Get playlist by name from file
getPlaylistByName :: FilePath -> Text -> IO (Maybe Playlist)
getPlaylistByName filepath playlistName = do
  result <- parsePlaylistFileWithError filepath
  case result of
    Left _ -> return Nothing
    Right playlistData -> return (findPlaylistByName playlistName playlistData)

findPlaylistByName :: Text -> PlaylistData -> Maybe Playlist
findPlaylistByName searchName playlistData = 
  find (\p -> name p == searchName) (playlists playlistData)