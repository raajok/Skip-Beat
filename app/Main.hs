{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Time (LocalTime)
import Data.Text ( Text, pack )
import GHC.Generics ( Generic )
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List (nub, intersect, (\\))

import qualified SpotifyPlaylist as SP

data TrackEntry = TrackEntry {
  endTime :: LocalTime,
  artistName :: Text,
  trackName :: Text,
  msPlayed :: Int
} deriving (Generic, Show)

instance FromJSON TrackEntry

instance Eq TrackEntry where
  (==) track1 track2 =
    artistName track1 == artistName track2 &&
    trackName track1 == trackName track2

-- The amount of milliseconds the track must have been played to count as a listened song
playedLimit :: Int
playedLimit = 30000 -- 30 seconds

-- hard coded file paths and playlist name for now
firstJsonFile :: FilePath
firstJsonFile = "StreamingHistory_music_0.json"

secondJsonFile :: FilePath
secondJsonFile = "StreamingHistory_music_1.json"

playlistJsonFile :: FilePath
playlistJsonFile = "Playlist1.json"

playlistName :: Text
playlistName = pack "---..."

getJSONFromFile :: FilePath -> IO B.ByteString
getJSONFromFile = B.readFile

-- Takes out tracks that have been played leaving unplayed tracks to the array
filterUnplayedTracks :: [TrackEntry] -> [TrackEntry]
filterUnplayedTracks tracks = uniqueTracks tracks \\ playedTracks tracks

-- All the tracks that have been played more than 30 seconds at least once
playedTracks :: [TrackEntry] -> [TrackEntry]
playedTracks = uniqueTracks . filter (\x -> msPlayed x > playedLimit)

-- Removes duplicate tracks from the list
uniqueTracks :: [TrackEntry] -> [TrackEntry]
uniqueTracks = nub

-- TrackEntries list to a list of only track names
trackNames :: [TrackEntry] -> [Text]
trackNames = map trackName

-- All tracks that have not been ever listened
skippedTracks :: Either String [TrackEntry] -> Either String [TrackEntry] -> Maybe [Text]
skippedTracks te1 te2 = case (te1, te2) of
    (Right firstTracks, Right secondTracks) -> Just . trackNames . uniqueTracks $ filterUnplayedTracks (firstTracks ++ secondTracks)
    (_, _) ->  Nothing

skippedTracksOfPlaylist :: [Text] -> Maybe SP.Playlist -> Maybe [Text]
skippedTracksOfPlaylist _ Nothing = Nothing
skippedTracksOfPlaylist tracks (Just playlist) = Just (tracks `intersect` map (SP.trackName . SP.track) (SP.items playlist))

-- Run with "cabal run" in root folder
main :: IO ()
main = do
  firstDecoded <- (eitherDecode <$> getJSONFromFile firstJsonFile) :: IO (Either String [TrackEntry])
  secondDecoded <- (eitherDecode <$> getJSONFromFile secondJsonFile) :: IO (Either String [TrackEntry])
  playlist <- SP.getPlaylistByName playlistJsonFile playlistName
  case skippedTracks firstDecoded secondDecoded of
    Just tracks -> do
      case skippedTracksOfPlaylist tracks playlist of
        Just skippedPlaylistTracks -> do
          mapM_ print skippedPlaylistTracks
          putStrLn $ "Amount of skipped tracks in the playlist: " ++ show (length skippedPlaylistTracks)
          putStrLn $ "Amount of skipped tracks in total: " ++ show (length tracks)
        Nothing -> putStrLn "Failed to parse playlist tracks"
    Nothing -> putStrLn "Failed to parse track history"