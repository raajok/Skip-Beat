{-# LANGUAGE DeriveGeneric #-}

module SpotifyTrack (
  TrackEntry(..),
  playedLimit,
  filterUnplayedTracks,
  playedTracks,
  uniqueTracks,
  trackNames,
  skippedTracks,
  skippedTracksOfPlaylist
) where

import Data.Time (LocalTime)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson
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

-- All tracks that have not been ever listened to
skippedTracks :: Either String [TrackEntry] -> Either String [TrackEntry] -> Maybe [Text]
skippedTracks te1 te2 = case (te1, te2) of
    (Right firstTracks, Right secondTracks) -> Just . trackNames . uniqueTracks $ filterUnplayedTracks (firstTracks ++ secondTracks)
    (_, _) ->  Nothing

skippedTracksOfPlaylist :: [Text] -> Maybe SP.Playlist -> Maybe [Text]
skippedTracksOfPlaylist _ Nothing = Nothing
skippedTracksOfPlaylist tracks (Just playlist) = Just (tracks `intersect` map (SP.trackName . SP.track) (SP.items playlist))