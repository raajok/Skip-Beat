module Main where

import Data.Text (pack, Text)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B

import qualified SpotifyPlaylist as SP
import SpotifyTrack (TrackEntry, skippedTracks, skippedTracksOfPlaylist)

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