{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Spreadsheet.Reader (fetchValueRanges, values) where

import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Char8 as S8
import qualified Network.HTTP.Simple   as HS
import qualified Network.HTTP.Conduit  as HC
import           System.Environment    (getEnv, getArgs)
import           Data.Text

data AccessTokenResponse = AccessTokenResponse {
  access_token :: Text
} deriving (Generic, Show)
instance FromJSON AccessTokenResponse

data ValueRanges = ValueRanges {
  spreadsheetId :: Text,
  valueRanges :: [ValueRange]
} deriving (Generic, Show)
instance FromJSON ValueRanges

data ValueRange = ValueRange {
  values :: [[Text]]
} deriving (Generic, Show)
instance FromJSON ValueRange

fetchAccessToken :: IO Text
fetchAccessToken = do
  refreshToken <- getEnv "SPREADSHEET_READER_REFRESH_TOKEN"
  clientId <- getEnv "SPREADSHEET_READER_CLIENT_ID"
  clientSecret <- getEnv "SPREADSHEET_READER_CLIENT_SECRET"

  let req = HS.setRequestMethod "POST" "https://www.googleapis.com/oauth2/v4/token"
      req' = HC.urlEncodedBody [("refresh_token", (S8.pack refreshToken)), ("client_id", (S8.pack clientId)), ("client_secret", (S8.pack clientSecret)), ("grant_type", "refresh_token")] req
  res <- HS.httpJSON req'
  let accessTokenRes = (HS.getResponseBody res :: AccessTokenResponse)
  return $ access_token accessTokenRes

fetchValueRanges :: String -> String -> IO [ValueRange]
fetchValueRanges sheetId ranges = do
  token <- fetchAccessToken
  let token' = "Bearer " ++ (Data.Text.unpack token)
  req <- HS.parseRequest $ "https://sheets.googleapis.com/v4/spreadsheets/" ++ sheetId ++ "/values:batchGet?ranges=" ++ ranges
  let req' = HS.setRequestHeader "Authorization" [S8.pack token'] req
  res <- HS.httpJSON req'
  return $ valueRanges (HS.getResponseBody res :: ValueRanges)
