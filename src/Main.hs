{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Char8 as S8
import qualified Network.HTTP.Simple   as HS
import qualified Network.HTTP.Conduit  as HC
import           System.Environment    (getEnv)
import           Data.Text
-- import qualified Data.Text.Encoding
-- import           Data.ByteString.UTF8

data AccessTokenResponse = AccessTokenResponse {
  access_token :: Text
} deriving (Generic, Show)
instance FromJSON AccessTokenResponse

data SubmittedData = SubmittedData {
  spreadsheetId :: Text,
  valueRanges :: [ValueRange]
} deriving (Generic, Show)
instance FromJSON SubmittedData

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

main :: IO ()
main = do
  token <- fetchAccessToken
  let token' = "Bearer " ++ (Data.Text.unpack token)
  let sheetId = "sheet id"
  req <- HS.parseRequest $ "https://sheets.googleapis.com/v4/spreadsheets/" ++ sheetId ++ "/values:batchGet?ranges=B2:C100"
  -- let req' = HS.setRequestHeader "Authorization" [S8.pack token'] req
  let req' = HS.setRequestHeaders [("Authorization", (S8.pack token')), ("Content-Type", "charset=utf-8")] req
  res <- HS.httpJSON req'
  -- res <- HS.httpLbs req'
  let dataList = HS.getResponseBody res :: SubmittedData
  print $ Prelude.head $ Prelude.tail $ Prelude.head $ values $ Prelude.head $ valueRanges dataList
  -- print $ HS.getResponseBody res
