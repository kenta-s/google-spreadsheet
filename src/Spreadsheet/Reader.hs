{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Spreadsheet.Reader (fetchValueRanges) where

import           Data.Aeson
import           GHC.Generics
import qualified Data.ByteString.Char8   as S8
import qualified Network.HTTP.Simple     as HS
import qualified Network.HTTP.Conduit    as HC
import           System.Environment      (getEnv, getArgs)
import           Data.Text
import           Spreadsheet.AccessToken (fetchAccessToken)

data ValueRanges = ValueRanges {
  spreadsheetId :: Text,
  valueRanges :: [ValueRange]
} deriving (Generic, Show)
instance FromJSON ValueRanges

data ValueRange = ValueRange {
  values :: [[Text]]
} deriving (Generic, Show)
instance FromJSON ValueRange

fetchValueRanges :: String -> String -> IO [[Text]]
fetchValueRanges sheetId ranges = do
  token <- fetchAccessToken
  let token' = "Bearer " ++ (Data.Text.unpack token)
  req <- HS.parseRequest $ "https://sheets.googleapis.com/v4/spreadsheets/" ++ sheetId ++ "/values:batchGet?ranges=" ++ ranges
  let req' = HS.setRequestHeader "Authorization" [S8.pack token'] req
  res <- HS.httpJSON req'
  return $ values $ Prelude.head $ valueRanges (HS.getResponseBody res :: ValueRanges)
