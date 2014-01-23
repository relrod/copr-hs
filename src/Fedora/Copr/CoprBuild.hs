{-# LANGUAGE OverloadedStrings #-}

module Fedora.Copr.CoprBuild (
    CoprBuild (..)
  , CoprBuildResponse (..)) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T

data CoprBuildResponse = CoprBuildResponse {
    output  :: String
  , buildId :: Maybe Int
  , message :: Maybe String
  , error   :: Maybe String
} deriving (Eq, Show)

instance FromJSON CoprBuildResponse where
  parseJSON (Object v) = CoprBuildResponse <$>
                             v .:  "output"
                         <*> v .:? "id"
                         <*> v .:? "message"
                         <*> v .:? "error"
  parseJSON _          = mzero

data CoprBuild = CoprBuild {
    packages :: NEL.NonEmpty T.Text
  , memory   :: Int
  , timeout  :: Int
} deriving (Eq, Show)

instance ToJSON CoprBuild where
  toJSON (CoprBuild p m t) = object [ "pkgs" .= T.intercalate " " (NEL.toList p)
                                    , "memory" .= m
                                    , "timeout" .= t
                                    ]
