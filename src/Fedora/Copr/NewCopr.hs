{-# LANGUAGE OverloadedStrings #-}

module Fedora.Copr.NewCopr (
    CoprProject (..)
  , NewCoprResponse (..)) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T

data NewCoprResponse = NewCoprResponse {
    output  :: String
  , message :: Maybe String
  , error  :: Maybe String
} deriving (Eq, Show)

instance FromJSON NewCoprResponse where
  parseJSON (Object v) = NewCoprResponse <$>
                             v .:  "output"
                         <*> v .:? "message"
                         <*> v .:? "error"
  parseJSON _          = mzero

data CoprProject = CoprProject {
    name :: S.ByteString
  , repos :: [S.ByteString]
  , initialPackages :: [S.ByteString]
  , chroots :: [S.ByteString]
} deriving (Eq, Show)

instance ToJSON CoprProject where
  toJSON (CoprProject n r p c) = object $ [ "name" .= n
                                          , "repos" .= S.intercalate " " r
                                          , "initial_pkgs" .= S.intercalate " " p
                                          ] ++ map (\x -> (T.pack $ C8.unpack x) .= C8.pack "y") c
