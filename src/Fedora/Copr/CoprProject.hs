{-# LANGUAGE OverloadedStrings #-}

module Fedora.Copr.CoprProject (
    CoprProject (..)
  , NewCoprResponse (..)) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T

data NewCoprResponse = NewCoprResponse {
    output  :: String
  , message :: Maybe String
  , error   :: Maybe String
} deriving (Eq, Show)

instance FromJSON NewCoprResponse where
  parseJSON (Object v) = NewCoprResponse <$>
                             v .:  "output"
                         <*> v .:? "message"
                         <*> v .:? "error"
  parseJSON _          = mzero

data CoprProject = CoprProject {
    name            :: T.Text
  , repos           :: [T.Text]
  , initialPackages :: [T.Text]
  , chroots         :: NEL.NonEmpty T.Text
  , description     :: Maybe T.Text
  , instructions    :: Maybe T.Text
} deriving (Eq, Show)

instance ToJSON CoprProject where
  toJSON (CoprProject n r p c d i) = object $ [ "name" .= n
                                          , "repos" .= T.intercalate " " r
                                          , "initial_pkgs" .= T.intercalate " " p
                                          , "description" .= d
                                          , "instructions" .= i
                                          ] ++ map (\x -> x .= (T.pack "y")) (NEL.toList c)
