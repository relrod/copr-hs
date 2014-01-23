{-# LANGUAGE OverloadedStrings #-}

module Fedora.Copr.ListCoprs (
    Coprs (..)
  , Repo (..)) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import Data.Map (Map)
import qualified Data.Text as T

data Coprs = Coprs {
    output :: String
  , repos  :: Maybe [Repo]
  , error  :: Maybe String
} deriving (Eq, Show)

data Repo = Repo {
    yumRepos        :: Map T.Text T.Text
  , additionalRepos :: T.Text
  , instructions    :: T.Text
  , name            :: T.Text
  , description     :: T.Text
} deriving (Eq, Show)

instance FromJSON Repo where
  parseJSON (Object v) = Repo <$>
                             v .: "yum_repos"
                         <*> v .: "additional_repos"
                         <*> v .: "instructions"
                         <*> v .: "name"
                         <*> v .: "description"
  parseJSON _          = mzero

instance FromJSON Coprs where
  parseJSON (Object v) = Coprs <$>
                             v .: "output"
                         <*> v .:? "repos"
                         <*> v .:? "error"
  parseJSON _          = mzero
