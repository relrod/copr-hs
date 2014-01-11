{-# LANGUAGE OverloadedStrings #-}

module Fedora.Copr.ListCoprs (
    Coprs (..)
  , Repo (..)) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as S
import Data.Map (Map)

data Coprs = Coprs {
    output :: String
  , repos  :: [Repo]
} deriving (Eq, Show)

data Repo = Repo {
    yumRepos        :: Map S.ByteString S.ByteString
  , additionalRepos :: S.ByteString
  , instructions    :: S.ByteString
  , name            :: S.ByteString
  , description     :: S.ByteString
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
                         <*> v .: "repos"
  parseJSON _          = mzero
