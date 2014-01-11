{-# LANGUAGE OverloadedStrings #-}

module Fedora.Copr.ListCoprs (
    coprs
  , Coprs (..)
  , Repo (..)) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as S
import Data.Map (Map)
import Data.Monoid (mappend)
import Network.Http.Client
import qualified System.IO.Streams as Streams
import System.IO.Streams (InputStream, OutputStream, stdout)

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

coprs :: S.ByteString -> IO ()
coprs username = do
  c <- openConnection "copr.fedoraproject.org" 80
  q <- buildRequest $ do
    http GET ("/api/coprs/" `mappend` username `mappend` "/")
    setAccept "application/json"

  sendRequest c q emptyBody

  x <- receiveResponse c jsonHandler :: IO Coprs
  putStrLn $ show x

  closeConnection c
