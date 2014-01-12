{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module : Fedora.Copr
-- Copyright : (c) Ricky Elrod 2014
-- License : BSD3
-- Maintainer : ricky@elrod.me
-- Stability : experimental
-- Portability : portable
--
-- Provides a Haskell interface to the Fedora
-- <http://copr.fedoraproject.org/ Copr> build system
-- <http://copr.fedoraproject.org/api API>.

module Fedora.Copr
    (
      CoprConfig (..)
    , Username
    , defaultConfig
    , withConfig
    , coprs
    , new
    ) where

import Fedora.Copr.ListCoprs (Coprs)
import Fedora.Copr.NewCopr (CoprProject, NewCoprResponse)

import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as LS
import Data.Monoid (mappend)
import Network.Http.Client
import qualified System.IO.Streams.ByteString as SBS

data CoprConfig = CoprConfig {
    domain :: Hostname     -- ^ The domain on which Copr is hosted.
  , port   :: Port         -- ^ The port on which Copr operates.
  , login  :: S.ByteString -- ^ The API login (/not/ the same as username).
  , token  :: S.ByteString -- ^ The API token.
} deriving (Eq, Show)

type Username = S.ByteString

defaultConfig :: CoprConfig
defaultConfig = CoprConfig {
    domain = "copr.fedoraproject.org"
  , port = 80
  , login = ""
  , token = ""
}

withConfig :: CoprConfig -> (CoprConfig -> IO a) -> IO a
withConfig = flip id

-- | Perform a GET request to the API, with authentication.
--
--   Requests that don't need authentication result in the authentication
--   details being ignored, so we don't have to worry about not sending them
--   in that case.
apiGet :: FromJSON a => S.ByteString -> CoprConfig -> IO a
apiGet url c = do
  cnx <- openConnection (domain c) (port c)
  q <- buildRequest $ do
    http GET url
    setAccept "application/json"
    setContentType "application/json"
    setAuthorizationBasic (login c) (token c)

  sendRequest cnx q emptyBody
  x <- receiveResponse cnx jsonHandler
  closeConnection cnx
  return x

-- | Perform a POST request to the API, with authentication.
apiPost :: (ToJSON a, FromJSON b) => S.ByteString -> a -> CoprConfig -> IO b
apiPost url d c = do
  cnx <- openConnection (domain c) (port c)

  q <- buildRequest $ do
    http POST url
    setAccept "application/json"
    setContentType "application/json"
    setAuthorizationBasic (login c) (token c)
    setContentLength $ LS.length (encode d)

  body <- SBS.fromLazyByteString (encode d)
  sendRequest cnx q (inputStreamBody body)
  x <- receiveResponse cnx jsonHandler
  closeConnection cnx
  return x


-- | Retrieve a list of copr projects for an individual user.
--
--   This makes use of the @\/api\/coprs/[username]\/@ endpoint.
coprs :: Username   -- ^ The username of the person whose projects we want to list.
      -> CoprConfig -- ^ The configuration to use.
      -> IO Coprs
coprs u c = apiGet ("/api/coprs/" `mappend` u `mappend` "/") c

-- | Create a new copr project.
--
--   This makes use of the @\/api\/coprs/[username]\/new\/@ endpoint.
new :: Username       -- ^ The username of the person whose project should be created.
    -> CoprProject    -- ^ The copr project to be created.
    -> CoprConfig     -- ^ The configuration to use.
    -> IO NewCoprResponse
new u p c = apiPost ("/api/coprs/" `mappend` u `mappend` "/new/") p c
