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
    , addBuild
    , buildStatus
    , coprs
    , new
    ) where

import Fedora.Copr.CoprBuild (CoprBuild, CoprBuildResponse)
import Fedora.Copr.ListCoprs (Coprs)
import Fedora.Copr.NewCopr (CoprProject, NewCoprResponse)
import Fedora.Copr.CoprStatus (CoprStatusResponse)

import Data.Aeson
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
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
type ProjectName = S.ByteString

defaultConfig :: CoprConfig
defaultConfig = CoprConfig {
    domain = "copr.fedoraproject.org"
  , port = 80
  , login = ""
  , token = ""
}

-- | A utility wrapper for calling API methods with a 'CoprConfig'.
--
--   You can use this to do things like:
--
--   >>> let c = defaultConfig { login = "your_login", token = "your_token" }
--   >>> withConfig c $ coprs "codeblock"
withConfig :: CoprConfig -> (CoprConfig -> IO a) -> IO a
withConfig = flip id

-- | Sets things that are common to all requests that we make.
prepareRequest :: CoprConfig -> Method -> S.ByteString -> RequestBuilder ()
prepareRequest c m url = do
  http m url
  setAccept "application/json"
  setContentType "application/json"
  setAuthorizationBasic (login c) (token c)

finishRequest :: FromJSON a => Connection -> IO a
finishRequest cnx = do
  x <- receiveResponse cnx jsonHandler
  closeConnection cnx
  return x

-- | Perform a GET request to the API, with authentication.
--
--   Requests that don't need authentication result in the authentication
--   details being ignored, so we don't have to worry about not sending them
--   in that case.
apiGet :: FromJSON a => S.ByteString -> CoprConfig -> IO a
apiGet url c = do
  cnx <- openConnection (domain c) (port c)
  q <- buildRequest $ prepareRequest c GET url
  sendRequest cnx q emptyBody
  finishRequest cnx

-- | Perform a POST request to the API, with authentication.
apiPost :: (ToJSON a, FromJSON b) => S.ByteString -> a -> CoprConfig -> IO b
apiPost url d c = do
  cnx <- openConnection (domain c) (port c)
  q <- buildRequest $ do
    prepareRequest c POST url
    setContentLength $ LS.length (encode d)
  body <- SBS.fromLazyByteString (encode d)
  sendRequest cnx q (inputStreamBody body)
  finishRequest cnx

-- | Retrieve a list of copr projects for an individual user.
--
--   This makes use of the @\/api\/coprs/[username]\/@ endpoint.
--
--   > withConfig c $ coprs "codeblock"
coprs :: Username   -- ^ The username of the person whose projects we want to list.
      -> CoprConfig -- ^ The configuration to use.
      -> IO Coprs
coprs u = apiGet ("/api/coprs/" `mappend` u `mappend` "/")

-- | Create a new copr project.
--
--   This makes use of the @\/api\/coprs/[username]\/new\/@ endpoint.
--
--   > withConfig c $ new "codeblock" (CoprProject "testproject" [] [] (NEL.fromList ["fedora-20-x86_64"]))
new :: Username       -- ^ The username of the person whose project should be created.
    -> CoprProject    -- ^ The copr project to be created.
    -> CoprConfig     -- ^ The configuration to use.
    -> IO NewCoprResponse
new u = apiPost ("/api/coprs/" `mappend` u `mappend` "/new/")

-- | Add a build to a copr project.
--
--   This makes use of the @\/api\/coprs/[username]\/[project]\/new_build\/@ endpoint.
--
--   > withConfig c $ addBuild "codeblock" "testproject" (CoprBuild (NEL.fromList ["http://example.com/foo-1.0.0.src.rpm"]) 2048 3600)
addBuild :: Username    -- ^ The username of the person who owns the copr project.
         -> ProjectName -- ^ The project to add the build to.
         -> CoprBuild   -- ^ A representation of the build to add.
         -> CoprConfig  -- ^ The configuration to use.
         -> IO CoprBuildResponse
addBuild u p = apiPost ("/api/coprs/" `mappend` u `mappend` "/" `mappend` p `mappend` "/new_build/")

-- | Check the status of a copr build
--
--   This makes use of the @\/api\/coprs\/build_status/[build_id]\/@ endpoint.
--
--   > withConfig c $ buildStatus 1033
buildStatus :: Int        -- ^ The build ID number to check.
            -> CoprConfig -- ^ The configuration to use.
            -> IO CoprStatusResponse
buildStatus i = apiGet ("/api/coprs/build_status/" `mappend` (C8.pack (show i)) `mappend` "/")
