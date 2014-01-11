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
-- <http://copr.fedoraproject.org/ API>.

module Fedora.Copr
    (
      coprs
    ) where

import Fedora.Copr.ListCoprs (Coprs)

import qualified Data.ByteString.Char8 as C8
import Data.Monoid (mappend)
import Network.Http.Client

coprConnection :: IO Connection
coprConnection = openConnection "copr.fedoraproject.org" 80

-- | Retrieve a list of copr projects for an individual user.
--
--   This makes use of the @\/api\/coprs/[username]@ endpoint.
coprs :: String -> IO Coprs
coprs username = do
  c <- coprConnection
  q <- buildRequest $ do
    http GET ("/api/coprs/" `mappend` C8.pack username `mappend` "/")
    setAccept "application/json"

  sendRequest c q emptyBody
  x <- receiveResponse c jsonHandler :: IO Coprs
  closeConnection c
  return x
