{-# LANGUAGE OverloadedStrings #-}

module Fedora.Copr.CoprStatus (
    CoprStatusResponse (..)) where

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as S

data CoprStatusResponse = CoprStatusResponse {
    output :: String
  , status :: Maybe String
  , error  :: Maybe String
} deriving (Eq, Show)

instance FromJSON CoprStatusResponse where
  parseJSON (Object v) = CoprStatusResponse <$>
                             v .:  "output"
                         <*> v .:? "status"
                         <*> v .:? "error"
  parseJSON _          = mzero
