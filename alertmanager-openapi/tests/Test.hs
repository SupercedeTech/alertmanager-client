{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Alertmanager.Model
import Alertmanager.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy Alert)
      propMimeEq MimeJSON (Proxy :: Proxy AlertGroup)
      propMimeEq MimeJSON (Proxy :: Proxy AlertStatus)
      propMimeEq MimeJSON (Proxy :: Proxy AlertmanagerConfig)
      propMimeEq MimeJSON (Proxy :: Proxy AlertmanagerStatus)
      propMimeEq MimeJSON (Proxy :: Proxy ClusterStatus)
      propMimeEq MimeJSON (Proxy :: Proxy GettableAlert)
      propMimeEq MimeJSON (Proxy :: Proxy GettableAlertAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy GettableSilence)
      propMimeEq MimeJSON (Proxy :: Proxy GettableSilenceAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponse200)
      propMimeEq MimeJSON (Proxy :: Proxy Matcher)
      propMimeEq MimeJSON (Proxy :: Proxy PeerStatus)
      propMimeEq MimeJSON (Proxy :: Proxy PostableAlert)
      propMimeEq MimeJSON (Proxy :: Proxy PostableAlertAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy PostableSilence)
      propMimeEq MimeJSON (Proxy :: Proxy PostableSilenceAllOf)
      propMimeEq MimeJSON (Proxy :: Proxy Receiver)
      propMimeEq MimeJSON (Proxy :: Proxy Silence)
      propMimeEq MimeJSON (Proxy :: Proxy SilenceStatus)
      propMimeEq MimeJSON (Proxy :: Proxy VersionInfo)
      
