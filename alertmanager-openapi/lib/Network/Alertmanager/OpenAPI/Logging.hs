{-
   Alertmanager API

   API of the Prometheus Alertmanager (https://github.com/prometheus/alertmanager)

   OpenAPI Version: 3.0.1
   Alertmanager API API version: 0.0.1
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Network.Alertmanager.OpenAPI.Logging
Logging functions
-}
{-# LANGUAGE CPP #-}

#ifdef USE_KATIP

module Network.Alertmanager.OpenAPI.Logging
  ( module Network.Alertmanager.OpenAPI.LoggingKatip
  ) where

import Network.Alertmanager.OpenAPI.LoggingKatip

#else

module Network.Alertmanager.OpenAPI.Logging
  ( module Network.Alertmanager.OpenAPI.LoggingMonadLogger
  ) where

import Network.Alertmanager.OpenAPI.LoggingMonadLogger

#endif