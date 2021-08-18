module Network.Alertmanager.Client.Class
  ( MonadAlertmanagerClient(..)
  ) where

import Network.Alertmanager.OpenAPI.Core (AlertmanagerClientConfig)

import Control.Monad.IO.Class (MonadIO(..))
import Network.HTTP.Client (Manager)

class MonadIO m => MonadAlertmanagerClient m where
  getConnectionManager :: m Manager
  getConfig :: m AlertmanagerClientConfig
