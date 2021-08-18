{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.Alertmanager.Client.Trans
  ( AlertmanagerClientT

  , runAlertmanagerClientWithManager
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Core (AlertmanagerClientConfig)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..), asks, runReaderT)
import Network.HTTP.Client (Manager)

newtype AlertmanagerClientT m r = AlertmanagerClientT
  { unAlertmanagerClientT :: ReaderT (Manager, AlertmanagerClientConfig) m r }

instance MonadTrans AlertmanagerClientT where
  lift = AlertmanagerClientT . lift

deriving via (ReaderT (Manager, AlertmanagerClientConfig) m)
  instance Functor m => Functor (AlertmanagerClientT m)
deriving via (ReaderT (Manager, AlertmanagerClientConfig) m)
  instance Applicative m => Applicative (AlertmanagerClientT m)
deriving via (ReaderT (Manager, AlertmanagerClientConfig) m)
  instance Monad m => Monad (AlertmanagerClientT m)
deriving via (ReaderT (Manager, AlertmanagerClientConfig) m)
  instance MonadIO m => MonadIO (AlertmanagerClientT m)

instance MonadIO m => MonadAlertmanagerClient (AlertmanagerClientT m) where
  getConnectionManager = AlertmanagerClientT $ asks fst
  getConfig = AlertmanagerClientT $ asks snd

runAlertmanagerClientWithManager
  :: Manager
  -> AlertmanagerClientConfig
  -> AlertmanagerClientT m r
  -> m r
runAlertmanagerClientWithManager manager clientConfig action
  = runReaderT (unAlertmanagerClientT action) (manager, clientConfig)
