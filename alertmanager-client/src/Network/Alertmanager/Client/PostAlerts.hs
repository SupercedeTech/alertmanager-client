{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.PostAlerts
  ( PostAlerts
  , postAlerts

  , alerts
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Alertmanager.API.Alert as OA
import qualified Alertmanager.Core as OA
import qualified Alertmanager.Model as OA

import Control.Lens.TH (makeLenses)

data PostAlerts = PostAlerts
  { _alerts :: OA.Alerts
  }

postAlerts
  :: OA.Alerts
  -> PostAlerts
postAlerts = PostAlerts

instance Req PostAlerts where
  type Resp PostAlerts = ()

  toAlertmanagerRequest PostAlerts{..} =
    IgnoringMimeInfo $ OA.postAlerts _alerts

$(makeLenses ''PostAlerts)
