{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.GetAlerts
  ( GetAlerts
  , getAlerts

  , showActive
  , showSilenced
  , showInhibited
  , alertFilters
  , receiverFilter
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Network.Alertmanager.OpenAPI.API.Alert as OA
import qualified Network.Alertmanager.OpenAPI.Core as OA
import qualified Network.Alertmanager.OpenAPI.Model as OA

import Control.Lens.TH (makeLenses)

data GetAlerts = GetAlerts
  { _showActive :: Bool
  , _showSilenced :: Bool
  , _showInhibited :: Bool
  , _showUnprocessed :: Bool
  , _alertFilters :: OA.Filter
  , _receiverFilter :: Maybe OA.Receiver2
  }

getAlerts :: GetAlerts
getAlerts = GetAlerts
  { _showActive = True
  , _showSilenced = True
  , _showInhibited = True
  , _showUnprocessed = True
  , _alertFilters = OA.Filter []
  , _receiverFilter = Nothing
  }

instance Req GetAlerts where
  type Resp GetAlerts = [OA.GettableAlert]

  toAlertmanagerRequest GetAlerts{..} =
    IgnoringMimeInfo $ OA.getAlerts
      OA.-&- OA.Active _showActive
      OA.-&- OA.Silenced _showSilenced
      OA.-&- OA.Inhibited _showInhibited
      OA.-&- _alertFilters
      ?&? _receiverFilter

$(makeLenses ''GetAlerts)
