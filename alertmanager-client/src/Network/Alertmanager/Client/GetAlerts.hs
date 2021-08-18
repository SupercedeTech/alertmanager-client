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
import Network.Alertmanager.Client.Types

import qualified Network.Alertmanager.OpenAPI.API.Alert as OA
import qualified Network.Alertmanager.OpenAPI.Core as OA ((-&-))

import Control.Lens.TH (makeLenses)

data GetAlerts = GetAlerts
  { _showActive :: Bool
  , _showSilenced :: Bool
  , _showInhibited :: Bool
  , _showUnprocessed :: Bool
  , _alertFilters :: Filter
  , _receiverFilter :: Maybe Receiver2
  }

getAlerts :: GetAlerts
getAlerts = GetAlerts
  { _showActive = True
  , _showSilenced = True
  , _showInhibited = True
  , _showUnprocessed = True
  , _alertFilters = Filter []
  , _receiverFilter = Nothing
  }

instance Req GetAlerts where
  type Resp GetAlerts = [GettableAlert]

  toAlertmanagerRequest GetAlerts{..} =
    IgnoringMimeInfo $ OA.getAlerts
      OA.-&- Active _showActive
      OA.-&- Silenced _showSilenced
      OA.-&- Inhibited _showInhibited
      OA.-&- _alertFilters
      ?&? _receiverFilter

$(makeLenses ''GetAlerts)
