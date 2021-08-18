{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.GetAlertGroups
  ( GetAlertGroups
  , getAlertGroups

  , showActive
  , showSilenced
  , showInhibited
  , alertFilters
  , receiverFilter
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Network.Alertmanager.OpenAPI.API.Alertgroup as OA
import qualified Network.Alertmanager.OpenAPI.Core as OA
import qualified Network.Alertmanager.OpenAPI.Model as OA

import Control.Lens.TH (makeLenses)

data GetAlertGroups = GetAlertGroups
  { _showActive :: Bool
  , _showSilenced :: Bool
  , _showInhibited :: Bool
  , _alertFilters :: OA.Filter
  , _receiverFilter :: Maybe OA.Receiver2
  }

getAlertGroups :: GetAlertGroups
getAlertGroups = GetAlertGroups
  { _showActive = True
  , _showSilenced = True
  , _showInhibited = True
  , _alertFilters = OA.Filter []
  , _receiverFilter = Nothing
  }

instance Req GetAlertGroups where
  type Resp GetAlertGroups = [OA.AlertGroup]

  toAlertmanagerRequest GetAlertGroups{..} =
    IgnoringMimeInfo $ OA.getAlertGroups
      OA.-&- OA.Active _showActive
      OA.-&- OA.Silenced _showSilenced
      OA.-&- OA.Inhibited _showInhibited
      OA.-&- _alertFilters
      ?&? _receiverFilter

$(makeLenses ''GetAlertGroups)
