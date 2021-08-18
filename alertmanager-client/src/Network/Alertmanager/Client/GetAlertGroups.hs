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
import Network.Alertmanager.Client.Types

import qualified Network.Alertmanager.OpenAPI.API.Alertgroup as OA
import qualified Network.Alertmanager.OpenAPI.Core as OA ((-&-))

import Control.Lens.TH (makeLenses)

data GetAlertGroups = GetAlertGroups
  { _showActive :: Bool
  , _showSilenced :: Bool
  , _showInhibited :: Bool
  , _alertFilters :: Filter
  , _receiverFilter :: Maybe Receiver2
  }

getAlertGroups :: GetAlertGroups
getAlertGroups = GetAlertGroups
  { _showActive = True
  , _showSilenced = True
  , _showInhibited = True
  , _alertFilters = Filter []
  , _receiverFilter = Nothing
  }

instance Req GetAlertGroups where
  type Resp GetAlertGroups = [AlertGroup]

  toAlertmanagerRequest GetAlertGroups{..} =
    IgnoringMimeInfo $ OA.getAlertGroups
      OA.-&- Active _showActive
      OA.-&- Silenced _showSilenced
      OA.-&- Inhibited _showInhibited
      OA.-&- _alertFilters
      ?&? _receiverFilter

$(makeLenses ''GetAlertGroups)
