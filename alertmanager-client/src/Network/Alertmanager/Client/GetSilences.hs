{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.GetSilences
  ( GetSilences
  , getSilences

  , filters
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Network.Alertmanager.OpenAPI.API.Silence as OA
import qualified Network.Alertmanager.OpenAPI.Core as OA
import qualified Network.Alertmanager.OpenAPI.Model as OA

import Control.Lens.TH (makeLenses)

data GetSilences = GetSilences
  { _filters :: OA.Filter
  }

getSilences
  :: GetSilences
getSilences = GetSilences
  { _filters = OA.Filter []
  }

instance Req GetSilences where
  type Resp GetSilences = [OA.GettableSilence]

  toAlertmanagerRequest GetSilences{..} =
    IgnoringMimeInfo $ OA.getSilences
      OA.-&- _filters

$(makeLenses ''GetSilences)
