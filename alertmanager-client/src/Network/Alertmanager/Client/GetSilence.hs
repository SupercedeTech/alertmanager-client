{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.GetSilence
  ( GetSilence
  , getSilence

  , silenceId
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal
import Network.Alertmanager.Client.Types

import qualified Network.Alertmanager.OpenAPI.API.Silence as OA

import Control.Lens.TH (makeLenses)

data GetSilence = GetSilence
  { _silenceId :: SilenceId
  }

getSilence
  :: SilenceId
  -> GetSilence
getSilence = GetSilence

instance Req GetSilence where
  type Resp GetSilence = GettableSilence

  toAlertmanagerRequest GetSilence{..} =
    IgnoringMimeInfo $ OA.getSilence _silenceId

$(makeLenses ''GetSilence)
