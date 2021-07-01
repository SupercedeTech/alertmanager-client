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

import qualified Alertmanager.API.Silence as OA
import qualified Alertmanager.Core as OA
import qualified Alertmanager.Model as OA

import Control.Lens.TH (makeLenses)

data GetSilence = GetSilence
  { _silenceId :: OA.SilenceId
  }

getSilence
  :: OA.SilenceId
  -> GetSilence
getSilence = GetSilence

instance Req GetSilence where
  type Resp GetSilence = OA.GettableSilence

  toAlertmanagerRequest GetSilence{..} =
    IgnoringMimeInfo $ OA.getSilence _silenceId

$(makeLenses ''GetSilence)
