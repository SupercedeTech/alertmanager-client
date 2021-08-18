{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.DeleteSilence
  ( DeleteSilence
  , deleteSilence

  , silenceId
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Network.Alertmanager.OpenAPI.API.Silence as OA
import qualified Network.Alertmanager.OpenAPI.Core as OA
import qualified Network.Alertmanager.OpenAPI.Model as OA

import Control.Lens.TH (makeLenses)

data DeleteSilence = DeleteSilence
  { _silenceId :: OA.SilenceId
  }

deleteSilence
  :: OA.SilenceId
  -> DeleteSilence
deleteSilence = DeleteSilence

instance Req DeleteSilence where
  type Resp DeleteSilence = ()

  toAlertmanagerRequest DeleteSilence{..} =
    IgnoringMimeInfoWithReturnType (OA.deleteSilence _silenceId) ignoreNoContent

$(makeLenses ''DeleteSilence)
