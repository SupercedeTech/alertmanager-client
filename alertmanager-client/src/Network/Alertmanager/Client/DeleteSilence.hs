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

import qualified Alertmanager.API.Silence as OA
import qualified Alertmanager.Core as OA
import qualified Alertmanager.Model as OA

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
    IgnoringMimeInfo $ OA.deleteSilence _silenceId

$(makeLenses ''DeleteSilence)