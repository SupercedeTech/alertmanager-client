{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.GetStatus
  ( GetStatus
  , getStatus
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Network.Alertmanager.OpenAPI.API.General as OA
import qualified Network.Alertmanager.OpenAPI.Core as OA
import qualified Network.Alertmanager.OpenAPI.Model as OA

import Control.Lens.TH (makeLenses)

data GetStatus = GetStatus

getStatus
  :: GetStatus
getStatus = GetStatus

instance Req GetStatus where
  type Resp GetStatus = OA.AlertmanagerStatus

  toAlertmanagerRequest getStatus =
    IgnoringMimeInfo OA.getStatus

$(makeLenses ''GetStatus)
