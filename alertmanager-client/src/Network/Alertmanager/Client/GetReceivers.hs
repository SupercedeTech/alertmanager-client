{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.GetReceivers
  ( GetReceivers
  , getReceivers
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Alertmanager.API.Receiver as OA
import qualified Alertmanager.Core as OA
import qualified Alertmanager.Model as OA

import Control.Lens.TH (makeLenses)

data GetReceivers = GetReceivers

getReceivers
  :: GetReceivers
getReceivers = GetReceivers

instance Req GetReceivers where
  type Resp GetReceivers = [OA.Receiver]

  toAlertmanagerRequest getReceivers =
    IgnoringMimeInfo OA.getReceivers

$(makeLenses ''GetReceivers)
