{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.PostSilences
  ( PostSilences
  , postSilences

  , silence
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Alertmanager.API.Silence as OA
import qualified Alertmanager.Core as OA
import qualified Alertmanager.Model as OA

import Control.Lens.TH (makeLenses)
import Data.Text (Text)

data PostSilences = PostSilences
  { _silence :: OA.PostableSilence
  }

postSilences
  :: OA.PostableSilence
  -> PostSilences
postSilences = PostSilences

instance Req PostSilences where
  type Resp PostSilences = OA.InlineResponse200

  toAlertmanagerRequest PostSilences{..} =
    IgnoringMimeInfo $ OA.postSilences _silence

$(makeLenses ''PostSilences)
