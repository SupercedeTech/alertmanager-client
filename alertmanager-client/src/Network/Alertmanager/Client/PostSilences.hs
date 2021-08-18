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
import Network.Alertmanager.Client.Types

import qualified Network.Alertmanager.OpenAPI.API.Silence as OA

import Control.Lens.TH (makeLenses)
import Data.Text (Text)

data PostSilences = PostSilences
  { _silence :: PostableSilence
  }

postSilences
  :: PostableSilence
  -> PostSilences
postSilences = PostSilences

instance Req PostSilences where
  type Resp PostSilences = InlineResponse200

  toAlertmanagerRequest PostSilences{..} =
    IgnoringMimeInfo $ OA.postSilences _silence

$(makeLenses ''PostSilences)
