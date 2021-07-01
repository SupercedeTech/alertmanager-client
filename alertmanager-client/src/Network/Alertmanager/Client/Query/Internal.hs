{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Alertmanager.Client.Query.Internal
  ( Req(..)

  , AlertmanagerRequest(..)
  , withAlertmanagerRequest

  , applyOptionalParams
  , (?&?)
  ) where

import qualified Alertmanager.Client as OA
import qualified Alertmanager.Core as OA
import qualified Alertmanager.MimeTypes as OA

import Control.Exception (Exception(..), throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (foldl')
import qualified Data.Text as T

class Req r where
  type Resp r :: *

  toAlertmanagerRequest :: r -> AlertmanagerRequest (Resp r)

data AlertmanagerRequest resp where
  IgnoringMimeInfo
    :: (OA.Produces req accept, OA.MimeUnrender accept res, OA.MimeType contentType)
    => OA.AlertmanagerRequest req contentType res accept
    -> AlertmanagerRequest res

withAlertmanagerRequest
  :: (forall req accept contentType.
      (OA.Produces req accept, OA.MimeUnrender accept resp, OA.MimeType contentType)
      => OA.AlertmanagerRequest req contentType resp accept
      -> result)
  -> AlertmanagerRequest resp
  -> result
withAlertmanagerRequest f (IgnoringMimeInfo inner) = f inner

applyOptionalParams
  :: (OA.HasOptionalParam req param, Foldable f)
  => OA.AlertmanagerRequest req contentType resp accept
  -> f param
  -> OA.AlertmanagerRequest req contentType resp accept
applyOptionalParams = foldl' OA.applyOptionalParam

(?&?)
  :: (OA.HasOptionalParam req param)
  => OA.AlertmanagerRequest req contentType resp accept
  -> Maybe param
  -> OA.AlertmanagerRequest req contentType resp accept
(?&?) = applyOptionalParams

infixl 2 ?&?
