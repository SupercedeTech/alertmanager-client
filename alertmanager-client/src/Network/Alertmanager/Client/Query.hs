module Network.Alertmanager.Client.Query
  ( Req(Resp)

  , AlertmanagerRequest
  , send
  ) where

import Network.Alertmanager.Client.Class
import Network.Alertmanager.Client.Query.Internal

import qualified Alertmanager.Client as OA
import qualified Alertmanager.Core as OA
import qualified Alertmanager.MimeTypes as OA

import Control.Exception (Exception(..), throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T

data AlertmanagerClientException
  = MimeException !T.Text
  deriving (Show)

instance Exception AlertmanagerClientException where

send
  :: (MonadAlertmanagerClient m, Req r)
  => r
  -> m (Resp r)
send req = do
  connectionManager <- getConnectionManager
  clientConfig <- getConfig

  result <- withAlertmanagerRequest
    (\req respTrans ->
        fmap respTrans <$> liftIO (OA.dispatchMime connectionManager clientConfig req))
    (toAlertmanagerRequest req)

  case OA.mimeResult result of
    Left mimeErr -> liftIO $ throwIO $ MimeException $ T.pack $ OA.mimeError mimeErr
    Right success -> pure success
