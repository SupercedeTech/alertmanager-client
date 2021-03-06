{-
   Alertmanager API

   API of the Prometheus Alertmanager (https://github.com/prometheus/alertmanager)

   OpenAPI Version: 3.0.1
   Alertmanager API API version: 0.0.1
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Network.Alertmanager.OpenAPI.API.Silence
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Network.Alertmanager.OpenAPI.API.Silence where

import Network.Alertmanager.OpenAPI.Core
import Network.Alertmanager.OpenAPI.MimeTypes
import Network.Alertmanager.OpenAPI.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Silence

-- *** deleteSilence

-- | @DELETE \/silence\/{silenceID}@
-- 
-- Delete a silence by its ID
-- 
deleteSilence 
  :: SilenceId -- ^ "silenceId" -  ID of the silence to get
  -> AlertmanagerRequest DeleteSilence MimeNoContent NoContent MimeNoContent
deleteSilence (SilenceId silenceId) =
  _mkRequest "DELETE" ["/silence/",toPath silenceId]

data DeleteSilence  
instance Produces DeleteSilence MimeNoContent


-- *** getSilence

-- | @GET \/silence\/{silenceID}@
-- 
-- Get a silence by its ID
-- 
getSilence 
  :: SilenceId -- ^ "silenceId" -  ID of the silence to get
  -> AlertmanagerRequest GetSilence MimeNoContent GettableSilence MimeJSON
getSilence (SilenceId silenceId) =
  _mkRequest "GET" ["/silence/",toPath silenceId]

data GetSilence  
-- | @application/json@
instance Produces GetSilence MimeJSON


-- *** getSilences

-- | @GET \/silences@
-- 
-- Get a list of silences
-- 
getSilences 
  :: AlertmanagerRequest GetSilences MimeNoContent [GettableSilence] MimeJSON
getSilences =
  _mkRequest "GET" ["/silences"]

data GetSilences  

-- | /Optional Param/ "filter" - A list of matchers to filter silences by
instance HasOptionalParam GetSilences Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toQueryColl MultiParamArray ("filter", Just xs)
-- | @application/json@
instance Produces GetSilences MimeJSON


-- *** postSilences

-- | @POST \/silences@
-- 
-- Post a new silence or update an existing one
-- 
postSilences 
  :: (Consumes PostSilences MimeJSON, MimeRender MimeJSON PostableSilence)
  => PostableSilence -- ^ "silence" -  The silence to create
  -> AlertmanagerRequest PostSilences MimeJSON InlineResponse200 MimeJSON
postSilences silence =
  _mkRequest "POST" ["/silences"]
    `setBodyParam` silence

data PostSilences 

-- | /Body Param/ "silence" - The silence to create
instance HasBodyParam PostSilences PostableSilence 

-- | @application/json@
instance Consumes PostSilences MimeJSON

-- | @application/json@
instance Produces PostSilences MimeJSON

