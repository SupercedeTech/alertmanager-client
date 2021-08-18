{-# LANGUAGE TemplateHaskell #-}

-- | Module: Network.Alertmanager.Client.Types
--
-- This module currently simply re-exports some API types from the generated
-- code, along with lens names that match the style used elsewhere in this
-- package (no exposed fields, lens names with no prefix or suffix). It
-- deliberately hides the constructors for all records, as a style choice.
--
-- In the future, it would be good to expose some first-class types with
-- better usage, but this will do for now.

module Network.Alertmanager.Client.Types
  -- * Newtypes
  --
  -- TODO: Eliminate unnecessary types & improve names. Some of these newtypes
  -- have no reason to be exposed to the user, so we should be able to omit them.
  ( OA.DateTime(..) -- NB: This comes from Core, not Model
  , OA.Active(..)
  , OA.Alerts(..)
  , OA.Filter(..)
  , OA.Inhibited(..)
  , OA.Receiver2(..)
  , OA.SilenceId(..)
  , OA.Silenced(..)
  , OA.Unprocessed(..)

  -- * Record types
  --
  -- TODO: These records leak implementation details in their use of newtypes for
  -- internal parsing instances. It would be better to replace them with definitions
  -- in this library and do the conversion to avoid exposing the newtypes to the
  -- user.
  , OA.Alert
  , OA.mkAlert
  , alertLabels
  , alertGeneratorUrl

  , OA.AlertGroup
  , OA.mkAlertGroup
  , alertGroupLabels
  , alertGroupReceiver
  , alertGroupAlerts

  , OA.AlertStatus
  , OA.mkAlertStatus
  , alertStatusState
  , alertStatusSilencedBy
  , alertStatusInhibitedBy

  , OA.AlertmanagerConfig
  , OA.mkAlertmanagerConfig
  , alertmanagerConfigOriginal

  , OA.AlertmanagerStatus
  , OA.mkAlertmanagerStatus
  , alertmanagerStatusCluster
  , alertmanagerStatusVersionInfo
  , alertmanagerStatusConfig
  , alertmanagerStatusUptime

  , OA.ClusterStatus
  , OA.mkClusterStatus
  , clusterStatusName
  , clusterStatusStatus
  , clusterStatusPeers

  , OA.GettableAlert
  , OA.mkGettableAlert
  , gettableAlertAnnotations
  , gettableAlertReceivers
  , gettableAlertFingerprint
  , gettableAlertStartsAt
  , gettableAlertUpdatedAt
  , gettableAlertEndsAt
  , gettableAlertStatus
  , gettableAlertLabels
  , gettableAlertGeneratorUrl

  , OA.GettableAlertAllOf
  , OA.mkGettableAlertAllOf
  , gettableAlertAllOfAnnotations
  , gettableAlertAllOfReceivers
  , gettableAlertAllOfFingerprint
  , gettableAlertAllOfStartsAt
  , gettableAlertAllOfUpdatedAt
  , gettableAlertAllOfEndsAt
  , gettableAlertAllOfStatus

  , OA.GettableSilence
  , OA.mkGettableSilence
  , gettableSilenceId
  , gettableSilenceStatus
  , gettableSilenceUpdatedAt
  , gettableSilenceMatchers
  , gettableSilenceStartsAt
  , gettableSilenceEndsAt
  , gettableSilenceCreatedBy
  , gettableSilenceComment

  , OA.GettableSilenceAllOf
  , OA.mkGettableSilenceAllOf
  , gettableSilenceAllOfId
  , gettableSilenceAllOfStatus
  , gettableSilenceAllOfUpdatedAt

  , OA.InlineResponse200
  , OA.mkInlineResponse200
  , inlineResponse200SilenceId

  , OA.Matcher
  , OA.mkMatcher
  , matcherName
  , matcherValue
  , matcherIsRegex
  , matcherIsEqual

  , OA.PeerStatus
  , OA.mkPeerStatus
  , peerStatusName
  , peerStatusAddress

  , OA.PostableAlert
  , OA.mkPostableAlert
  , postableAlertStartsAt
  , postableAlertEndsAt
  , postableAlertAnnotations
  , postableAlertLabels
  , postableAlertGeneratorUrl

  , OA.PostableAlertAllOf
  , OA.mkPostableAlertAllOf
  , postableAlertAllOfStartsAt
  , postableAlertAllOfEndsAt
  , postableAlertAllOfAnnotations

  , OA.PostableSilence
  , OA.mkPostableSilence
  , postableSilenceId
  , postableSilenceMatchers
  , postableSilenceStartsAt
  , postableSilenceEndsAt
  , postableSilenceCreatedBy
  , postableSilenceComment

  , OA.PostableSilenceAllOf
  , OA.mkPostableSilenceAllOf
  , postableSilenceAllOfId

  , OA.Receiver
  , OA.mkReceiver
  , receiverName

  , OA.Silence
  , OA.mkSilence
  , silenceMatchers
  , silenceStartsAt
  , silenceEndsAt
  , silenceCreatedBy
  , silenceComment

  , OA.SilenceStatus
  , OA.mkSilenceStatus
  , silenceStatusState

  , OA.VersionInfo
  , OA.mkVersionInfo
  , versionInfoVersion
  , versionInfoRevision
  , versionInfoBranch
  , versionInfoBuildUser
  , versionInfoBuildDate
  , versionInfoGoVersion

  -- * Enums
  --
  -- TODO: Fix naming - enough said.
  , OA.E'State(..)
  , OA.E'State2(..)
  , OA.E'Status(..)
  ) where

import Network.Alertmanager.Client.Types.Internal (makeFieldLenses)

import qualified Network.Alertmanager.OpenAPI.Core as OA
import qualified Network.Alertmanager.OpenAPI.Model as OA

$(makeFieldLenses ''OA.Alert)
$(makeFieldLenses ''OA.AlertGroup)
$(makeFieldLenses ''OA.AlertStatus)
$(makeFieldLenses ''OA.AlertmanagerConfig)
$(makeFieldLenses ''OA.AlertmanagerStatus)
$(makeFieldLenses ''OA.ClusterStatus)
$(makeFieldLenses ''OA.GettableAlert)
$(makeFieldLenses ''OA.GettableAlertAllOf)
$(makeFieldLenses ''OA.GettableSilence)
$(makeFieldLenses ''OA.GettableSilenceAllOf)
$(makeFieldLenses ''OA.InlineResponse200)
$(makeFieldLenses ''OA.Matcher)
$(makeFieldLenses ''OA.PeerStatus)
$(makeFieldLenses ''OA.PostableAlert)
$(makeFieldLenses ''OA.PostableAlertAllOf)
$(makeFieldLenses ''OA.PostableSilence)
$(makeFieldLenses ''OA.PostableSilenceAllOf)
$(makeFieldLenses ''OA.Receiver)
$(makeFieldLenses ''OA.Silence)
$(makeFieldLenses ''OA.SilenceStatus)
$(makeFieldLenses ''OA.VersionInfo)
