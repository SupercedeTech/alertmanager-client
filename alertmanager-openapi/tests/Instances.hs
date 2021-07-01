{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Alertmanager.Model
import Alertmanager.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)
    
arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models
 
instance Arbitrary Alert where
  arbitrary = sized genAlert

genAlert :: Int -> Gen Alert
genAlert n =
  Alert
    <$> arbitrary -- alertLabels :: (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- alertGeneratorUrl :: Maybe Text
  
instance Arbitrary AlertGroup where
  arbitrary = sized genAlertGroup

genAlertGroup :: Int -> Gen AlertGroup
genAlertGroup n =
  AlertGroup
    <$> arbitrary -- alertGroupLabels :: (Map.Map String Text)
    <*> arbitraryReduced n -- alertGroupReceiver :: Receiver
    <*> arbitraryReduced n -- alertGroupAlerts :: [GettableAlert]
  
instance Arbitrary AlertStatus where
  arbitrary = sized genAlertStatus

genAlertStatus :: Int -> Gen AlertStatus
genAlertStatus n =
  AlertStatus
    <$> arbitrary -- alertStatusState :: E'State2
    <*> arbitrary -- alertStatusSilencedBy :: [Text]
    <*> arbitrary -- alertStatusInhibitedBy :: [Text]
  
instance Arbitrary AlertmanagerConfig where
  arbitrary = sized genAlertmanagerConfig

genAlertmanagerConfig :: Int -> Gen AlertmanagerConfig
genAlertmanagerConfig n =
  AlertmanagerConfig
    <$> arbitrary -- alertmanagerConfigOriginal :: Text
  
instance Arbitrary AlertmanagerStatus where
  arbitrary = sized genAlertmanagerStatus

genAlertmanagerStatus :: Int -> Gen AlertmanagerStatus
genAlertmanagerStatus n =
  AlertmanagerStatus
    <$> arbitraryReduced n -- alertmanagerStatusCluster :: ClusterStatus
    <*> arbitraryReduced n -- alertmanagerStatusVersionInfo :: VersionInfo
    <*> arbitraryReduced n -- alertmanagerStatusConfig :: AlertmanagerConfig
    <*> arbitraryReduced n -- alertmanagerStatusUptime :: DateTime
  
instance Arbitrary ClusterStatus where
  arbitrary = sized genClusterStatus

genClusterStatus :: Int -> Gen ClusterStatus
genClusterStatus n =
  ClusterStatus
    <$> arbitraryReducedMaybe n -- clusterStatusName :: Maybe Text
    <*> arbitrary -- clusterStatusStatus :: E'Status
    <*> arbitraryReducedMaybe n -- clusterStatusPeers :: Maybe [PeerStatus]
  
instance Arbitrary GettableAlert where
  arbitrary = sized genGettableAlert

genGettableAlert :: Int -> Gen GettableAlert
genGettableAlert n =
  GettableAlert
    <$> arbitrary -- gettableAlertAnnotations :: (Map.Map String Text)
    <*> arbitraryReduced n -- gettableAlertReceivers :: [Receiver]
    <*> arbitrary -- gettableAlertFingerprint :: Text
    <*> arbitraryReduced n -- gettableAlertStartsAt :: DateTime
    <*> arbitraryReduced n -- gettableAlertUpdatedAt :: DateTime
    <*> arbitraryReduced n -- gettableAlertEndsAt :: DateTime
    <*> arbitraryReduced n -- gettableAlertStatus :: AlertStatus
    <*> arbitrary -- gettableAlertLabels :: (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- gettableAlertGeneratorUrl :: Maybe Text
  
instance Arbitrary GettableAlertAllOf where
  arbitrary = sized genGettableAlertAllOf

genGettableAlertAllOf :: Int -> Gen GettableAlertAllOf
genGettableAlertAllOf n =
  GettableAlertAllOf
    <$> arbitrary -- gettableAlertAllOfAnnotations :: (Map.Map String Text)
    <*> arbitraryReduced n -- gettableAlertAllOfReceivers :: [Receiver]
    <*> arbitrary -- gettableAlertAllOfFingerprint :: Text
    <*> arbitraryReduced n -- gettableAlertAllOfStartsAt :: DateTime
    <*> arbitraryReduced n -- gettableAlertAllOfUpdatedAt :: DateTime
    <*> arbitraryReduced n -- gettableAlertAllOfEndsAt :: DateTime
    <*> arbitraryReduced n -- gettableAlertAllOfStatus :: AlertStatus
  
instance Arbitrary GettableSilence where
  arbitrary = sized genGettableSilence

genGettableSilence :: Int -> Gen GettableSilence
genGettableSilence n =
  GettableSilence
    <$> arbitrary -- gettableSilenceId :: Text
    <*> arbitraryReduced n -- gettableSilenceStatus :: SilenceStatus
    <*> arbitraryReduced n -- gettableSilenceUpdatedAt :: DateTime
    <*> arbitraryReduced n -- gettableSilenceMatchers :: [Matcher]
    <*> arbitraryReduced n -- gettableSilenceStartsAt :: DateTime
    <*> arbitraryReduced n -- gettableSilenceEndsAt :: DateTime
    <*> arbitrary -- gettableSilenceCreatedBy :: Text
    <*> arbitrary -- gettableSilenceComment :: Text
  
instance Arbitrary GettableSilenceAllOf where
  arbitrary = sized genGettableSilenceAllOf

genGettableSilenceAllOf :: Int -> Gen GettableSilenceAllOf
genGettableSilenceAllOf n =
  GettableSilenceAllOf
    <$> arbitrary -- gettableSilenceAllOfId :: Text
    <*> arbitraryReduced n -- gettableSilenceAllOfStatus :: SilenceStatus
    <*> arbitraryReduced n -- gettableSilenceAllOfUpdatedAt :: DateTime
  
instance Arbitrary InlineResponse200 where
  arbitrary = sized genInlineResponse200

genInlineResponse200 :: Int -> Gen InlineResponse200
genInlineResponse200 n =
  InlineResponse200
    <$> arbitraryReducedMaybe n -- inlineResponse200SilenceId :: Maybe Text
  
instance Arbitrary Matcher where
  arbitrary = sized genMatcher

genMatcher :: Int -> Gen Matcher
genMatcher n =
  Matcher
    <$> arbitrary -- matcherName :: Text
    <*> arbitrary -- matcherValue :: Text
    <*> arbitrary -- matcherIsRegex :: Bool
    <*> arbitraryReducedMaybe n -- matcherIsEqual :: Maybe Bool
  
instance Arbitrary PeerStatus where
  arbitrary = sized genPeerStatus

genPeerStatus :: Int -> Gen PeerStatus
genPeerStatus n =
  PeerStatus
    <$> arbitrary -- peerStatusName :: Text
    <*> arbitrary -- peerStatusAddress :: Text
  
instance Arbitrary PostableAlert where
  arbitrary = sized genPostableAlert

genPostableAlert :: Int -> Gen PostableAlert
genPostableAlert n =
  PostableAlert
    <$> arbitraryReducedMaybe n -- postableAlertStartsAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- postableAlertEndsAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- postableAlertAnnotations :: Maybe (Map.Map String Text)
    <*> arbitrary -- postableAlertLabels :: (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- postableAlertGeneratorUrl :: Maybe Text
  
instance Arbitrary PostableAlertAllOf where
  arbitrary = sized genPostableAlertAllOf

genPostableAlertAllOf :: Int -> Gen PostableAlertAllOf
genPostableAlertAllOf n =
  PostableAlertAllOf
    <$> arbitraryReducedMaybe n -- postableAlertAllOfStartsAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- postableAlertAllOfEndsAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- postableAlertAllOfAnnotations :: Maybe (Map.Map String Text)
  
instance Arbitrary PostableSilence where
  arbitrary = sized genPostableSilence

genPostableSilence :: Int -> Gen PostableSilence
genPostableSilence n =
  PostableSilence
    <$> arbitraryReducedMaybe n -- postableSilenceId :: Maybe Text
    <*> arbitraryReduced n -- postableSilenceMatchers :: [Matcher]
    <*> arbitraryReduced n -- postableSilenceStartsAt :: DateTime
    <*> arbitraryReduced n -- postableSilenceEndsAt :: DateTime
    <*> arbitrary -- postableSilenceCreatedBy :: Text
    <*> arbitrary -- postableSilenceComment :: Text
  
instance Arbitrary PostableSilenceAllOf where
  arbitrary = sized genPostableSilenceAllOf

genPostableSilenceAllOf :: Int -> Gen PostableSilenceAllOf
genPostableSilenceAllOf n =
  PostableSilenceAllOf
    <$> arbitraryReducedMaybe n -- postableSilenceAllOfId :: Maybe Text
  
instance Arbitrary Receiver where
  arbitrary = sized genReceiver

genReceiver :: Int -> Gen Receiver
genReceiver n =
  Receiver
    <$> arbitrary -- receiverName :: Text
  
instance Arbitrary Silence where
  arbitrary = sized genSilence

genSilence :: Int -> Gen Silence
genSilence n =
  Silence
    <$> arbitraryReduced n -- silenceMatchers :: [Matcher]
    <*> arbitraryReduced n -- silenceStartsAt :: DateTime
    <*> arbitraryReduced n -- silenceEndsAt :: DateTime
    <*> arbitrary -- silenceCreatedBy :: Text
    <*> arbitrary -- silenceComment :: Text
  
instance Arbitrary SilenceStatus where
  arbitrary = sized genSilenceStatus

genSilenceStatus :: Int -> Gen SilenceStatus
genSilenceStatus n =
  SilenceStatus
    <$> arbitrary -- silenceStatusState :: E'State
  
instance Arbitrary VersionInfo where
  arbitrary = sized genVersionInfo

genVersionInfo :: Int -> Gen VersionInfo
genVersionInfo n =
  VersionInfo
    <$> arbitrary -- versionInfoVersion :: Text
    <*> arbitrary -- versionInfoRevision :: Text
    <*> arbitrary -- versionInfoBranch :: Text
    <*> arbitrary -- versionInfoBuildUser :: Text
    <*> arbitrary -- versionInfoBuildDate :: Text
    <*> arbitrary -- versionInfoGoVersion :: Text
  



instance Arbitrary E'State where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'State2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

