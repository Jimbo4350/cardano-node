{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Protocol.Shelley
  ( mkSomeConsensusProtocolShelley

    -- * Errors
  , ShelleyProtocolInstantiationError(..)
  , GenesisReadError(..)
  , GenesisValidationError(..)
  , PraosLeaderCredentialsError(..)

    -- * Reusable parts
  , readGenesis
  , readGenesisAny
  , readLeaderCredentials
  , genesisHashToPraosNonce
  , validateGenesis
  ) where

import           Cardano.Prelude
import           Prelude (String, id)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   newExceptT)

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (coerceKeyRole)

import qualified Ouroboros.Consensus.Cardano as Consensus

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)
import           Ouroboros.Consensus.Shelley.Node (Nonce (..), ProtocolParamsShelley (..),
                   ProtocolParamsShelleyBased (..), TPraosLeaderCredentials (..))
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCanBeLeader (..))

import qualified Shelley.Spec.Ledger.Genesis as Shelley
import           Shelley.Spec.Ledger.PParams (ProtVer (..))

import qualified Cardano.Api as Api (FileError (..))
import           Cardano.Api.Orphans ()
import qualified Cardano.Api.Protocol.Types as Protocol
import           Cardano.Api.Shelley hiding (FileError)


import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.HardFork ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Cardano.TraceDispatcher.Era.HardFork ()
import           Cardano.TraceDispatcher.Era.Shelley ()
import           Cardano.TraceDispatcher.Formatting ()

import           Cardano.Node.Protocol.Types

------------------------------------------------------------------------------
-- Shelley protocol
--

-- | Make 'SomeConsensusProtocol' using the Shelley instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
mkSomeConsensusProtocolShelley
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ShelleyProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolShelley NodeShelleyProtocolConfiguration {
                                  npcShelleyGenesisFile,
                                  npcShelleyGenesisFileHash
                                }
                          files = do
    (genesis, genesisHash) <- firstExceptT GenesisReadError $
                              readGenesis npcShelleyGenesisFile
                                          npcShelleyGenesisFileHash
    firstExceptT GenesisValidationError $ validateGenesis genesis
    leaderCredentials <- firstExceptT PraosLeaderCredentialsError $
                         readLeaderCredentials files

    return $ SomeConsensusProtocol Protocol.ShelleyBlockType $ Protocol.ProtocolInfoArgsShelley
      Consensus.ProtocolParamsShelleyBased {
        shelleyBasedGenesis = genesis,
        shelleyBasedInitialNonce = genesisHashToPraosNonce genesisHash,
        shelleyBasedLeaderCredentials =
            leaderCredentials
      }
      Consensus.ProtocolParamsShelley {
        shelleyProtVer =
          ProtVer 2 0
      }

genesisHashToPraosNonce :: GenesisHash -> Nonce
genesisHashToPraosNonce (GenesisHash h) = Nonce (Crypto.castHash h)

readGenesis :: GenesisFile
            -> Maybe GenesisHash
            -> ExceptT GenesisReadError IO
                       (ShelleyGenesis StandardShelley, GenesisHash)
readGenesis = readGenesisAny

readGenesisAny :: FromJSON genesis
               => GenesisFile
               -> Maybe GenesisHash
               -> ExceptT GenesisReadError IO (genesis, GenesisHash)
readGenesisAny (GenesisFile file) mbExpectedGenesisHash = do
    content <- handleIOExceptT (GenesisReadFileError file) $
                 BS.readFile file
    let genesisHash = GenesisHash (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (GenesisDecodeError file) $ hoistEither $
                 Aeson.eitherDecodeStrict' content
    return (genesis, genesisHash)
  where
    checkExpectedGenesisHash :: GenesisHash
                             -> ExceptT GenesisReadError IO ()
    checkExpectedGenesisHash actual =
      case mbExpectedGenesisHash of
        Just expected | actual /= expected
          -> throwError (GenesisHashMismatch actual expected)
        _ -> return ()

validateGenesis :: ShelleyGenesis StandardShelley
                -> ExceptT GenesisValidationError IO ()
validateGenesis genesis =
    firstExceptT GenesisValidationErrors . hoistEither $
      Shelley.validateGenesis genesis

readLeaderCredentials :: Maybe ProtocolFilepaths
                      -> ExceptT PraosLeaderCredentialsError IO
                                 [TPraosLeaderCredentials StandardCrypto]
readLeaderCredentials Nothing = return []
readLeaderCredentials (Just pfp) =
  -- The set of credentials is a sum total of what comes from the CLI,
  -- as well as what's in the bulk credentials file.
  (<>) <$> readLeaderCredentialsSingleton pfp
       <*> readLeaderCredentialsBulk      pfp

readLeaderCredentialsSingleton ::
     ProtocolFilepaths ->
     ExceptT PraosLeaderCredentialsError IO
             [TPraosLeaderCredentials StandardCrypto]
-- It's OK to supply none of the files on the CLI
readLeaderCredentialsSingleton
   ProtocolFilepaths
     { shelleyCertFile      = Nothing,
       shelleyVRFFile       = Nothing,
       shelleyKESFile       = Nothing
     } = pure []
-- Or to supply all of the files
readLeaderCredentialsSingleton
   ProtocolFilepaths
     { shelleyCertFile      = Just certFile,
       shelleyVRFFile       = Just vrfFile,
       shelleyKESFile       = Just kesFile
     } =
     fmap (:[]) $
     mkPraosLeaderCredentials
       <$> firstExceptT FileError (newExceptT $ readFileTextEnvelope AsOperationalCertificate certFile)
       <*> firstExceptT FileError (newExceptT $ readFileTextEnvelope (AsSigningKey AsVrfKey) vrfFile)
       <*> firstExceptT FileError (newExceptT $ readFileTextEnvelope (AsSigningKey AsKesKey) kesFile)
-- But not OK to supply some of the files without the others.
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyCertFile = Nothing} =
     throwError OCertNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyVRFFile = Nothing} =
     throwError VRFKeyNotSpecified
readLeaderCredentialsSingleton ProtocolFilepaths {shelleyKESFile = Nothing} =
     throwError KESKeyNotSpecified

data ShelleyCredentials
  = ShelleyCredentials
    { scCert :: (TextEnvelope, FilePath)
    , scVrf  :: (TextEnvelope, FilePath)
    , scKes  :: (TextEnvelope, FilePath)
    }

readLeaderCredentialsBulk ::
     ProtocolFilepaths
  -> ExceptT PraosLeaderCredentialsError IO
             [TPraosLeaderCredentials StandardCrypto]
readLeaderCredentialsBulk ProtocolFilepaths { shelleyBulkCredsFile = mfp } =
  mapM parseShelleyCredentials =<< readBulkFile mfp
 where
   parseShelleyCredentials ::
        ShelleyCredentials
     -> ExceptT PraosLeaderCredentialsError IO
                (TPraosLeaderCredentials StandardCrypto)
   parseShelleyCredentials ShelleyCredentials { scCert, scVrf, scKes } = do
     mkPraosLeaderCredentials
       <$> parseEnvelope AsOperationalCertificate scCert
       <*> parseEnvelope (AsSigningKey AsVrfKey) scVrf
       <*> parseEnvelope (AsSigningKey AsKesKey) scKes

   readBulkFile :: Maybe FilePath
                -> ExceptT PraosLeaderCredentialsError IO
                           [ShelleyCredentials]
   readBulkFile Nothing = pure []
   readBulkFile (Just fp) = do
     content <- handleIOExceptT (CredentialsReadError fp) $
                  BS.readFile fp
     envelopes <- firstExceptT (EnvelopeParseError fp) $ hoistEither $
                    Aeson.eitherDecodeStrict' content
     pure $ uncurry mkCredentials <$> zip [0..] envelopes
    where
      mkCredentials :: Int -> (TextEnvelope, TextEnvelope, TextEnvelope)
                    -> ShelleyCredentials
      mkCredentials ix (teCert, teVrf, teKes) =
       let loc ty = fp <> "." <> show ix <> ty
       in ShelleyCredentials (teCert, loc "cert")
                             (teVrf,  loc "vrf")
                             (teKes,  loc "kes")

mkPraosLeaderCredentials ::
     OperationalCertificate
  -> SigningKey VrfKey
  -> SigningKey KesKey
  -> TPraosLeaderCredentials StandardCrypto
mkPraosLeaderCredentials
    (OperationalCertificate opcert (StakePoolVerificationKey vkey))
    (VrfSigningKey vrfKey)
    (KesSigningKey kesKey) =
    TPraosLeaderCredentials
    { tpraosLeaderCredentialsCanBeLeader =
        TPraosCanBeLeader {
        tpraosCanBeLeaderOpCert     = opcert,
          tpraosCanBeLeaderColdVerKey = coerceKeyRole vkey,
          tpraosCanBeLeaderSignKeyVRF = vrfKey
        },
      tpraosLeaderCredentialsInitSignKey = kesKey,
      tpraosLeaderCredentialsLabel = "Shelley"
    }

parseEnvelope ::
     HasTextEnvelope a
  => AsType a
  -> (TextEnvelope, String)
  -> ExceptT PraosLeaderCredentialsError IO a
parseEnvelope as (te, loc) =
  firstExceptT (FileError . Api.FileError loc) . hoistEither $
    deserialiseFromTextEnvelope as te


------------------------------------------------------------------------------
-- Errors
--

data ShelleyProtocolInstantiationError =
       GenesisReadError GenesisReadError
     | GenesisValidationError GenesisValidationError
     | PraosLeaderCredentialsError PraosLeaderCredentialsError
  deriving Show

instance Error ShelleyProtocolInstantiationError where
  displayError (GenesisReadError err) = displayError err
  displayError (GenesisValidationError err) = displayError err
  displayError (PraosLeaderCredentialsError err) = displayError err


data GenesisReadError =
       GenesisReadFileError !FilePath !IOException
     | GenesisHashMismatch !GenesisHash !GenesisHash -- actual, expected
     | GenesisDecodeError !FilePath !String
  deriving Show

instance Error GenesisReadError where
  displayError (GenesisReadFileError fp err) =
        "There was an error reading the genesis file: "
     <> toS fp <> " Error: " <> show err

  displayError (GenesisHashMismatch actual expected) =
        "Wrong Shelley genesis file: the actual hash is " <> show actual
     <> ", but the expected Shelley genesis hash given in the node "
     <> "configuration file is " <> show expected

  displayError (GenesisDecodeError fp err) =
        "There was an error parsing the genesis file: "
     <> toS fp <> " Error: " <> show err


newtype GenesisValidationError = GenesisValidationErrors [Shelley.ValidationErr]
  deriving Show

instance Error GenesisValidationError where
  displayError (GenesisValidationErrors vErrs) =
    T.unpack (unlines (map Shelley.describeValidationErr vErrs))


data PraosLeaderCredentialsError =
       CredentialsReadError !FilePath !IOException
     | EnvelopeParseError !FilePath !String
     | FileError !(Api.FileError TextEnvelopeError)
--TODO: pick a less generic constructor than FileError

     | OCertNotSpecified
     | VRFKeyNotSpecified
     | KESKeyNotSpecified
  deriving Show

instance Error PraosLeaderCredentialsError where
  displayError (CredentialsReadError fp err) =
        "There was an error reading a credentials file: "
     <> toS fp <> " Error: " <> show err

  displayError (EnvelopeParseError fp err) =
        "There was an error parsing a credentials envelope: "
     <> toS fp <> " Error: " <> show err

  displayError (FileError fileErr) = displayError fileErr

  displayError OCertNotSpecified  = missingFlagMessage "shelley-operational-certificate"
  displayError VRFKeyNotSpecified = missingFlagMessage "shelley-vrf-key"
  displayError KESKeyNotSpecified = missingFlagMessage "shelley-kes-key"

missingFlagMessage :: String -> String
missingFlagMessage flag =
  "To create blocks, the --" <> flag <> " must also be specified"
