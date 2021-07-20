-- | This module provides a library interface for interacting with Cardano as
-- a user of the system.
--
-- It is intended to be the complete API covering everything but without exposing
-- constructors that reveal any lower level types.
--
-- In the interest of simplicity it glosses over some details of the system.
-- Most simple tools should be able to work just using this interface,
-- however you can go deeper and expose the types from the underlying libraries
-- using "Cardano.Api.Byron" or "Cardano.Api.Shelley".
--

module Cardano.Api (
    -- * Eras
    ByronEra,
    ShelleyEra,
    AllegraEra,
    MaryEra,
    AlonzoEra,
    CardanoEra(..),
    IsCardanoEra(..),
    AnyCardanoEra(..),
    anyCardanoEra,
    InAnyCardanoEra(..),

    -- ** Shelley-based eras
    ShelleyBasedEra(..),
    IsShelleyBasedEra(..),
    InAnyShelleyBasedEra(..),
    CardanoEraStyle(..),
    cardanoEraStyle,

    -- ** Deprecated
    Byron,
    Shelley,
    Allegra,
    Mary,
    -- * Type tags
    HasTypeProxy(..),
    AsType(..),
    -- * Cryptographic key interface
    -- $keys
    Key,
    VerificationKey,
    SigningKey(..),
    getVerificationKey,
    verificationKeyHash,
    castVerificationKey,
    castSigningKey,

    -- ** Generating keys
    generateSigningKey,
    deterministicSigningKey,
    deterministicSigningKeySeedSize,

    -- ** Hashes
    -- | In Cardano most keys are identified by their hash, and hashes are
    -- used in many other places.
    Hash,
    castHash,

    -- * Payment addresses
    -- | Constructing and inspecting normal payment addresses
    Address,
    ByronAddr,
    ShelleyAddr,
    NetworkId(..),
    -- ** Byron addresses
    makeByronAddress,
    ByronKey,
    ByronKeyLegacy,

    -- ** Shelley addresses
    makeShelleyAddress,
    PaymentCredential(..),
    StakeAddressPointer(..),
    StakeAddressReference(..),
    PaymentKey,
    PaymentExtendedKey,

    -- ** Addresses in any era
    AddressAny(..),

    -- ** Addresses in specific eras
    AddressInEra(..),
    AddressTypeInEra(..),
    byronAddressInEra,
    shelleyAddressInEra,
    anyAddressInShelleyBasedEra,
    anyAddressInEra,
    toAddressAny,
    makeByronAddressInEra,
    makeShelleyAddressInEra,

    -- * Stake addresses
    -- | Constructing and inspecting stake addresses
    StakeAddress,
    StakeCredential,
    makeStakeAddress,
    StakeKey,
    StakeExtendedKey,

    -- * Currency values
    -- ** Ada \/ Lovelace
    Lovelace,

    -- ** Multi-asset values
    Quantity(..),
    PolicyId(..),
    scriptPolicyId,
    AssetName(..),
    AssetId(..),
    Value,
    selectAsset,
    valueFromList,
    valueToList,
    filterValue,
    negateValue,
    ValueNestedRep(..),
    ValueNestedBundle(..),
    valueToNestedRep,
    valueFromNestedRep,

    -- ** Ada \/ Lovelace within multi-asset values
    quantityToLovelace,
    lovelaceToQuantity,
    selectLovelace,
    lovelaceToValue,
    valueToLovelace,

    -- * Blocks

    -- ** Blocks in the context of an era
    Block(Block),
    BlockHeader(..),

    -- ** Points on the chain
    ChainPoint(..),
    EpochNo(..),

    -- ** Tip of the chain
    ChainTip(..),
    BlockNo(..),
    chainTipToChainPoint,

    -- * Building transactions

    -- * Building transactions
    -- | Constructing and inspecting transactions

    -- ** Transaction bodies
    TxBody(TxBody),
    makeTransactionBody,
    TxBodyContent(..),
    TxBodyError(..),

    -- ** Transaction Ids
    TxId(..),
    getTxId,

    -- ** Transaction inputs
    TxIn(TxIn),
    TxIx(TxIx),

    -- ** Transaction outputs
    TxOut(TxOut),
    TxOutValue(..),
    serialiseAddressForTxOut,
    TxOutDatumHash(..),

    -- ** Other transaction body types
    TxInsCollateral(..),
    TxFee(..),
    TxValidityLowerBound(..),
    TxValidityUpperBound(..),
    SlotNo(..),
    EpochSlots(..),
    TxMetadataInEra(..),
    TxAuxScripts(..),
    TxExtraScriptData(..),
    TxExtraKeyWitnesses(..),
    TxWithdrawals(..),
    TxCertificates(..),
    TxUpdateProposal(..),
    TxMintValue(..),

    -- ** Building vs viewing transactions
    BuildTxWith(..),
    BuildTx,
    ViewTx,

    -- ** Era-dependent transaction body features
    CollateralSupportedInEra(..),
    MultiAssetSupportedInEra(..),
    OnlyAdaSupportedInEra(..),
    TxFeesExplicitInEra(..),
    TxFeesImplicitInEra(..),
    ValidityUpperBoundSupportedInEra(..),
    ValidityNoUpperBoundSupportedInEra(..),
    ValidityLowerBoundSupportedInEra(..),
    TxMetadataSupportedInEra(..),
    AuxScriptsSupportedInEra(..),
    TxExtraKeyWitnessesSupportedInEra(..),
    ScriptDataSupportedInEra(..),
    WithdrawalsSupportedInEra(..),
    CertificatesSupportedInEra(..),
    UpdateProposalSupportedInEra(..),

    -- ** Feature availability functions
    collateralSupportedInEra,
    multiAssetSupportedInEra,
    txFeesExplicitInEra,
    validityUpperBoundSupportedInEra,
    validityNoUpperBoundSupportedInEra,
    validityLowerBoundSupportedInEra,
    txMetadataSupportedInEra,
    auxScriptsSupportedInEra,
    extraKeyWitnessesSupportedInEra,
    withdrawalsSupportedInEra,
    certificatesSupportedInEra,
    updateProposalSupportedInEra,
    scriptDataSupportedInEra,

    -- ** Fee calculation
    transactionFee,
    estimateTransactionFee,
    evaluateTransactionFee,
    estimateTransactionKeyWitnessCount,

    -- ** Script execution units
    evaluateTransactionExecutionUnits,
    ScriptExecutionError(..),
    TransactionValidityIntervalError,

    -- ** Transaction balance
    evaluateTransactionBalance,

    -- ** Building transactions with automated fees and balancing
    makeTransactionBodyAutoBalance,
    TxBodyErrorAutoBalance(..),

    -- * Signing transactions
    -- | Creating transaction witnesses one by one, or all in one go.
    Tx(Tx),
    getTxBody,
    getTxWitnesses,

    -- ** Signing in one go
    signByronTransaction,
    signShelleyTransaction,

    -- ** Incremental signing and separate witnesses
    makeSignedTransaction,
    KeyWitness,
    makeByronKeyWitness,
    ShelleyWitnessSigningKey(..),
    makeShelleyKeyWitness,
    makeShelleyBootstrapWitness,

    -- * Transaction metadata
    -- | Embedding additional structured data within transactions.
    TxMetadata(..),

    -- ** Constructing metadata
    TxMetadataValue(..),
    makeTransactionMetadata,

    -- ** Validating metadata
    validateTxMetadata,
    TxMetadataRangeError (..),

    -- ** Converstion to\/from JSON
    TxMetadataJsonSchema (..),
    metadataFromJson,
    metadataToJson,
    metadataValueToJsonNoSchema,
    TxMetadataJsonError (..),
    TxMetadataJsonSchemaError (..),

    -- * Certificates
    Certificate,

    -- ** Registering stake address and delegating
    -- | Certificates that are embedded in transactions for registering and
    -- unregistering stake address, and for setting the stake pool delegation
    -- choice for a stake address.
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressDelegationCertificate,

    -- ** Registering stake pools
    -- | Certificates that are embedded in transactions for registering and
    -- retiring stake pools. This includes updating the stake pool parameters.
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters,
    StakePoolRelay,
    StakePoolMetadataReference,

    -- * Stake pool off-chain metadata
    StakePoolMetadata,
    validateAndHashStakePoolMetadata,
    StakePoolMetadataValidationError,

    -- * Scripts
    -- | Both 'PaymentCredential's and 'StakeCredential's can use scripts.

    -- ** Script languages
    SimpleScriptV1,
    SimpleScriptV2,
    PlutusScriptV1,
    ScriptLanguage(..),
    SimpleScriptVersion(..),
    PlutusScriptVersion(..),
    AnyScriptLanguage(..),
    AnyPlutusScriptVersion(..),
    IsScriptLanguage(..),
    IsSimpleScriptLanguage(..),

    -- ** Scripts in a specific language
    Script(..),

    -- ** Scripts in any language
    ScriptInAnyLang(..),
    toScriptInAnyLang,

    -- ** Scripts in a specific era
    ScriptInEra(..),
    toScriptInEra,
    eraOfScriptInEra,

    -- * Use of a script in an era as a witness
    WitCtxTxIn, WitCtxMint, WitCtxStake,
    WitCtx(..),
    ScriptWitness(..),
    Witness(..),
    KeyWitnessInCtx(..),
    ScriptWitnessInCtx(..),
    ScriptDatum(..),
    ScriptRedeemer,
    scriptWitnessScript,

    -- *** Languages supported in each era
    ScriptLanguageInEra(..),
    scriptLanguageSupportedInEra,
    languageOfScriptLanguageInEra,
    eraOfScriptLanguageInEra,

    -- ** Simple scripts
    -- | Making multi-signature and time-lock scripts.
    SimpleScript(..),
    TimeLocksSupported(..),
    timeLocksSupported,
    adjustSimpleScriptVersion,

    -- ** Plutus scripts
    PlutusScript,
    examplePlutusScriptAlwaysSucceeds,
    examplePlutusScriptAlwaysFails,

    -- * Script data
    ScriptData(..),
    hashScriptData,

    -- ** Validation
    ScriptDataRangeError (..),
    validateScriptData,

    -- ** Conversion to\/from JSON
    ScriptDataJsonSchema (..),
    scriptDataFromJson,
    scriptDataToJson,
    ScriptDataJsonError (..),
    ScriptDataJsonSchemaError (..),

    -- * Script execution units
    ExecutionUnits(..),
    ExecutionUnitPrices(..),
    CostModel(..),
    validateCostModel,

    -- ** Script addresses
    -- | Making addresses from scripts.
    ScriptHash,
    hashScript,

    -- * Serialisation
    -- | Support for serialising data in JSON, CBOR and text files.

    -- ** CBOR
    SerialiseAsCBOR,
    ToCBOR,
    FromCBOR,
    serialiseToCBOR,
    deserialiseFromCBOR,

    -- ** JSON
    ToJSON,
    FromJSON,
    serialiseToJSON,
    deserialiseFromJSON,
    JsonDecodeError(..),
    readFileJSON,
    writeFileJSON,
    prettyPrintJSON,

    -- ** Bech32
    SerialiseAsBech32,
    serialiseToBech32,
    deserialiseFromBech32,
    deserialiseAnyOfFromBech32,
    Bech32DecodeError(..),

    -- ** Addresses
    -- | Address serialisation is (sadly) special
    SerialiseAddress,
    serialiseAddress,
    deserialiseAddress,

    -- ** Raw binary
    -- | Some types have a natural raw binary format.
    SerialiseAsRawBytes,
    serialiseToRawBytes,
    deserialiseFromRawBytes,
    serialiseToRawBytesHex,
    deserialiseFromRawBytesHex,
    serialiseToRawBytesHexText,

    -- ** Text envelope
    -- | Support for a envelope file format with text headers and a hex-encoded
    -- binary payload.
    HasTextEnvelope(..),
    TextEnvelope(..),
    TextEnvelopeType(..),
    TextEnvelopeDescr,
    TextEnvelopeError(..),
    textEnvelopeRawCBOR,
    serialiseToTextEnvelope,
    deserialiseFromTextEnvelope,
    readFileTextEnvelope,
    writeFileTextEnvelope,
    writeFileTextEnvelopeWithOwnerPermissions,
    readTextEnvelopeFromFile,
    readTextEnvelopeOfTypeFromFile,
    -- *** Reading one of several key types
    FromSomeType(..),
    deserialiseFromTextEnvelopeAnyOf,
    readFileTextEnvelopeAnyOf,

    -- * Errors
    Error(..),
    throwErrorAsException,
    FileError(..),

    -- * Node interaction
    -- | Operations that involve talking to a local Cardano node.

    -- ** Queries
    -- ** Submitting transactions

    -- ** High level protocol interaction with a Cardano node
    -- *** Initialization / Accumulation
    Env(..),
    envSecurityParam,
    LedgerState(..),
    initialLedgerState,
    applyBlock,

    -- *** Traversing the block chain
    foldBlocks,

    -- *** Errors
    FoldBlocksError(..),
    GenesisConfigError(..),
    InitialLedgerStateError(..),
    renderFoldBlocksError,
    renderGenesisConfigError,
    renderInitialLedgerStateError,

    -- ** Low level protocol interaction with a Cardano node
    connectToLocalNode,
    connectToLocalNodeWithVersion,
    LocalNodeConnectInfo(..),
    AnyConsensusMode(..),
    renderMode,
    ConsensusMode(CardanoMode),
    consensusModeOnly,
    ConsensusModeIsMultiEra(..),
    AnyConsensusModeParams(..),
    ConsensusModeParams(..),
    EraInMode(..),
    toEraInMode,
    LocalNodeClientProtocols(..),
    LocalChainSyncClient(..),
    CardanoMode,
--  connectToRemoteNode,

    -- *** Chain sync protocol
    -- | To construct a @ChainSyncClient@ see @Cardano.Api.Client@ or
    -- @Cardano.Api.ClientPipelined@.
    ChainSyncClient(..),
    ChainSyncClientPipelined(..),
    BlockInMode(..),
    LocalNodeClientProtocolsInMode,

    -- *** Local tx submission
    LocalTxSubmissionClient,
    TxInMode(..),
    TxValidationErrorInMode(..),
    runLocalTxSubmissionClient,
    submitTxToNodeLocal,

    -- *** Local state query
    LocalStateQueryClient(..),
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),
    QueryUTxOFilter(..),
    UTxO(..),
    queryNodeLocalState,

    EraHistory(..),
    getProgress,

    -- *** Common queries
    getLocalChainTip,

    -- * Node operation
    -- | Support for the steps needed to operate a node

    -- ** Operational certificates
    OperationalCertificate,
    OperationalCertificateIssueCounter,
    OperationalCertIssueError,
    issueOperationalCertificate,

    -- * Genesis file
    -- | Types and functions needed to inspect or create a genesis file.
    GenesisKey,
    GenesisExtendedKey,
    GenesisDelegateKey,
    GenesisDelegateExtendedKey,
    GenesisUTxOKey,
    genesisUTxOPseudoTxIn,

    -- ** Genesis paramaters
    GenesisParameters(..),

    -- * Special transactions
    -- | There are various additional things that can be embedded in a
    -- transaction for special operations.
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,
    MIRTarget (..),

    -- * Protocol parameter updates
    UpdateProposal(..),
    ProtocolParametersUpdate(..),
    makeShelleyUpdateProposal,
    PraosNonce,
    makePraosNonce,

    NetworkMagic(..),

    -- ** Conversions
    --TODO: arrange not to export these
    toNetworkMagic,
    --TODO: Remove after updating cardano-node-chairman with new IPC
    SomeNodeClientProtocol(..),

    SlotsInEpoch(..),
    SlotsToEpochEnd(..),
    slotToEpoch,

    NodeToClientVersion(..),

    -- ** Monadic queries
    LocalStateQueryScript,
    sendMsgQuery,
    setupLocalStateQueryScript
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.Error
import           Cardano.Api.Fees
import           Cardano.Api.GenesisParameters
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Hash
import           Cardano.Api.IPC
import           Cardano.Api.IPC.Monad
import           Cardano.Api.Key
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysPraos
import           Cardano.Api.KeysShelley
import           Cardano.Api.LedgerState
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.OperationalCertificate
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.Query hiding (LedgerState (..))
import           Cardano.Api.Script
import           Cardano.Api.ScriptData
import           Cardano.Api.SerialiseBech32
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON
import           Cardano.Api.SerialiseRaw
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Tx
import           Cardano.Api.TxBody
import           Cardano.Api.TxMetadata
import           Cardano.Api.Value
--TODO: Remove after updating cardano-node-chairman with new IPC
import           Cardano.Api.Protocol.Types
