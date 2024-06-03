{-# LANGUAGE FlexibleContexts #-}

module Polysemy.Blockfrost.Api
  ( Blockfrost,

    AccountDelegation(..),
    AccountHistory(..),
    AccountInfo(..),
    AccountMir(..),
    AccountRegistration(..),
    AccountReward(..),
    AccountWithdrawal(..),
    Address(..),
    AddressAssociated(..),
    AddressAssociatedTotal(..),
    AddressDetails(..),
    AddressInfo(..),
    AddressInfoExtended(..),
    AddressTransaction(..),
    AddressUtxo(..),
    Amount(..),
    AssetAddress(..),
    AssetDetails(..),
    AssetHistory(..),
    AssetId(..),
    AssetInfo(..),
    AssetTransaction(..),
    Block(..),
    BlockHash(..),
    BlockfrostError(..),
    CBORString(..),
    DatumHash(..),
    Epoch(..),
    EpochInfo(..),
    Genesis(..),
    Healthy(..),
    IPFSData(..),
    IPFSPin(..),
    IPFSPinChange(..),
    Metric(..), Network,
    NetworkEraSummary(..),
    NutlinkAddress(..),
    NutlinkAddressTicker(..),
    NutlinkTicker(..),
    Paged(..), PolicyId,
    Pool(..),
    PoolDelegator(..),
    PoolEpoch(..),
    PoolHistory(..),
    PoolId(..),
    PoolInfo(..),
    PoolMetadata(..),
    PoolRelay(..),
    PoolStakeDistribution(..),
    PoolUpdate(..),
    Project(..),
    ProtocolParams(..),
    Script(..),
    ScriptCBOR(..),
    ScriptDatum(..),
    ScriptDatumCBOR(..),
    ScriptHash(..),
    ScriptHashList(..),
    ScriptJSON(..),
    ScriptRedeemer(..),
    ServerTime(..),
    Slot(..), SortOrder,
    StakeDistribution(..),
    Transaction(..),
    TransactionDelegation(..),
    TransactionMetaCBOR(..),
    TransactionMetaJSON(..),
    TransactionMir(..),
    TransactionPoolRetiring(..),
    TransactionPoolUpdate(..),
    TransactionRedeemer(..),
    TransactionStake(..),
    TransactionUtxos(..),
    TransactionWithdrawal(..),
    TxHash(..), TxMeta,
    TxMetaCBOR(..),
    TxMetaJSON(..),
    URLVersion(..),

    runBlockfrost,

    -- Client
    getRoot                             ,
    getHealth                           ,
    getClock                            ,
    getMetrics                          ,
    getMetricsEndpoints                 ,

    -- Client.NutLink
    nutlinkListAddress                  ,
    nutlinkListAddressTickers'          ,
    nutlinkListAddressTickers           ,
    nutlinkAddressTickers'              ,
    nutlinkAddressTickers               ,
    nutlinkTickers'                     ,
    nutlinkTickers                      ,

    -- Client.IPFS
    ipfsGateway                         ,
    ipfsPin                             ,
    ipfsListPins'                       ,
    ipfsListPins                        ,
    ipfsGetPin                          ,
    ipfsRemovePin                       ,

    -- Client.Cardano.Blocks
    getLatestBlock                      ,
    getLatestBlockTxs'                  ,
    getLatestBlockTxs                   ,
    getBlock                            ,
    getBlockSlot                        ,
    getBlockEpochSlot                   ,
    getNextBlocks'                      ,
    getNextBlocks                       ,
    getPreviousBlocks'                  ,
    getPreviousBlocks                   ,
    getBlockTxs'                        ,
    getBlockTxs                         ,
    getBlockAffectedAddresses'          ,
    getBlockAffectedAddresses           ,

    -- Client.Cardano.Network
    getNetworkInfo                      ,
    getNetworkEras                      ,

    -- Client.Cardano.Addresses
    getAddressInfo                      ,
    getAddressInfoExtended              ,
    getAddressDetails                   ,
    getAddressUtxos'                    ,
    getAddressUtxos                     ,
    getAddressUtxosAsset'               ,
    getAddressUtxosAsset                ,
    getAddressTransactions'             ,
    getAddressTransactions              ,

    -- Client.Cardano.Assets
    getAssets'                          ,
    getAssets                           ,
    getAssetDetails                     ,
    getAssetHistory'                    ,
    getAssetHistory                     ,
    getAssetTransactions'               ,
    getAssetTransactions                ,
    getAssetAddresses'                  ,
    getAssetAddresses                   ,
    getAssetsByPolicy'                  ,
    getAssetsByPolicy                   ,

    -- Client.Cardano.Scripts
    listScripts'                        ,
    listScripts                         ,
    getScript                           ,
    getScriptRedeemers'                 ,
    getScriptRedeemers                  ,
    getScriptDatum                      ,
    getScriptDatumCBOR                  ,
    getScriptJSON                       ,
    getScriptCBOR                       ,

    -- Client.Cardano.Epochs
    getLatestEpoch                      ,
    getLatestEpochProtocolParams        ,
    getEpoch                            ,
    getNextEpochs'                      ,
    getNextEpochs                       ,
    getPreviousEpochs'                  ,
    getPreviousEpochs                   ,
    getEpochStake'                      ,
    getEpochStake                       ,
    getEpochStakeByPool'                ,
    getEpochStakeByPool                 ,
    getEpochBlocks'                     ,
    getEpochBlocks                      ,
    getEpochBlocksByPool'               ,
    getEpochBlocksByPool                ,
    getEpochProtocolParams              ,

    -- Client.Cardano.Transactions
    getTx                               ,
    getTxUtxos                          ,
    getTxRedeemers                      ,
    getTxStakes                         ,
    getTxDelegations                    ,
    getTxWithdrawals                    ,
    getTxMirs                           ,
    getTxPoolUpdates                    ,
    getTxPoolRetiring                   ,
    getTxMetadataJSON                   ,
    getTxMetadataCBOR                   ,
    submitTx                            ,

    -- Client.Cardano.Ledger
    getLedgerGenesis                    ,

    -- Client.Cardano.Accounts
    getAccount                          ,
    getAccountRewards'                  ,
    getAccountRewards                   ,
    getAccountHistory'                  ,
    getAccountHistory                   ,
    getAccountDelegations'              ,
    getAccountDelegations               ,
    getAccountRegistrations'            ,
    getAccountRegistrations             ,
    getAccountWithdrawals'              ,
    getAccountWithdrawals               ,
    getAccountMirs'                     ,
    getAccountMirs                      ,
    getAccountAssociatedAddresses'      ,
    getAccountAssociatedAddresses       ,
    getAccountAssociatedAddressesTotal  ,
    getAccountAssociatedAssets'         ,
    getAccountAssociatedAssets          ,

    -- Client.Cardano.Pools
    listPools'                          ,
    listPools                           ,
    listPoolsExtended'                  ,
    listPoolsExtended                   ,
    listRetiredPools'                   ,
    listRetiredPools                    ,
    listRetiringPools'                  ,
    listRetiringPools                   ,
    getPool                             ,
    getPoolHistory'                     ,
    getPoolHistory                      ,
    getPoolMetadata                     ,
    getPoolRelays                       ,
    getPoolDelegators'                  ,
    getPoolDelegators                   ,
    getPoolBlocks'                      ,
    getPoolBlocks                       ,
    getPoolUpdates'                     ,
    getPoolUpdates                      ,

    -- Client.Cardano.Metadata
    getTxMetadataLabels'                ,
    getTxMetadataLabels                 ,
    getTxMetadataByLabelJSON'           ,
    getTxMetadataByLabelJSON            ,
    getTxMetadataByLabelCBOR'           ,
    getTxMetadataByLabelCBOR            ,

  ) where

import           Control.Monad
import           Data.Either
import           Data.Maybe
import           Data.Text
import           Prelude                               (Integer)

import           Blockfrost.Client                     (AccountDelegation (..),
                                                        AccountHistory (..),
                                                        AccountInfo (..),
                                                        AccountMir (..),
                                                        AccountRegistration (..),
                                                        AccountReward (..),
                                                        AccountWithdrawal (..),
                                                        Address (..),
                                                        AddressAssociated (..),
                                                        AddressAssociatedTotal (..),
                                                        AddressDetails (..),
                                                        AddressInfo (..),
                                                        AddressInfoExtended (..),
                                                        AddressTransaction (..),
                                                        AddressUtxo (..),
                                                        Amount (..),
                                                        AssetAddress (..),
                                                        AssetDetails (..),
                                                        AssetHistory (..),
                                                        AssetId (..),
                                                        AssetInfo (..),
                                                        AssetTransaction (..),
                                                        Block (..),
                                                        BlockHash (..),
                                                        BlockIndex (..),
                                                        BlockfrostError (..),
                                                        CBORString (..),
                                                        DatumHash (..),
                                                        Epoch (..),
                                                        EpochInfo (..),
                                                        Genesis (..),
                                                        Healthy (..),
                                                        IPFSData (..),
                                                        IPFSPin (..),
                                                        IPFSPinChange (..),
                                                        Metric (..),
                                                        Network (..),
                                                        NetworkEraSummary (..),
                                                        NutlinkAddress (..),
                                                        NutlinkAddressTicker (..),
                                                        NutlinkTicker (..),
                                                        Paged (..),
                                                        PolicyId (..),
                                                        Pool (..),
                                                        PoolDelegator (..),
                                                        PoolEpoch (..),
                                                        PoolHistory (..),
                                                        PoolId (..),
                                                        PoolInfo (..),
                                                        PoolMetadata (..),
                                                        PoolRelay (..),
                                                        PoolStakeDistribution (..),
                                                        PoolUpdate (..),
                                                        Project (..),
                                                        ProtocolParams (..),
                                                        Script (..),
                                                        ScriptCBOR (..),
                                                        ScriptDatum (..),
                                                        ScriptDatumCBOR (..),
                                                        ScriptHash (..),
                                                        ScriptHashList (..),
                                                        ScriptJSON (..),
                                                        ScriptRedeemer (..),
                                                        ServerTime (..),
                                                        Slot (..),
                                                        SortOrder (..),
                                                        StakeDistribution (..),
                                                        Transaction (..),
                                                        TransactionDelegation (..),
                                                        TransactionMetaCBOR (..),
                                                        TransactionMetaJSON (..),
                                                        TransactionMir (..),
                                                        TransactionPoolRetiring (..),
                                                        TransactionPoolUpdate (..),
                                                        TransactionRedeemer (..),
                                                        TransactionStake (..),
                                                        TransactionUtxos (..),
                                                        TransactionWithdrawal (..),
                                                        TxHash (..),
                                                        TxMeta (..),
                                                        TxMetaCBOR (..),
                                                        TxMetaJSON (..),
                                                        URLVersion (..))
import           Polysemy
import           Polysemy.Blockfrost.Effect.Blockfrost (Blockfrost,
                                                        runBlockfrost)
import qualified Polysemy.Blockfrost.Effect.Blockfrost as BF
import           Polysemy.Error


-- Client
getRoot                             :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r URLVersion
getHealth                           :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r Healthy
getClock                            :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r ServerTime
getMetrics                          :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [Metric]
getMetricsEndpoints                 :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [(Text, Metric)]

-- Client.NutLink
nutlinkListAddress                  :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r NutlinkAddress
nutlinkListAddressTickers'          :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [NutlinkAddressTicker]
nutlinkListAddressTickers           :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [NutlinkAddressTicker]
nutlinkAddressTickers'              :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Text -> Paged -> SortOrder -> Sem r [NutlinkTicker]
nutlinkAddressTickers               :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Text -> Sem r [NutlinkTicker]
nutlinkTickers'                     :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Paged -> SortOrder -> Sem r [(Address, NutlinkTicker)]
nutlinkTickers                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Sem r [(Address, NutlinkTicker)]

-- Client.IPFS
ipfsGateway                         :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Sem r IPFSData
ipfsPin                             :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Sem r IPFSPinChange
ipfsListPins'                       :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r [IPFSPin]
ipfsListPins                        :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [IPFSPin]
ipfsGetPin                          :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Sem r IPFSPin
ipfsRemovePin                       :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Sem r IPFSPinChange

-- Client.Cardano.Blocks
getLatestBlock                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r Block
getLatestBlockTxs'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r [TxHash]
getLatestBlockTxs                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [TxHash]
getBlock                            :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Sem r Block
getBlockSlot                        :: Member Blockfrost r => Member (Error BlockfrostError) r => Slot -> Sem r Block
getBlockEpochSlot                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Slot -> Sem r Block
getNextBlocks'                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Paged -> Sem r [Block]
getNextBlocks                       :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Sem r [Block]
getPreviousBlocks'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Paged -> Sem r [Block]
getPreviousBlocks                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Sem r [Block]
getBlockTxs'                        :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Paged -> SortOrder -> Sem r [TxHash]
getBlockTxs                         :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Sem r [TxHash]
getBlockAffectedAddresses'          :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Paged -> Sem r [(Address, [TxHash])]
getBlockAffectedAddresses           :: Member Blockfrost r => Member (Error BlockfrostError) r => Either Integer BlockHash -> Sem r [(Address, [TxHash])]

-- Client.Cardano.Network
getNetworkInfo                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r Network
getNetworkEras                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [NetworkEraSummary]

-- Client.Cardano.Addresses
getAddressInfo                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r AddressInfo
getAddressInfoExtended              :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r AddressInfoExtended
getAddressDetails                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r AddressDetails
getAddressUtxos'                    :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [AddressUtxo]
getAddressUtxos                     :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AddressUtxo]
getAddressUtxosAsset'               :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> AssetId-> Paged -> SortOrder -> Sem r [AddressUtxo]
getAddressUtxosAsset                :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> AssetId -> Sem r [AddressUtxo]
getAddressTransactions'             :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Maybe BlockIndex -> Maybe BlockIndex -> Sem r [AddressTransaction]
getAddressTransactions              :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AddressTransaction]

-- Client.Cardano.Assets
getAssets'                          :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r [AssetInfo]
getAssets                           :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [AssetInfo]
getAssetDetails                     :: Member Blockfrost r => Member (Error BlockfrostError) r => AssetId -> Sem r AssetDetails
getAssetHistory'                    :: Member Blockfrost r => Member (Error BlockfrostError) r => AssetId -> Paged -> SortOrder -> Sem r [AssetHistory]
getAssetHistory                     :: Member Blockfrost r => Member (Error BlockfrostError) r => AssetId -> Sem r [AssetHistory]
getAssetTransactions'               :: Member Blockfrost r => Member (Error BlockfrostError) r => AssetId -> Paged -> SortOrder -> Sem r [AssetTransaction]
getAssetTransactions                :: Member Blockfrost r => Member (Error BlockfrostError) r => AssetId -> Sem r [AssetTransaction]
getAssetAddresses'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => AssetId -> Paged -> SortOrder -> Sem r [AssetAddress]
getAssetAddresses                   :: Member Blockfrost r => Member (Error BlockfrostError) r => AssetId -> Sem r [AssetAddress]
getAssetsByPolicy'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => PolicyId -> Paged -> SortOrder -> Sem r [AssetInfo]
getAssetsByPolicy                   :: Member Blockfrost r => Member (Error BlockfrostError) r => PolicyId -> Sem r [AssetInfo]

-- Client.Cardano.Scripts
listScripts'                        :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r ScriptHashList
listScripts                         :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r ScriptHashList
getScript                           :: Member Blockfrost r => Member (Error BlockfrostError) r => ScriptHash -> Sem r Script
getScriptRedeemers'                 :: Member Blockfrost r => Member (Error BlockfrostError) r => ScriptHash -> Paged -> SortOrder -> Sem r [ScriptRedeemer]
getScriptRedeemers                  :: Member Blockfrost r => Member (Error BlockfrostError) r => ScriptHash -> Sem r [ScriptRedeemer]
getScriptDatum                      :: Member Blockfrost r => Member (Error BlockfrostError) r => DatumHash -> Sem r ScriptDatum
getScriptDatumCBOR                  :: Member Blockfrost r => Member (Error BlockfrostError) r => DatumHash -> Sem r ScriptDatumCBOR
getScriptJSON                       :: Member Blockfrost r => Member (Error BlockfrostError) r => ScriptHash -> Sem r ScriptJSON
getScriptCBOR                       :: Member Blockfrost r => Member (Error BlockfrostError) r => ScriptHash -> Sem r ScriptCBOR

-- Client.Cardano.Epochs
getLatestEpoch                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r EpochInfo
getLatestEpochProtocolParams        :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r ProtocolParams
getEpoch                            :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Sem r EpochInfo
getNextEpochs'                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Paged -> Sem r [EpochInfo]
getNextEpochs                       :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Sem r [EpochInfo]
getPreviousEpochs'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Paged -> Sem r [EpochInfo]
getPreviousEpochs                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Sem r [EpochInfo]
getEpochStake'                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Paged -> Sem r [StakeDistribution]
getEpochStake                       :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Sem r [StakeDistribution]
getEpochStakeByPool'                :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> PoolId -> Paged -> Sem r [PoolStakeDistribution]
getEpochStakeByPool                 :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> PoolId -> Sem r [PoolStakeDistribution]
getEpochBlocks'                     :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Paged -> SortOrder -> Sem r [BlockHash]
getEpochBlocks                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Sem r [BlockHash]
getEpochBlocksByPool'               :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> PoolId -> Paged -> SortOrder -> Sem r [BlockHash]
getEpochBlocksByPool                :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> PoolId -> Sem r [BlockHash]
getEpochProtocolParams              :: Member Blockfrost r => Member (Error BlockfrostError) r => Epoch -> Sem r ProtocolParams

-- Client.Cardano.Transactions
getTx                               :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r Transaction
getTxUtxos                          :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r TransactionUtxos
getTxRedeemers                      :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionRedeemer]
getTxStakes                         :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionStake]
getTxDelegations                    :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionDelegation]
getTxWithdrawals                    :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionWithdrawal]
getTxMirs                           :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionMir]
getTxPoolUpdates                    :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionPoolUpdate]
getTxPoolRetiring                   :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionPoolRetiring]
getTxMetadataJSON                   :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionMetaJSON]
getTxMetadataCBOR                   :: Member Blockfrost r => Member (Error BlockfrostError) r => TxHash -> Sem r [TransactionMetaCBOR]
submitTx                            :: Member Blockfrost r => Member (Error BlockfrostError) r => CBORString -> Sem r TxHash

-- Client.Cardano.Ledger
getLedgerGenesis                    :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r Genesis

-- Client.Cardano.Accounts
getAccount                          :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r AccountInfo
getAccountRewards'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [AccountReward]
getAccountRewards                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AccountReward]
getAccountHistory'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [AccountHistory]
getAccountHistory                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AccountHistory]
getAccountDelegations'              :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [AccountDelegation]
getAccountDelegations               :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AccountDelegation]
getAccountRegistrations'            :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [AccountRegistration]
getAccountRegistrations             :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AccountRegistration]
getAccountWithdrawals'              :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [AccountWithdrawal]
getAccountWithdrawals               :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AccountWithdrawal]
getAccountMirs'                     :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [AccountMir]
getAccountMirs                      :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AccountMir]
getAccountAssociatedAddresses'      :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [AddressAssociated]
getAccountAssociatedAddresses       :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [AddressAssociated]
getAccountAssociatedAddressesTotal  :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r AddressAssociatedTotal
getAccountAssociatedAssets'         :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Paged -> SortOrder -> Sem r [Amount]
getAccountAssociatedAssets          :: Member Blockfrost r => Member (Error BlockfrostError) r => Address -> Sem r [Amount]

-- Client.Cardano.Pools
listPools'                          :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r [PoolId]
listPools                           :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [PoolId]
listPoolsExtended'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r [Pool]
listPoolsExtended                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [Pool]
listRetiredPools'                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r [PoolEpoch]
listRetiredPools                    :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [PoolEpoch]
listRetiringPools'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r [PoolEpoch]
listRetiringPools                   :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [PoolEpoch]
getPool                             :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Sem r PoolInfo
getPoolHistory'                     :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Paged -> SortOrder -> Sem r [PoolHistory]
getPoolHistory                      :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Sem r [PoolHistory]
getPoolMetadata                     :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Sem r (Maybe PoolMetadata)
getPoolRelays                       :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Sem r [PoolRelay]
getPoolDelegators'                  :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Paged -> SortOrder -> Sem r [PoolDelegator]
getPoolDelegators                   :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Sem r [PoolDelegator]
getPoolBlocks'                      :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Paged -> SortOrder -> Sem r [BlockHash]
getPoolBlocks                       :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Sem r [BlockHash]
getPoolUpdates'                     :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Paged -> SortOrder -> Sem r [PoolUpdate]
getPoolUpdates                      :: Member Blockfrost r => Member (Error BlockfrostError) r => PoolId -> Sem r [PoolUpdate]

-- Client.Cardano.Metadata
getTxMetadataLabels'                :: Member Blockfrost r => Member (Error BlockfrostError) r => Paged -> SortOrder -> Sem r [TxMeta]
getTxMetadataLabels                 :: Member Blockfrost r => Member (Error BlockfrostError) r => Sem r [TxMeta]
getTxMetadataByLabelJSON'           :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Paged -> SortOrder -> Sem r [TxMetaJSON]
getTxMetadataByLabelJSON            :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Sem r [TxMetaJSON]
getTxMetadataByLabelCBOR'           :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Paged -> SortOrder -> Sem r [TxMetaCBOR]
getTxMetadataByLabelCBOR            :: Member Blockfrost r => Member (Error BlockfrostError) r => Text -> Sem r [TxMetaCBOR]


-- Client
getRoot                                     = fromEither =<< BF.getRoot
getHealth                                   = fromEither =<< BF.getHealth
getClock                                    = fromEither =<< BF.getClock
getMetrics                                  = fromEither =<< BF.getMetrics
getMetricsEndpoints                         = fromEither =<< BF.getMetricsEndpoints

-- Client.NutLink
nutlinkListAddress                 a        = fromEither =<< BF.nutlinkListAddress                 a
nutlinkListAddressTickers'         a b c    = fromEither =<< BF.nutlinkListAddressTickers'         a b c
nutlinkListAddressTickers          a        = fromEither =<< BF.nutlinkListAddressTickers          a
nutlinkAddressTickers'             a b c d  = fromEither =<< BF.nutlinkAddressTickers'             a b c d
nutlinkAddressTickers              a b      = fromEither =<< BF.nutlinkAddressTickers              a b
nutlinkTickers'                    a b c    = fromEither =<< BF.nutlinkTickers'                    a b c
nutlinkTickers                     a        = fromEither =<< BF.nutlinkTickers                     a

-- -- Client.IPFS
ipfsGateway                        a        = fromEither =<< BF.ipfsGateway                        a
ipfsPin                            a        = fromEither =<< BF.ipfsPin                            a
ipfsListPins'                      a b      = fromEither =<< BF.ipfsListPins'                      a b
ipfsListPins                                = fromEither =<< BF.ipfsListPins
ipfsGetPin                         a        = fromEither =<< BF.ipfsGetPin                         a
ipfsRemovePin                      a        = fromEither =<< BF.ipfsRemovePin                      a

-- Client.Cardano.Blocks
getLatestBlock                              = fromEither =<< BF.getLatestBlock
getLatestBlockTxs'                 a b      = fromEither =<< BF.getLatestBlockTxs'                 a b
getLatestBlockTxs                           = fromEither =<< BF.getLatestBlockTxs
getBlock a                                  = fromEither =<< BF.getBlock a
getBlockSlot                       a        = fromEither =<< BF.getBlockSlot                       a
getBlockEpochSlot                  a b      = fromEither =<< BF.getBlockEpochSlot                  a b
getNextBlocks'                     a b      = fromEither =<< BF.getNextBlocks'                     a b
getNextBlocks                      a        = fromEither =<< BF.getNextBlocks                      a
getPreviousBlocks'                 a b      = fromEither =<< BF.getPreviousBlocks'                 a b
getPreviousBlocks                  a        = fromEither =<< BF.getPreviousBlocks                  a
getBlockTxs'                       a b c    = fromEither =<< BF.getBlockTxs'                       a b c
getBlockTxs                        a        = fromEither =<< BF.getBlockTxs                        a
getBlockAffectedAddresses'         a b      = fromEither =<< BF.getBlockAffectedAddresses'         a b
getBlockAffectedAddresses          a        = fromEither =<< BF.getBlockAffectedAddresses          a

-- -- Client.Cardano.Network
getNetworkInfo                              = fromEither =<< BF.getNetworkInfo
getNetworkEras                              = fromEither =<< BF.getNetworkEras

-- Client.Cardano.Addresses
getAddressInfo                   a          = fromEither =<< BF.getAddressInfo                   a
getAddressInfoExtended           a          = fromEither =<< BF.getAddressInfoExtended           a
getAddressDetails                a          = fromEither =<< BF.getAddressDetails                a
getAddressUtxos'                 a b c      = fromEither =<< BF.getAddressUtxos'                 a b c
getAddressUtxos                  a          = fromEither =<< BF.getAddressUtxos                  a
getAddressUtxosAsset'            a b c d    = fromEither =<< BF.getAddressUtxosAsset'            a b c d
getAddressUtxosAsset             a b        = fromEither =<< BF.getAddressUtxosAsset             a b
getAddressTransactions           a          = fromEither =<< BF.getAddressTransactions           a
getAddressTransactions'          a b c d e  = fromEither =<< BF.getAddressTransactions'          a b c d e

-- Client.Cardano.Assets
getAssets'                         a b        = fromEither =<< BF.getAssets'                       a b
getAssets                                     = fromEither =<< BF.getAssets
getAssetDetails                    a          = fromEither =<< BF.getAssetDetails                  a
getAssetHistory'                   a b c      = fromEither =<< BF.getAssetHistory'                 a b c
getAssetHistory                    a          = fromEither =<< BF.getAssetHistory                  a
getAssetTransactions'              a b c      = fromEither =<< BF.getAssetTransactions'            a b c
getAssetTransactions               a          = fromEither =<< BF.getAssetTransactions             a
getAssetAddresses'                 a b c      = fromEither =<< BF.getAssetAddresses'               a b c
getAssetAddresses                  a          = fromEither =<< BF.getAssetAddresses                a
getAssetsByPolicy'                 a b c      = fromEither =<< BF.getAssetsByPolicy'               a b c
getAssetsByPolicy                  a          = fromEither =<< BF.getAssetsByPolicy                a

-- Client.Cardano.Scripts
listScripts'                       a b      = fromEither =<< BF.listScripts'                       a b
listScripts                                 = fromEither =<< BF.listScripts
getScript                          a        = fromEither =<< BF.getScript                          a
getScriptRedeemers'                a b c    = fromEither =<< BF.getScriptRedeemers'                a b c
getScriptRedeemers                 a        = fromEither =<< BF.getScriptRedeemers                 a
getScriptDatum                     a        = fromEither =<< BF.getScriptDatum                     a
getScriptDatumCBOR                 a        = fromEither =<< BF.getScriptDatumCBOR                 a
getScriptJSON                      a        = fromEither =<< BF.getScriptJSON                      a
getScriptCBOR                      a        = fromEither =<< BF.getScriptCBOR                      a

-- Client.Cardano.Epochs
getLatestEpoch                              = fromEither =<<  BF.getLatestEpoch
getLatestEpochProtocolParams                = fromEither =<<  BF.getLatestEpochProtocolParams
getEpoch                           a        = fromEither =<<  BF.getEpoch                           a
getNextEpochs'                     a b      = fromEither =<<  BF.getNextEpochs'                     a b
getNextEpochs                      a        = fromEither =<<  BF.getNextEpochs                      a
getPreviousEpochs'                 a b      = fromEither =<<  BF.getPreviousEpochs'                 a b
getPreviousEpochs                  a        = fromEither =<<  BF.getPreviousEpochs                  a
getEpochStake'                     a b      = fromEither =<<  BF.getEpochStake'                     a b
getEpochStake                      a        = fromEither =<<  BF.getEpochStake                      a
getEpochStakeByPool'               a b c    = fromEither =<<  BF.getEpochStakeByPool'               a b c
getEpochStakeByPool                a b      = fromEither =<<  BF.getEpochStakeByPool                a b
getEpochBlocks'                    a b c    = fromEither =<<  BF.getEpochBlocks'                    a b c
getEpochBlocks                     a        = fromEither =<<  BF.getEpochBlocks                     a
getEpochBlocksByPool'              a b c d  = fromEither =<<  BF.getEpochBlocksByPool'              a b c d
getEpochBlocksByPool               a b      = fromEither =<<  BF.getEpochBlocksByPool               a b
getEpochProtocolParams             a        = fromEither =<<  BF.getEpochProtocolParams             a

-- Client.Cardano.Transactions
getTx                              a        = fromEither =<< BF.getTx                              a
getTxUtxos                         a        = fromEither =<< BF.getTxUtxos                         a
getTxRedeemers                     a        = fromEither =<< BF.getTxRedeemers                     a
getTxStakes                        a        = fromEither =<< BF.getTxStakes                        a
getTxDelegations                   a        = fromEither =<< BF.getTxDelegations                   a
getTxWithdrawals                   a        = fromEither =<< BF.getTxWithdrawals                   a
getTxMirs                          a        = fromEither =<< BF.getTxMirs                          a
getTxPoolUpdates                   a        = fromEither =<< BF.getTxPoolUpdates                   a
getTxPoolRetiring                  a        = fromEither =<< BF.getTxPoolRetiring                  a
getTxMetadataJSON                  a        = fromEither =<< BF.getTxMetadataJSON                  a
getTxMetadataCBOR                  a        = fromEither =<< BF.getTxMetadataCBOR                  a
submitTx                           a        = fromEither =<< BF.submitTx                           a

-- Client.Cardano.Ledger
getLedgerGenesis                            = fromEither =<< BF.getLedgerGenesis

-- Client.Cardano.Accounts
getAccount                         a        = fromEither =<< BF.getAccount                         a
getAccountRewards'                 a b c    = fromEither =<< BF.getAccountRewards'                 a b c
getAccountRewards                  a        = fromEither =<< BF.getAccountRewards                  a
getAccountHistory'                 a b c    = fromEither =<< BF.getAccountHistory'                 a b c
getAccountHistory                  a        = fromEither =<< BF.getAccountHistory                  a
getAccountDelegations'             a b c    = fromEither =<< BF.getAccountDelegations'             a b c
getAccountDelegations              a        = fromEither =<< BF.getAccountDelegations              a
getAccountRegistrations'           a b c    = fromEither =<< BF.getAccountRegistrations'           a b c
getAccountRegistrations            a        = fromEither =<< BF.getAccountRegistrations            a
getAccountWithdrawals'             a b c    = fromEither =<< BF.getAccountWithdrawals'             a b c
getAccountWithdrawals              a        = fromEither =<< BF.getAccountWithdrawals              a
getAccountMirs'                    a b c    = fromEither =<< BF.getAccountMirs'                    a b c
getAccountMirs                     a        = fromEither =<< BF.getAccountMirs                     a
getAccountAssociatedAddresses'     a b c    = fromEither =<< BF.getAccountAssociatedAddresses'     a b c
getAccountAssociatedAddresses      a        = fromEither =<< BF.getAccountAssociatedAddresses      a
getAccountAssociatedAddressesTotal a        = fromEither =<< BF.getAccountAssociatedAddressesTotal a
getAccountAssociatedAssets'        a b c    = fromEither =<< BF.getAccountAssociatedAssets'        a b c
getAccountAssociatedAssets         a        = fromEither =<< BF.getAccountAssociatedAssets         a

-- Client.Cardano.Pools
listPools'                         a b      = fromEither =<< BF.listPools'                         a b
listPools                                   = fromEither =<< BF.listPools
listPoolsExtended'                 a b      = fromEither =<< BF.listPoolsExtended'                 a b
listPoolsExtended                           = fromEither =<< BF.listPoolsExtended
listRetiredPools'                  a b      = fromEither =<< BF.listRetiredPools'                  a b
listRetiredPools                            = fromEither =<< BF.listRetiredPools
listRetiringPools'                 a b      = fromEither =<< BF.listRetiringPools'                 a b
listRetiringPools                           = fromEither =<< BF.listRetiringPools
getPool                            a        = fromEither =<< BF.getPool                            a
getPoolHistory'                    a b c    = fromEither =<< BF.getPoolHistory'                    a b c
getPoolHistory                     a        = fromEither =<< BF.getPoolHistory                     a
getPoolMetadata                    a        = fromEither =<< BF.getPoolMetadata                    a
getPoolRelays                      a        = fromEither =<< BF.getPoolRelays                      a
getPoolDelegators'                 a b c    = fromEither =<< BF.getPoolDelegators'                 a b c
getPoolDelegators                  a        = fromEither =<< BF.getPoolDelegators                  a
getPoolBlocks'                     a b c    = fromEither =<< BF.getPoolBlocks'                     a b c
getPoolBlocks                      a        = fromEither =<< BF.getPoolBlocks                      a
getPoolUpdates'                    a b c    = fromEither =<< BF.getPoolUpdates'                    a b c
getPoolUpdates                     a        = fromEither =<< BF.getPoolUpdates                     a

-- Client.Cardano.Metadata
getTxMetadataLabels'               a b      = fromEither =<< BF.getTxMetadataLabels'               a b
getTxMetadataLabels                         = fromEither =<< BF.getTxMetadataLabels
getTxMetadataByLabelJSON'          a b c    = fromEither =<< BF.getTxMetadataByLabelJSON'          a b c
getTxMetadataByLabelJSON           a        = fromEither =<< BF.getTxMetadataByLabelJSON           a
getTxMetadataByLabelCBOR'          a b c    = fromEither =<< BF.getTxMetadataByLabelCBOR'          a b c
getTxMetadataByLabelCBOR           a        = fromEither =<< BF.getTxMetadataByLabelCBOR           a
