{-# LANGUAGE DerivingVia #-}

module Cardano.Api.IPC.Monad
  ( LocalStateQueryScript
  , sendMsgQuery
  , setupLocalStateQueryScript
  ) where

import Cardano.Api.Block
import Cardano.Api.IPC
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.Either
import Data.Function
import Data.Maybe
import Data.Ord
import Shelley.Spec.Ledger.Scripts ()
import System.IO

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query

newtype LocalStateQueryScript block point query r m a = LocalStateQueryScript
  { runLocalStateQueryScript
      :: (a -> m (Net.Query.ClientStAcquired block point query m r))
      -> m (Net.Query.ClientStAcquired block point query m r)
  } deriving (Functor, Applicative, Monad, MonadIO) via ContT (Net.Query.ClientStAcquired block point query m r) m

sendMsgQuery :: Monad m => query a -> LocalStateQueryScript block point query r m a
sendMsgQuery q = LocalStateQueryScript $ \f -> pure $
  Net.Query.SendMsgQuery q $
    Net.Query.ClientStQuerying
    { Net.Query.recvMsgResult = f
    }

setupLocalStateQueryScript ::
     STM x
  -> Maybe ChainPoint
  -> NodeToClientVersion
  -> TMVar (Maybe (Either Net.Query.AcquireFailure a))
  -> LocalStateQueryScript (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) () IO a
  -> Net.Query.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
setupLocalStateQueryScript waitDone mPointVar' ntcVersion resultVar' moo =
  LocalStateQueryClient $
    if ntcVersion >= NodeToClientV_8
      then do
        pure . Net.Query.SendMsgAcquire mPointVar' $
          Net.Query.ClientStAcquiring
          { Net.Query.recvMsgAcquired = runLocalStateQueryScript moo $ \result -> do
              atomically $ putTMVar resultVar' (Just (Right result))
              void $ atomically waitDone
              pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()

          , Net.Query.recvMsgFailure = \failure -> do
              atomically $ putTMVar resultVar' (Just (Left failure))
              void $ atomically waitDone
              pure $ Net.Query.SendMsgDone ()
          }
      else do
        atomically $ putTMVar resultVar' Nothing
        void $ atomically waitDone
        pure $ Net.Query.SendMsgDone ()
