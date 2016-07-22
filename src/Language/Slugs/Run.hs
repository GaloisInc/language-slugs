{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.Run (
    SlugsResult(..), runSlugs,
    SlugsError(..),
    FSM(..),
    Node(..)
  ) where

import Control.Exception
import Language.Slugs.AST
import Language.Slugs.PP

import           Control.Monad (unless,when)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Read as T
import           Data.Typeable
import           System.Directory (getTemporaryDirectory,removeFile)
import           System.Exit (ExitCode(..))
import           System.IO (openTempFile,Handle,hFlush,hPrint,hClose)
import           System.Process.ByteString.Lazy (readProcessWithExitCode)


data SlugsResult = Unrealizable FSM
                   -- ^ The input specification was not realizable, and the
                   -- returned FSM is a counter-strategy.

                 | StateMachine FSM
                   -- ^ The input specification was realizable, and the returned
                   -- FSM describes the controller.
                   deriving (Show)

data SlugsError = SlugsError L.ByteString
                | DecodeFailed
                  deriving (Typeable,Show)

instance Exception SlugsError


data SlugsResponse = RespRealizable
                   | RespUnrealizable
                   | RespError L.ByteString
                     deriving (Eq,Show)

-- | From the stderr of running slugs, interpret its response.
parseResponse :: L.ByteString -> SlugsResponse
parseResponse err =
  case take 1 (drop 1 (L.lines err)) of
    ["RESULT: Specification is realizable."]     -> RespRealizable
    ["RESULT: Specification is not realizable."] -> RespUnrealizable
    ls                                           -> RespError (L.unlines ls)



runSlugs :: Bool -> FilePath -> Spec -> IO SlugsResult
runSlugs dbg slugs spec =
  do tmpPath <- getTemporaryDirectory
     bracket (openTempFile tmpPath "slugsin") cleanup $ \ (tmpFile,h) ->
       do writeSpec spec h
          hClose h

          (ec,out,err) <- readProcessWithExitCode slugs
                             ["--explicitStrategy", "--jsonOutput", tmpFile]
                             L.empty

          when dbg (L.putStrLn err)

          case parseResponse err of

            -- decode the json result
            RespRealizable ->
              do when dbg (L.putStrLn out)
                 case decode out of
                   Just val -> return (StateMachine val)
                   Nothing  -> throwIO DecodeFailed

            -- re-run slugs and generate the counter-strategy
            RespUnrealizable ->
              do fsm <- counterStrategy dbg slugs tmpFile
                 return (Unrealizable fsm)

            RespError msg -> throwIO (SlugsError msg)

  where
  cleanup (f,h) =
    do hClose h
       removeFile f


counterStrategy :: Bool -> FilePath -> FilePath -> IO FSM
counterStrategy dbg slugs specFile = return undefined
  -- do (ec,out,err) <- readProcessWithExitCode slugs ["--counterStrategy", specFile]

  --    when dbg (L.putStrLn err)

  --    case parseResponse err of
  --      RespRealizable ->


-- | Write the specification to a temporary file.
writeSpec :: Spec -> Handle -> IO ()
writeSpec spec h =
  do hPrint h (ppSpec spec)
     hFlush h


-- Controller ------------------------------------------------------------------

data FSM = FSM { fsmStateDescr :: [String]
               , fsmNodes      :: Map.Map Int Node
               } deriving (Show)

data Node = Node { nRank  :: Int
                 , nState :: [Int]
                 , nTrans :: [Int]
                 } deriving (Show)


instance FromJSON FSM where
  parseJSON = withObject "FSM" $ \ obj ->
    do fsmStateDescr  <- obj .: "variables"
       Nodes fsmNodes <- obj .: "nodes"
       return FSM { .. }

newtype Nodes = Nodes (Map.Map Int Node)

instance FromJSON Nodes where
  parseJSON = withObject "FSM Nodes" $ \ obj ->
    do let parse (k,v) =
             case T.decimal k of
               Right (n,_) -> do node <- parseJSON v
                                 return (n,node)
               Left err    -> fail err

       nodes <- traverse parse (HM.toList obj)
       return (Nodes (Map.fromList nodes))

instance FromJSON Node where
  parseJSON = withObject "FSM Nodes" $ \ obj ->
    do nRank  <- obj .: "rank"
       nState <- obj .: "state"
       nTrans <- obj .: "trans"
       return Node { .. }
