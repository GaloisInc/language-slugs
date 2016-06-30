{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Slugs.Run (
    SlugsResult(..), runSlugs,
    SlugsError(..),
  ) where

import Control.Exception
import Language.Slugs.AST
import Language.Slugs.PP

import           Control.Monad (unless)
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


data SlugsResult = Unrealizable
                 | StateMachine FSM
                   deriving (Show)

data SlugsError = SlugsFailed
                | DecodeFailed
                  deriving (Typeable,Show)

instance Exception SlugsError


runSlugs :: FilePath -> Spec -> IO SlugsResult
runSlugs slugs spec =
  do tmpPath <- getTemporaryDirectory
     bracket (openTempFile tmpPath "slugsin") cleanup $ \ (tmpFile,h) ->
       do writeSpec spec h
          hClose h

          (ec,out,_) <- readProcessWithExitCode slugs
                           ["--explicitStrategy", "--jsonOutput", tmpFile]
                           L.empty
          if ec == ExitSuccess
             then case decode out of
                    Just val -> return (StateMachine val)
                    Nothing  -> throwIO DecodeFailed

             else return Unrealizable

  where
  cleanup (f,h) =
    do hClose h
       removeFile f


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
