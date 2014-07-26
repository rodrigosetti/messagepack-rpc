{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Network.MessagePack
Description : Message pack RPC over TCP
Copyright   : (c) 2014, Rodrigo Setti
License     : MIT
Maintainer  : rodrigosetti@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Network.MessagePack ( Method
                           , runRPC ) where

import Control.Applicative
import Control.Monad
import Data.MessagePack
import Network.Simple.TCP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Serialize as S
import qualified Data.Text as T

type MsgId   = Int
type MsgType = Int

reqMessage :: MsgType
reqMessage = 0

resMessage :: MsgType
resMessage = 1

errorMsgId :: Int
errorMsgId = 0

-- | The type of a message pack RPC method. It gets an Object as a parameter, and
--   returns either an error or the result as an Object.
type Method = Object -> IO (Either String Object)

-- | Start the RPC server binding the socket using the given preferences, and
--   using the RPC methods defined in the map of method name -> Method.
runRPC :: M.Map T.Text Method -> HostPreference -> ServiceName -> IO ()
runRPC methods host service = 
    serve host service rpcServer
  where
    rpcServer (socket, _) =
        do bs <- LBS.fromChunks <$> fetchData
           send socket =<< LBS.toStrict <$> executeRPC methods bs
      where
        chunkSize = 1024
        fetchData = do maybeData <- recv socket chunkSize
                       case maybeData of
                        Nothing -> return []
                        Just s  -> do let len = BS.length s
                                      if len < chunkSize
                                        then return [s]
                                        else do rest <- fetchData
                                                return $ s : rest

executeRPC :: M.Map T.Text Method -> LBS.ByteString -> IO LBS.ByteString
executeRPC methods input = 
   case getRPCData of
    Left err -> return $ errorResponse errorMsgId err
    Right (method, msgid, params) -> do result <- method params
                                        return $ either (errorResponse msgid) (resultResponse msgid) result
 where
   getRPCData =
    do let m .: k = maybe (Left $ "missing key: " ++ show k) Right $ M.lookup (ObjectString k) m
           getMethod ms name = maybe (Left $ "unsupported method: " ++ show name) Right $ M.lookup name ms
       obj <- S.decodeLazy input
       case obj of
        (ObjectMap m) -> do ObjectInt type_ <- m .: "type"
                            when (type_ /= reqMessage) $ Left $ "invalid RPC type: " ++ show type_

                            ObjectString methodName <- m .: "method"
                            method <- getMethod methods methodName

                            ObjectInt msgid <- m .: "msgid"
                            params          <- m .: "params"
                            return (method, msgid, params)
        _              -> Left "invalid msgpack RPC request"

errorResponse :: MsgId -> String -> LBS.ByteString
errorResponse msgid err = response msgid (ObjectString $ T.pack err) ObjectNil

resultResponse :: MsgId -> Object -> LBS.ByteString
resultResponse msgid = response msgid ObjectNil

response :: MsgId -> Object -> Object -> LBS.ByteString
response msgid errObj resObj =
    S.encodeLazy $ ObjectMap $ M.fromList [ (ObjectString "type"  , ObjectInt resMessage)
                                          , (ObjectString "msgid" , ObjectInt msgid)
                                          , (ObjectString "error" , errObj)
                                          , (ObjectString "result", resObj) ]

