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
import Data.Int
import Data.MessagePack
import Data.Maybe
import Data.Serialize.Get
import Network.Simple.TCP
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Serialize as S

type MsgId   = Int64
type MsgType = Int64

reqMessage :: MsgType
reqMessage = 0

resMessage :: MsgType
resMessage = 1

errorMsgId :: Int64
errorMsgId = 0

-- | The type of a message pack RPC method. It gets an Object as a parameter, and
--   returns either an error or the result as an Object.
type Method = Object -> IO (Either String Object)

-- | Start the RPC server binding the socket using the given preferences, and
--   using the RPC methods defined in the map of method name -> Method.
runRPC :: M.Map String Method -> HostPreference -> ServiceName -> IO ()
runRPC methods host service = 
    serve host service rpcServer
  where
    rpcServer (socket, _) =
        do reqMsg  <- getRequestMessage
           respMsg <- either (return . errorResponse errorMsgId) (executeRPC methods) reqMsg
           send socket $ S.encode respMsg
      where
        getRequestMessage =
            do result <- go $ runGetPartial S.get
               return $ case result of
                            Fail err _ -> Left err
                            Done r   _ -> Right r
                            Partial  _ -> Left "unexpected end of RPC message"
          where
            go partial = do bs <- fromMaybe BS.empty <$> recv socket 1024
                            case partial bs of
                                Partial partial' -> go partial'
                                x                -> return x

executeRPC :: M.Map String Method -> Object -> IO Object
executeRPC methods obj = 
   case getRPCData of
    Left err -> return $ errorResponse errorMsgId err
    Right (method, msgid, params) -> do result <- method params
                                        return $ either (errorResponse msgid) (resultResponse msgid) result
 where
   getRPCData =
    do let m .: k = maybe (Left $ "missing key: " ++ show k) Right $ M.lookup (ObjectString k) m
           getMethod ms name = maybe (Left $ "unsupported method: " ++ show name) Right $ M.lookup name ms
       case obj of
        (ObjectMap m) -> do ObjectInt type_ <- m .: "type"
                            when (type_ /= reqMessage) $ Left $ "invalid RPC type: " ++ show type_

                            ObjectString methodName <- m .: "method"
                            method <- getMethod methods $ unpack methodName

                            ObjectInt msgid <- m .: "msgid"
                            params          <- m .: "params"
                            return (method, msgid, params)
        _              -> Left "invalid msgpack RPC request"

errorResponse :: MsgId -> String -> Object
errorResponse msgid err = response msgid (ObjectString $ pack err) ObjectNil

resultResponse :: MsgId -> Object -> Object
resultResponse msgid = response msgid ObjectNil

response :: MsgId -> Object -> Object -> Object
response msgid errObj resObj =
    ObjectMap $ M.fromList [ (ObjectString "type"  , ObjectInt resMessage)
                           , (ObjectString "msgid" , ObjectInt msgid)
                           , (ObjectString "error" , errObj)
                           , (ObjectString "result", resObj) ]

