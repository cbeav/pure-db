{-# LANGUAGE OverloadedStrings #-}
{- |

This module houses the storage layer of PureDB. It sets up a storage node on
a specified port, which handles GET, POST, and DELETE requests against an
HTTP path of the form /db/table/key1[/subkeys].

Maintainer: Chris Beavers <crbeavers@gmail.com>

-} 
module StorageNode (main) where

import qualified Blaze.ByteString.Builder.Char8 as B
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource (runResourceT, transResourceT)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Conduit.Lazy (lazyConsume)
import Data.Monoid
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.MySQL as HDBC
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types

import Data.Text.Encoding (encodeUtf8)

-- | Key record type to provide convenient access to heirarchical information.
data Key = Key {db :: B.ByteString, table :: B.ByteString, keys :: [B.ByteString]} deriving (Show)
parseKey (d:t:k) = Key {db=d, table=t, keys=k}

-- | Starts server on local port.
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

getStatement table = "SELECT * FROM `" ++ (B.unpack table) ++ "` WHERE k=?;"
deleteStatement table = "DELETE FROM `" ++ (B.unpack table) ++ "` WHERE k=?;"
upsertStatement table = "UPDATE `" ++ (B.unpack table) ++ "` SET v=? WHERE k=?;"

dbconn db = HDBC.connectMySQL HDBC.defaultMySQLConnectInfo {
                        HDBC.mysqlHost     = "localhost",
                        HDBC.mysqlUser     = "root",
                        HDBC.mysqlDatabase = B.unpack db,
                        HDBC.mysqlUnixSocket = "/tmp/mysql.sock"
                      }

-- | Application logic to parse requests and route to appropriate operations.
app :: Application
app req =
   do
    body <- lazyConsume . requestBody $ req
    let key  = parseKey $ map encodeUtf8 $ pathInfo req
    liftIO $
     case requestMethod req of
        "GET"    -> get key
        "DELETE" -> delete key
        "POST"   -> post key body
        --_        -> return $ ResponseBuilder status500 [("Content-Type", "text/plain")] $ B.fromText "Unsupported request type"

-- | Leverages HDBC to fetch a key.
get :: Key -> IO Response
get key = do
    conn <- dbconn $ db key
    result <- HDBC.quickQuery' conn (getStatement $ table key) [HDBC.toSql $ head $ keys key]
    HDBC.disconnect conn
    return $ getResponse result

getResponse []     = ResponseBuilder status404 [("Content-Type", "text/plain")] $ B.fromText "404: Not found"
getResponse result = ResponseBuilder status200 [("Content-Type", "text/plain")] $ B.fromText $ resultToText result

resultToText :: [[HDBC.SqlValue]] -> T.Text
resultToText res = foldl T.append T.empty $ map (\row -> HDBC.fromSql (row!!1)) res

-- | Leverages HDBC to perform a delete against a specified key.
delete :: Key -> IO Response
delete key = do
    conn <- dbconn $ db key
    HDBC.run conn (deleteStatement $ table key) [HDBC.toSql $ head $ keys key]
    HDBC.commit conn
    HDBC.disconnect conn
    return $ ResponseBuilder status204 [("Content-Type", "text/plain")] $ B.fromText $ "204: No content"

-- | Leverages HDBC to perform an upsert.
post :: Key -> [B.ByteString] -> IO Response
post key body = do
    conn <- dbconn $ db key
    let bodyBytes = mconcat body
    HDBC.run conn (upsertStatement $ table key) [HDBC.toSql $ bodyBytes, HDBC.toSql $ head $ keys key]
    HDBC.commit conn
    HDBC.disconnect conn
    return $ ResponseBuilder status201 [("Content-Type", "text/plain")] $ B.fromText $ "201: Created"
