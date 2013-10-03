{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--import Control.Monad.Base (liftBase)
import Network.HTTP.Conduit --(ManagerSettings, Manager, newManager, def, Request(..), parseUrl, RequestBody(RequestBodyBS),httpLbs, responseBody)
--import Control.Monad.IO.Class (MonadIO (liftIO))
import Network.HTTP.Types (RequestHeaders)
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.CaseInsensitive as CI
import Control.Concurrent
import Control.Monad
import Data.ByteString (ByteString)
import Control.Concurrent.STM (TBQueue, newTBQueue, atomically, readTBQueue, writeTBQueue)


--import Network.HTTP.Types (StdMethod(POST))
--

body = "{\"advwatchid\":\"689340710147.0479_829816917091.873\",\"_domain\":\"www.ivi.ru\",\"advid\":14893,\"uid\":\"689340710147.0 479\",\"referrer\":\"http://www.ivi.ru/watch/lubovnitsy/56061\",\"watchid\":\"56061_689340710147.0479_1379413912675\",\"contentid\":56061,\"_url\":\"http://www.ivi.ru/watch/lubovnitsy/56061\",\"site\":\"s183\"}"

main :: IO ()
main = do
        let managerSettings = (def)::ManagerSettings
        let headers = [(CI.mk "Content-Type", "application/json")]
        let url = "http://localhost:4444/logger/adv/got/"

        queue <- atomically $ newTBQueue 10

        manager <- newManager managerSettings

        res <- runQuery manager url headers body
        replicateM_ 10 $ forkIO $ forever $ do 
                                     num <- atomically $ readTBQueue queue
                                     print $ show num
                                     runQuery manager url headers body
        print "RUN"
        let nums = [1..]
        forever $ do
            let num = head nums
            print $ "ZZZ" ++ (show num)
            atomically $ writeTBQueue queue num



runQuery:: Manager -> String -> RequestHeaders -> ByteString -> IO ()
runQuery manager url headers body = do
        defaultReq <- parseUrl url
        let req = defaultReq {method="POST", rawBody=True, requestBody=(RequestBodyBS body), requestHeaders = headers}
        resp <- runResourceT $  fmap responseBody $ httpLbs req manager
        L.putStrLn resp


{-runQuery :: MonadIO m => Manager -> Request m-> m L.ByteString-}
{-runQuery manager request = liftIO $ fmap responseBody $ httpLbs request manager-}


{-simpleHttp :: MonadIO m => String -> m L.ByteString-}
{-simpleHttp url = liftIO $ withManager $ \man -> do-}
    {-url' <- liftBase $ parseUrl url-}
    {-fmap responseBody $ httpLbs url' man-}
