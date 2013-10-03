{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--import Control.Monad.Base (liftBase)
import Network.HTTP.Conduit --(ManagerSettings, Manager, newManager, def, Request(..), parseUrl, RequestBody(RequestBodyBS),httpLbs, responseBody)
--import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.CaseInsensitive as CI

--import Network.HTTP.Types (StdMethod(POST))
--

body = "{\"advwatchid\":\"689340710147.0479_829816917091.873\",\"_domain\":\"www.ivi.ru\",\"advid\":14893,\"uid\":\"689340710147.0 479\",\"referrer\":\"http://www.ivi.ru/watch/lubovnitsy/56061\",\"watchid\":\"56061_689340710147.0479_1379413912675\",\"contentid\":56061,\"_url\":\"http://www.ivi.ru/watch/lubovnitsy/56061\",\"site\":\"s183\"}"

main :: IO ()
main = do
        defaultReq <- parseUrl "http://localhost:4444/logger/adv/got/"
        let managerSettings = (def)::ManagerSettings
            headers = [(CI.mk "Content-Type", "application/json")]
            req = defaultReq {method="POST", rawBody=True, requestBody=(RequestBodyBS body), requestHeaders = headers}
        manager <- newManager managerSettings
        runResourceT $  fmap responseBody $ httpLbs req manager
        print "TEST"


{-runQuery :: MonadIO m => Manager -> Request m-> m L.ByteString-}
{-runQuery manager request = liftIO $ fmap responseBody $ httpLbs request manager-}


{-simpleHttp :: MonadIO m => String -> m L.ByteString-}
{-simpleHttp url = liftIO $ withManager $ \man -> do-}
    {-url' <- liftBase $ parseUrl url-}
    {-fmap responseBody $ httpLbs url' man-}
