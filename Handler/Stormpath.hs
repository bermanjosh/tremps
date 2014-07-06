{-# LANGUAGE OverloadedStrings #-}
module Stormpath where

import Data.Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.ByteString.Char8
import Network.HTTP.Client
import Network.HTTP.Client.TLS

data StormpathInfo = StormpathInfo
    { keyId     :: ByteString
    , keySecret :: ByteString
    , app    :: String
    } deriving Show
    
data LoginInfo = LoginInfo
    { email    :: String
    , password :: String
    } deriving Show
    
instance ToJSON LoginInfo where
     toJSON (LoginInfo email password) = object ["value" .= (B8.unpack . B64.encode . B8.pack $ email ++ ':' : password), "type" .= (String "basic")]
    
type UserURL = String

getUserURL :: StormpathInfo -> LoginInfo -> IO ()
getUserURL spi li = do
    let url = "https://api.stormpath.com/v1/applications/" ++ app spi ++ "/loginAttempts"
    req' <- parseUrl url
    let req = applyBasicAuth (keyId spi) (keySecret spi) $ req' 
            { method = "POST"
            , requestHeaders = [("Accept", "application/json"), ("Content-Type", "application/json")]
            , requestBody = RequestBodyBS . LB8.toStrict $ encode li
            }
    manager <- newManager $ tlsManagerSettings 
    resp <- httpLbs req manager
    print resp
            
one :: StormpathInfo 
one = StormpathInfo "2KD0CHRWCV3S4FT2C4NTUGU3L" "jrMbUuhIZLsboHsqAlR4QwHFsw4MhQ/Q3imMmo0Xw3c" "2NjsRRdpeSg2L2k6D1bo5O"

two :: LoginInfo
two = LoginInfo "joshb@suite-sol.com" "Test1234"