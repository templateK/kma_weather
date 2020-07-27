{-# language DerivingStrategies #-}

module KMA
       ( API
       , Result(..)
       , Header(..)
       , Body(..)
       , Item(..)
       , clientApp
       , test
       ) where


import Servant hiding (Header)
import Servant.Client
import qualified Servant.Client.Core as SC
import Servant.API.ContentTypes
import Data.Aeson
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings, Request, requestHeaders)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import qualified Data.Text.Encoding    as T
import qualified Data.ByteString.Char8 as B
import Data.List.NonEmpty ( NonEmpty(..))

type API  = "1360000" :> "VilageFcstInfoService" :> "getVilageFcst"
            :> QueryParam' '[Required] "serviceKey" String
            :> QueryParam' '[Required] "pageNo" String
            :> QueryParam' '[Required] "numOfRows" String
            :> QueryParam' '[Required] "dataType" String
            :> QueryParam' '[Required] "base_date" String
            :> QueryParam' '[Required] "base_time" String
            :> QueryParam' '[Required] "nx" String
            :> QueryParam' '[Required] "ny" String
            :> Get '[KMAJSON] FcstResp

api :: Proxy API
api = Proxy

clientApp :: String -> String -> String -> String -> String -> String -> String -> String -> ClientM FcstResp
clientApp = client api

data KMAJSON

instance Accept KMAJSON where
  contentTypes _ = "application/json;charset=UTF-8" :| []

instance FromJSON a => MimeUnrender KMAJSON a where
  mimeUnrender _ = eitherDecodeLenient

test :: IO ()
test = do

  manager' <- newManager defaultManagerSettings
  let service_key = ""
  let baseUrl = BaseUrl Http "apis.data.go.kr" 80 ""
  let clientEnv = mkClientEnv manager' baseUrl
      saturatedClientApp = clientApp service_key "1" "10" "JSON" "20200726" "1130" "1" "1"
  -- let clientEnv' :: ClientEnv = clientEnv { makeClientRequest = removeAcceptHeaders }
  runClientM saturatedClientApp clientEnv >>= \case
    Left err -> handleClientError err
    Right fcstResp -> T.putStrLn . T.pack $ show fcstResp
  pure ()

-- removeAcceptHeaders :: BaseUrl -> SC.Request -> Request
-- removeAcceptHeaders baseUrl request =
--   let req = defaultMakeClientRequest baseUrl request
--   in req { requestHeaders = [("Accept-Encoding", ""), ("Connection", "Close") ] }

handleClientError :: ClientError -> IO ()
handleClientError err =
  case err of
    FailureResponse reqf resp             -> putStrLn "failresp" >> print reqf >> print resp
    DecodeFailure txt resp                -> putStrLn "decode fail" >> T.putStrLn txt >> print resp
    UnsupportedContentType _mediaTy resp  -> putStrLn "unsupported ctyp" >> (T.putStrLn . T.pack $ show resp)
    InvalidContentTypeHeader resp         -> putStrLn "InvalidctypHeader" >> (T.putStrLn . T.pack $ show resp)
    ConnectionError excpt                 -> putStrLn "Conn err" >> print excpt

data FcstResp = FcstResp
  { response :: HeaderBody }
  deriving stock (Generic, Show)

data HeaderBody = FcstHeaderBody
  { header :: Header
  , body   :: Body
  }
  deriving stock (Generic, Show)

data Header = Header
  { resultCode :: String
  , resultMsg :: String
  }
  deriving stock (Generic, Show)

data Body = Body
  { dataType :: String
  , items :: ItemWrapper
  , pageNo :: Int
  , numOfRows :: Int
  , totalCount :: Int
  }
  deriving stock (Generic, Show)

data ItemWrapper = ItemWrapper
  { item :: [Item] }
  deriving stock (Generic, Show)

data Item = Item
  { baseDate :: String
  , baseTime :: String
  , category :: String
  , fcstDate :: String
  , fcstTime :: String
  , fcstValue :: String
  , nx :: Int
  , ny :: Int
  }
  deriving stock (Generic, Show)

instance FromJSON Item
instance FromJSON ItemWrapper
instance FromJSON Body
instance FromJSON Header
instance FromJSON HeaderBody
instance FromJSON FcstResp

instance ToJSON ItemWrapper
instance ToJSON Item
instance ToJSON Body
instance ToJSON Header
instance ToJSON HeaderBody
instance ToJSON FcstResp

-- Reponse received {
--   "response":
--    {
--     "header": {"resultCode":"00", "resultMsg":"NORMAL_SERVICE"},
--     "body": { "dataType":"JSON",
  --     "items":{ "item":
  --     [{"baseDate":"20200726","baseTime":"0800","category":"POP","fcstDate":"20200726","fcstTime":"1200","fcstValue":"20","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"PTY","fcstDate":"20200726","fcstTime":"1200","fcstValue":"0","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"R06","fcstDate":"20200726","fcstTime":"1200","fcstValue":"0","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"REH","fcstDate":"20200726","fcstTime":"1200","fcstValue":"70","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"S06","fcstDate":"20200726","fcstTime":"1200","fcstValue":"0","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"SKY","fcstDate":"20200726","fcstTime":"1200","fcstValue":"3","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"T3H","fcstDate":"20200726","fcstTime":"1200","fcstValue":"28","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"UUU","fcstDate":"20200726","fcstTime":"1200","fcstValue":"0.5","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"VEC","fcstDate":"20200726","fcstTime":"1200","fcstValue":"191","nx":97,"ny":77},
  --       {"baseDate":"20200726","baseTime":"0800","category":"VVV","fcstDate":"20200726","fcstTime":"1200","fcstValue":"2.5","nx":97,"ny":77} ]
  --       },
  --       "pageNo":1,
  --       "numOfRows":10,
  --       "totalCount":216
--    }}}
-- var url = 'http://apis.data.go.kr/1360000/VilageFcstInfoService/getVilageFcst';
-- var queryParams = '?' + 'serviceKey' + '=' + service_key;
-- queryParams += '&' + encodeURIComponent('pageNo') + '=' + encodeURIComponent('1'); /* */
-- queryParams += '&' + encodeURIComponent('numOfRows') + '=' + encodeURIComponent('10'); /* */
-- queryParams += '&' + encodeURIComponent('dataType') + '=' + encodeURIComponent('JSON'); /* */
-- queryParams += '&' + encodeURIComponent('base_date') + '=' + encodeURIComponent('20200726'); /* */
-- queryParams += '&' + encodeURIComponent('base_time') + '=' + encodeURIComponent('0800'); /* */
-- queryParams += '&' + encodeURIComponent('nx') + '=' + encodeURIComponent('97'); /* */
-- queryParams += '&' + encodeURIComponent('ny') + '=' + encodeURIComponent('77'); /* */
