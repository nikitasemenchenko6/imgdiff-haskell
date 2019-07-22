{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Trustworthy           #-}
{-# LANGUAGE TypeOperators         #-}

module Web.Server
  ( listen
  , app
  )
where

import RIO

--import Control.Monad.Except
--import Control.Monad.Reader
--import Data.Attoparsec.ByteString
--import Data.ByteString (ByteString)
--import Data.List
--import Data.Maybe
--import Data.String.Conversions
--import Lucid
--import Network.HTTP.Media ((//), (/:))
--import System.Directory
--import Text.Blaze
--import Text.Blaze.Html.Renderer.Utf8
--import Servant.Types.SourceT (source)
--import qualified Data.Aeson.Parser
--import qualified Text.Blaze.Html
import Data.Aeson
import Data.Time.Calendar
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

server :: Server HelloAPI
server = return users

type HelloAPI = "hello" :> Get '[ JSON] [User]

data User =
  User
    { name              :: String
    , age               :: Int
    , email             :: String
    , registration_date :: Day
    }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

users :: [User]
users =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

userAPI = Proxy :: Proxy HelloAPI

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Application
app = serve userAPI server

listen :: IO ()
listen = runSettings settings app
 where
  logSettings = hPutStrLn stderr ("listening on port " ++ show port ++ ": " ++ show host)
  port        = 3000
  host        = "localhost"
  settings    = setPort port $ setHost host $ setBeforeMainLoop (logSettings) $ defaultSettings
