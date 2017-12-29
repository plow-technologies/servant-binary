{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}
module Main (main, Foo (..)) where

import Data.Binary        (Binary(..))
import Data.Maybe         (fromMaybe)
import Network.Wai        (Application)
import Servant
import Servant.Binary
import System.Environment (getArgs, lookupEnv)
import Text.Read          (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

data Foo = Foo
    { fooName :: String
    , fooNum  :: Int
    }
  deriving (Eq, Show)

instance Binary Foo where
  put (Foo name num) = do
    put name
    put num
  get = do
    name <- get
    num <- get
    return $ Foo name num


type API = "post" :> ReqBody '[Bin GZip] [Foo] :> Post '[Bin GZip] [Foo]
      :<|> "post" :> "compress" :> ReqBody '[Bin NoCompression] [Foo] :> Post '[Bin GZip] [Foo]
      :<|> "binary" :> "gz" :> Get '[Bin GZip] [Foo]
      :<|> "binary" :> "raw" :> Get '[Bin NoCompression] [Foo]

api :: Proxy API
api = Proxy

server :: Server API
server = pure :<|> pure [Foo "GZipped!" 42] :<|> pure [Foo "Not compressed" 7]

app :: Application
app = serve api server

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        _ -> putStrLn "To run, pass run argument"