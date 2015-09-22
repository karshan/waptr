{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
import Control.Arrow
import Database.Redis
import Data.Binary.Get (runGet)
import Data.Bson (Binary(..), Document, at)
import Data.Bson.Binary (getDocument)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Text.Parsec
import HHTTPP.Request (Request, parse_request)
import HHTTPP.Response (Response, parse_response)

unBinary :: Binary -> ByteString
unBinary (Binary b) = b

go :: [Document] -> [(Either ParseError Request, Either ParseError Response)]
go = map (\d -> (parse parse_request "" $ unBinary $ at "request" d, 
                 parse parse_response "" $ unBinary $ at "response" d))

rs :: IO [(Either ParseError Request, Either ParseError Response)] = do
  conn <- connect defaultConnectInfo
  r <- runRedis conn $ do
    llen "qert-history" >>= (either (error "llen failed") (lrange "qert-history" 0))
  either (error "lrange failed") (return . go . map (runGet getDocument . fromStrict)) r
