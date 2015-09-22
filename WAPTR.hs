{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module WAPTR where

import Control.Arrow
import Database.Redis
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.Binary.Get (runGet)
import Data.Bson (Binary(..), Document, at)
import Data.Bson.Binary (getDocument)
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.List (nub)
import Text.Parsec
import HHTTPP.Request (Request(..), parse_request, query_param_string)
import HHTTPP.Response (Response(..), parse_response)
import HHTTPP.Common

type Record = (Request, Response)

getHistory :: IO [Record] = do
  conn <- connect defaultConnectInfo
  r <- runRedis conn $ do
    llen "qert-history" >>= (either (error "llen failed") (lrange "qert-history" 0))
  either (error "lrange failed") (return . parse' . map (runGet getDocument . fromStrict)) r
    where
      parse' :: [Document] -> [(Request, Response)]
      parse' = mapMaybe (\d -> parse parse_request "" (unBinary (at "request" d)) & either (const Nothing) (\a ->
                     parse parse_response "" (unBinary (at "response" d)) & either (const Nothing) (\b ->
                     return (a, b))))
      unBinary :: Binary -> ByteString
      unBinary (Binary b) = b

andF :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andF f g a = (f a) && (g a)

orF :: (a -> Bool) -> (a -> Bool) -> a -> Bool
orF f g a = (f a) || (g a)

-- lenses
class HasCommonBody a where
  common_body :: a -> CommonBody
instance HasCommonBody Request where
  common_body = request_rest
instance HasCommonBody Response where
  common_body = response_rest

header :: (HasCommonBody a) => ByteString -> a -> ByteString
header h a = fromMaybe "" (lookupHeader h (headers (common_body a)))

host :: Record -> ByteString
host (req, _) = header "host" req

hasHeader :: (HasCommonBody a) => ByteString -> a -> Bool
hasHeader h a = maybe False (const True) (lookupHeader h (headers (common_body a)))

hostF :: (ByteString -> Bool) -> Record -> Bool
hostF f (req, _) = f (header "host" req)

reqF :: (Request -> Bool) -> Record -> Bool
reqF f (req, _) = f req

respF :: (Response -> Bool) -> Record -> Bool
respF f (_, resp) = f resp

p n (req, resp) = BS.putStrLn
                (verb req <> " " <> path req <> query_param_string (query_params req) <> " " <> request_version req
             <> "\n" <> BS.concat (map print_http (headers (common_body req))) <> "\n"
             <> BS.take n (body (common_body req))
             <> "\n" <> http_version resp <> " " <> status_code resp <> " " <> status_msg resp <> "\n"
             <> BS.concat (map print_http (headers (common_body resp))) <> "\n"
              <> BS.take n (body (common_body resp)) <> "\n")

ps = mapM_ (p 400)

(-=-) = isInfixOf
(&-&) = andF
(|-|) = orF

main = do
  rs <- getHistory
  ps $ filter (respF (hasHeader "Content-Encoding")) rs
