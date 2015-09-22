{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, FlexibleInstances #-}
module WAPTR where

import Control.Arrow
import Control.Monad
import Database.Redis
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.Binary.Get (runGet)
import Data.Bson (Binary(..), Document, at)
import Data.Bson.Binary (getDocument)
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.UTF8 as BS  (toString)
import qualified Data.ByteString.Lazy.UTF8 as LBS  (toString)
import Data.List (nub)
import Text.Parsec
import HHTTPP.Request (Request(..), parse_request, query_param_string)
import HHTTPP.Response (Response(..), parse_response)
import HHTTPP.Common
import IHaskell.Display (IHaskellDisplay(..), Display(..))
import qualified IHaskell.Display as IHaskell (html)
import Text.Blaze.Html5 (html, table, tr, td, toHtml, th, pre)
import Text.Blaze.Renderer.Utf8

-- util
lastM :: [a] -> Maybe a
lastM [] = Nothing
lastM (x:[]) = Just x
lastM (x:xs) = lastM xs

data Record = Record {
    recId :: String,
    req  :: Request,
    resp :: Response
  } deriving (Show, Eq, Ord)

data Records = Records [Record] -- for IHaskellDisplay instance

unRecords :: Records -> [Record]
unRecords (Records a) = a

getHistory :: IO Records = do
  conn <- connect defaultConnectInfo
  r <- runRedis conn $ do
    llen "qert-history" >>= (either (error "llen failed") (lrange "qert-history" 0))
  either (error "lrange failed") (return . parse' . map (runGet getDocument . fromStrict)) r
    where
      parse' :: [Document] -> Records
      parse' = Records .
                 mapMaybe (\d -> parse parse_request "" (unBinary (at "request" d)) &
                     either (const Nothing) (\req' ->
                     parse parse_response "" (unBinary (at "response" d)) &
                     either (const Nothing) (\resp' ->
                     return $ Record (at "id" d) req' resp')))
      unBinary :: Binary -> ByteString
      unBinary (Binary b) = b

redisSave :: IO () = do
  conn <- connect defaultConnectInfo
  runRedis conn save
  return ()

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
host Record{..} = header "host" req

hasHeader :: (HasCommonBody a) => ByteString -> a -> Bool
hasHeader h a = maybe False (const True) (lookupHeader h (headers (common_body a)))

hostF :: (ByteString -> Bool) -> Record -> Bool
hostF f Record{..} = f (header "host" req)

reqF :: (Request -> Bool) -> Record -> Bool
reqF f Record{..} = f req

respF :: (Response -> Bool) -> Record -> Bool
respF f Record{..} = f resp

idF :: (String -> Bool) -> Record -> Bool
idF f Record{..} = f recId

pathF :: (ByteString -> Bool) -> Record -> Bool
pathF f Record{..} = f (path req)

fileExtF :: (ByteString -> Bool) -> Record -> Bool
fileExtF f = pathF (maybe False f . lastM . BS.split '.')

p n Record{..} = putStrLn ("-------" ++ recId ++ "-------") >> BS.putStrLn
                (verb req <> " " <> path req <> query_param_string (query_params req) <> " " <> request_version req
             <> "\n" <> BS.concat (map print_http (headers (common_body req))) <> "\n"
             <> BS.take n (body (common_body req))
             <> "\n" <> http_version resp <> " " <> status_code resp <> " " <> status_msg resp <> "\n"
             <> BS.concat (map print_http (headers (common_body resp))) <> "\n"
              <> BS.take n (body (common_body resp)) <> "\n")

ellipsify :: Int -> String -> String
ellipsify n s = if length s > (n - 3) then take (n - 3) s ++ "..." else s

instance IHaskellDisplay Records where
  display rs = return $ Display [ IHaskell.html $ LBS.toString $ renderMarkup $ void $ html $ do
      table (
        tr (td "id" >> td "host" >> td "verb" >> td "path" >> td "status" >> td "length") >>
        mapM_ (\r@Record{..} -> do
          tr $ do
            td $ pre $ toHtml $ (take 6 recId)
            td $ pre $ toHtml $ BS.toString $ host r
            td $ pre $ toHtml $ BS.toString $ verb req
            td $ pre $ toHtml $ ellipsify 50 $ BS.toString $ path req <> query_param_string (query_params req)
            td $ pre $ toHtml $ BS.toString $ status_code resp
            td $ pre $ toHtml $ BS.toString $ header "Content-Length" resp
          ) (unRecords rs))
    ]

filt f = Records . filter f . unRecords

ps xs = putStrLn ((show (length xs)) ++ " records:") >> mapM_ (p 400) xs

(-=-) = isInfixOf
(&-&) = andF
(|-|) = orF

main = do
  rs <- getHistory
  ps $ filter (respF (hasHeader "Content-Encoding")) (unRecords rs)
