{-# LANGUAGE RecordWildCards, ScopedTypeVariables, OverloadedStrings, FlexibleInstances #-}
module WAPTR where

import Control.Arrow
import Control.Monad
import Database.Redis
import Data.Char
import Data.Maybe
import Data.Either
import Data.Monoid
import Data.Function
import Data.Binary.Get (runGet)
import Data.Bson (Binary(..), Document, at)
import Data.Bson.Binary (getDocument)
import Data.ByteString (ByteString, isInfixOf)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.UTF8 as BS  (toString)
import qualified Data.ByteString.Lazy.UTF8 as LBS  (toString)
import Data.List (nub, isPrefixOf)
import Text.Parsec (parse, ParseError)
import HHTTPP.Request (Request(..), parse_request, query_param_string)
import HHTTPP.Response (Response(..), parse_response)
import HHTTPP.Common
import IHaskell.Display (IHaskellDisplay(..), Display(..))
import qualified IHaskell.Display as IHaskell (html)
import Text.Blaze.Html5 (html, table, tr, td, toHtml, th, pre)
import Text.Blaze.Renderer.Utf8
import Control.Exception (evaluate, try, SomeException)
import qualified Codec.Compression.Zlib as Deflate
import qualified Codec.Compression.GZip as GZip
import System.IO.Unsafe (unsafePerformIO)
import Data.Aeson (json')
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Attoparsec.ByteString as Atto (parseOnly)

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

getRecord :: String -> IO Record
getRecord s = head . filter (idF (s `isPrefixOf`)) . unRecords <$> getHistory

getHistory :: IO Records
getHistory = Records . rights <$> _getHistory

parseErrors = lefts <$> _getHistory

_getHistory :: IO [Either ParseError Record] = do
  conn <- connect defaultConnectInfo
  r <- runRedis conn $ do
    llen "qert-history" >>= (either (error "llen failed") (lrange "qert-history" 0))
  either (error "lrange failed") (return . parse' . map (runGet getDocument . fromStrict)) r
    where
      parse' :: [Document] -> [Either ParseError Record]
      parse' = map (\d -> parse parse_request "" (unBinary (at "request" d)) &
                     either Left (\req' ->
                     parse parse_response "" (unBinary (at "response" d)) &
                     either Left (\resp' ->
                     return $ jsBeautifyRecord $ Record (at "id" d) (decompress req') (decompress resp'))))
      unBinary :: Binary -> ByteString
      unBinary (Binary b) = b

decompress :: HasCommonBody a => a -> a
decompress r = if      header "Content-Encoding" r `ciEq` "gzip" ||
                       header "Transfer-Encoding" r `ciEq` "gzip" then
                 setBody ((getBody r) { body = fromMaybe (body' r) $ gzipDecompress (body' r) }) r -- TODO real lenses
               else if header "Content-Encoding" r `ciEq` "deflate" ||
                       header "Transfer-Encoding" r `ciEq` "deflate" then
                 setBody ((getBody r) { body = fromMaybe (body' r) $ deflateDecompress (body' r) }) r
               else if header "Transfer-Encoding" r `ciEq` "chunked" then
                 setBody ((getBody r) { body = fromMaybe (body' r) $ unChunk (body' r) }) r
               else
                 r

eToM :: Either e a -> Maybe a
eToM (Left _) = Nothing
eToM (Right a) = Just a

gzipDecompress :: ByteString -> Maybe ByteString
gzipDecompress a = fmap (toStrict) $ eToM $ unsafePerformIO $ (try :: IO LBS.ByteString -> IO (Either SomeException LBS.ByteString))
                    $ evaluate (GZip.decompress (fromStrict a))

deflateDecompress :: ByteString -> Maybe ByteString
deflateDecompress a = fmap (toStrict) $ eToM $ unsafePerformIO $ (try :: IO LBS.ByteString -> IO (Either SomeException LBS.ByteString))
                    $ evaluate (Deflate.decompress (fromStrict a))

unChunk :: ByteString -> Maybe ByteString
unChunk = error "LOL CHUNKED"

ciEq :: ByteString -> ByteString -> Bool
ciEq a b = BS.map toLower a == BS.map toLower b

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
  getBody :: a -> CommonBody
  setBody :: CommonBody -> a -> a
instance HasCommonBody Request where
  getBody = request_rest
  setBody c r = r { request_rest = c }
instance HasCommonBody Response where
  getBody = response_rest
  setBody c r = r { response_rest = c }

body' :: (HasCommonBody a) => a -> ByteString
body' = body . getBody

header :: (HasCommonBody a) => ByteString -> a -> ByteString
header h a = fromMaybe "" (lookupHeader h (headers (getBody a)))

host :: Record -> ByteString
host Record{..} = header "host" req

hasHeader :: (HasCommonBody a) => ByteString -> a -> Bool
hasHeader h a = maybe False (const True) (lookupHeader h (headers (getBody a)))

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
fileExtF f = pathF (maybe True f . lastM . BS.split '.')

p :: Maybe Int -> Record -> ByteString
p maybeN Record{..} = verb req <> " " <> path req <> query_param_string (query_params req) <> " "
             <> request_version req
             <> "\n" <> BS.concat (map print_http (headers (getBody req))) <> "\n"
             <> maybe (body' req) (\n -> BS.take n $ body' req) maybeN <> "\n"
             <> "\n" <> http_version resp <> " " <> status_code resp <> " " <> status_msg resp <> "\n"
             <> BS.concat (map print_http (headers (getBody resp))) <> "\n"
             <> maybe (body' resp) (\n -> BS.take n $ body' resp) maybeN <> "\n"

ellipsify :: Int -> String -> String
ellipsify n s = if length s > (n - 3) then take (n - 3) s ++ "..." else s

instance IHaskellDisplay Records where
  display rs = return $ Display [ IHaskell.html $ LBS.toString $ renderMarkup $ void $ html $ do
      table (
        tr (td (pre "id") >> td (pre "host") >> td (pre "verb") >> td (pre "path") >> td (pre "status") >>
            td (pre "length")) >>
        mapM_ (\r@Record{..} -> do
          tr $ do
            td $ pre $ toHtml $ (take 6 recId)
            td $ pre $ toHtml $ BS.toString $ host r
            td $ pre $ toHtml $ BS.toString $ verb req
            td $ pre $ toHtml $ ellipsify 50 $ BS.toString $ path req <> query_param_string (query_params req)
            td $ pre $ toHtml $ BS.toString $ status_code resp
            td $ pre $ toHtml $ show $ BS.length (body' resp)
          ) (unRecords rs))
    ]

instance IHaskellDisplay Record where
  display r = return $ Display [ IHaskell.html $ LBS.toString $ renderMarkup $ void $ html $ do
      pre $ toHtml $ BS.toString (p Nothing r)
    ]

filt f = Records . filter f . unRecords
map' f = Records . map f . unRecords

(-=-) = isInfixOf
(&-&) = andF
(|-|) = orF

main = do
  rs <- getHistory
  undefined $ filter (respF (hasHeader "Content-Encoding")) (unRecords rs)

jsBeautify :: ByteString -> Maybe ByteString
jsBeautify s = (toStrict . encodePretty) <$> (eToM (Atto.parseOnly json' s))

jsBeautifyRecord :: Record -> Record
jsBeautifyRecord r = if "application/json" `BS.isPrefixOf` header "Content-Type" (resp r) then
                       maybe r (\beautified -> r { resp = (resp r) { response_rest = (response_rest (resp r)) { body = beautified }}}) (jsBeautify (body' $ resp r))
                     else
                       r
