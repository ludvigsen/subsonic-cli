{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module SubsonicAPI where

import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Maybe
import Control.Applicative
import Control.Monad
import GHC.Generics
import qualified Data.HashMap.Lazy as HM
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Network.HTTP (getResponseBody, simpleHTTP, getRequest, defaultGETRequest_)
import Network.URI(parseURI)
import Data.Scientific

data Song = 
    Song {
        songGenre :: !Text
      , songAlbumId :: Int
      , songAlbum :: !Text
      , songTrack :: Int
      , songParent :: Int
      , songContentType :: !Text
      , songIsDir :: Bool
      , songType :: !Text
      , songSuffix :: !Text
      , songIsVideo :: Bool
      , songSize :: Int
      , songId :: Int
      , songTitle :: !Text
      , songDuration :: Int
      , songIndexArtistId :: Int
      , songCreated :: !Text
      , songPath :: !Text
      , songYear :: Int
      , songIndexArtist :: !Text
      , songBitRate :: Int
      , songCoverArt :: Int
    } deriving (Show, Generic)

instance FromJSON Song where 
    parseJSON (Object v) = do 
        ge <- (v .:? "genre" .!= (pack ""))
        alid <- (v .:? "albumId" .!= 0)
        tr <- (v .:? "track" .!= 0)
        pa <- (v .:? "parent" .!= 0)
        ct <- (v .:? "contentType" .!= "")
        isd <- (v .:? "isDir" .!= False)
        ty <- (v .:? "type" .!= "")
        su <- (v .:? "suffix" .!= "")
        isv <- (v .:? "isVideo" .!= False)
        si <- (v .:? "size" .!= 0)
        id <- (v .: "id")
        ti <- (v .: "title")
        du <- (v .:? "duration" .!= 0)
        aid <- (v .:? "artistId" .!= 0)
        cr <- (v .:? "created" .!= "")
        pt <- (v .:? "path" .!= "")
        yr <- (v .:? "year" .!= 0)
        br <- (v .:? "bitRate" .!= 0)
        ca <- (v .:? "coverArt" .!= 0)
        return $ Song ge alid (artistNameToString (HM.lookupDefault Null "album" v)) tr pa ct isd ty su isv si id ti du aid cr pt yr  (artistNameToString (HM.lookupDefault Null "name" v)) br ca
    parseJSON _ = mzero


data Album =
   Album { genre  :: !Text
         , id   :: Int
         , song   :: [Song]
         , duration   :: Int
         , songCount   :: Int
         , created   :: !Text
         , artistId   :: Int
         , name   :: !Text
         , year   :: Int
         , albumArtist   :: !Text
         , coverArt   :: !Text
   } deriving (Show,Generic)

instance FromJSON Album where
    parseJSON (Object v) = do
        so <- parseJSON =<< (v .:? "song" .!= emptyArray)
        ge <- (v .:? "genre" .!= (pack ""))
        du <- (v .: "duration")
        i <- (v .: "id")
        sc <- (v .: "songCount")
        cr <- (v .: "created")
        ai <- (v .: "artistId")
        y <- (v .: "year")
        ca <- (v .:? "coverArt" .!= (pack ""))
        return $ Album ge i so du sc cr ai (artistNameToString (HM.lookupDefault Null "name" v)) y (artistNameToString (HM.lookupDefault Null "artist" v)) ca
    parseJSON _ = mzero

data Artist = Artist {
    artistGenre :: !Text,
    arId :: Int,
    artistAlbum :: [Album],
    artistName :: !Text,
    artistAlbumCount :: Int
    } deriving (Show,Generic)

instance FromJSON Artist where
    parseJSON (Object v) = do
        ge <- (v .:? "genre" .!= (pack ""))
        aid <- (v .: "id")
        ct <- (v .: "albumCount")
        let ar = HM.lookup "album" v
        case ar of
            (Just (Object _)) -> do
                alb <- parseJSON =<< (v .:? "album" .!= emptyObject)
                return $ Artist ge aid [alb] (artistNameToString (HM.lookupDefault Null "artist" v)) ct
            (Just (Array _)) -> do
                alb <- parseJSON =<< (v .:? "album" .!= emptyArray)
                return $ Artist ge aid alb (artistNameToString (HM.lookupDefault Null "artist" v)) ct
            _ -> return $ Artist ge aid [] (artistNameToString (HM.lookupDefault Null "artist" v)) ct
    parseJSON _ = mzero

data IndexArtist = IndexArtist {
    aId :: Int,
    indexArtistName :: !Text,
    albumCount :: Int,
    indexArtistGenre :: !Text
    }
    deriving (Show, Generic)

artistNameToString :: Value -> Text
artistNameToString (Number n) = pack $ formatScientific Fixed (Just 0) n
artistNameToString (String n) = n
artistNameToString _ = ""

instance FromJSON IndexArtist where
    parseJSON (Object v) = do
        i <- (v .: "id")
        ac <- (v .: "albumCount")
        ge <- (v .:? "genre" .!= "Empty")
        return $ IndexArtist i (artistNameToString (HM.lookupDefault Null "name" v)) ac ge 
    parseJSON _ = mzero


data Index = Index {
        indexName :: !Text,
        indexIndexArtist :: [IndexArtist]
    }
    deriving (Show, Generic)

instance FromJSON Index where
    parseJSON (Object v) = do
        nm <- (v .: "name")
        let ar = HM.lookup "artist" v
        case ar of
            (Just (Object _)) -> do
                art <- parseJSON =<< (v .: "artist")
                return $ Index nm [art]
            (Just (Array _)) -> do
                art <- parseJSON =<< (v .: "artist")
                return $ Index nm art
            _ -> mzero
    parseJSON _ = mzero

data Artists = Artists {
        index :: [Index]
      , ignoredArticles :: !Text
    }
    deriving (Show, Generic)


instance FromJSON Artists where
    parseJSON (Object v) = do
        ind <- parseJSON =<< (v .: "index")
        ig <- (v .: "ignoredArticles")
        return $ Artists ind ig
    parseJSON _ = mzero


-- | Type of each JSON entry in record syntax.
data SubsonicResponse =
   AlbumSubsonicResponse { album  :: Album
                    , status   :: !Text
                    , xmlns   :: !Text
                    , version   :: !Text
                    } 
                    |
   IndexSubsonicResponse { artists  :: Artists
                    , status   :: !Text
                    , xmlns   :: !Text
                    , version   :: !Text
                    }
                    |
   ArtistSubsonicResponse { artist  :: Artist
                    , status   :: !Text
                    , xmlns   :: !Text
                    , version   :: !Text
                    }
                    
                    deriving (Show,Generic)

instance FromJSON SubsonicResponse where
    parseJSON (Object v) = do
        al <- (v .:? "album") :: (Parser (Maybe Album))
        st <- (v .: "status")
        xm <- (v .: "xmlns")
        ve <- (v .: "version")
        if isJust al then do
            sr <- parseJSON =<< (v .: "album")
            return $ AlbumSubsonicResponse sr st xm ve
            else do
                ar <- (v .:? "artist") :: (Parser (Maybe Artist))
                if isJust ar then do  
                    return $ ArtistSubsonicResponse (fromJust ar) st xm ve
                    else do
                        ind <- parseJSON =<< (v .: "artists")
                        return $ IndexSubsonicResponse ind st xm ve
    parseJSON _ = mzero

data Response = 
    Response { subsonicResponse :: SubsonicResponse }
        deriving (Show, Generic)

instance FromJSON Response where
    parseJSON (Object v) = do
        sr <- parseJSON =<< (v .: "subsonic-response")
        return $ Response sr
    parseJSON _ = mzero

data APICredentials = APICredentials {
    url :: String,
    userName :: String,
    password :: String
}

getUrlWithoutMethod :: APICredentials -> String -> String -> String
getUrlWithoutMethod (APICredentials u un pw) m  extras = u ++ "/rest/" ++ m ++ ".view?u="++un ++ "&p=" ++ pw ++ "&v=1.9.0&c=subsonic-cli&f=json" ++ extras 


getUrl :: APICredentials -> String -> String -> String
getUrl (APICredentials u un pw) m  extras = u ++ "/rest/get" ++ m ++ ".view?u="++un ++ "&p=" ++ pw ++ "&v=1.9.0&c=subsonic-cli&f=json" ++ extras 

getIndexArtists :: APICredentials -> IO (Maybe [IndexArtist])
getIndexArtists apiC = do
    d <- (eitherDecode <$> (simpleHttp (getUrl apiC "Artists" ""))) :: IO (Either String Response)
    case d of
        Left _ -> return Nothing
        Right ps -> do
            let sr = subsonicResponse ps
            case sr of
                (IndexSubsonicResponse ind _ _ _) -> do
                    return $ Just (Prelude.concatMap indexIndexArtist (SubsonicAPI.index ind))
                _ -> return Nothing

getAlbums :: APICredentials -> Int -> IO(Maybe [Album])
getAlbums apiC id = do
    d <- eitherDecode <$> (simpleHttp (getUrl apiC "Artist" ("&id="++(show id)))) :: IO (Either String Response)
    res <- case d of
        Left err -> do
            putStrLn err
            return Nothing
        Right ps -> do
            let sr = subsonicResponse ps
            case sr of
                (ArtistSubsonicResponse art _ _ _) -> do
                    return $ Just $ artistAlbum art
                    --return $ Just (Prelude.concatMap indexIndexArtist (SubsonicAPI.index ind))
                _ -> return Nothing


    return res

getSongs :: APICredentials -> Int -> IO(Maybe [Song])
getSongs apiC id = do
    d <- eitherDecode <$> (simpleHttp (getUrl apiC "Album" ("&id="++(show id)))) :: IO (Either String Response)
    res <- case d of
        Left err -> do
            putStrLn err
            return Nothing
        Right ps -> do
            let sr = subsonicResponse ps
            case sr of
                (AlbumSubsonicResponse art _ _ _) -> do
                    return $ Just $ song art
                    --return $ Just (Prelude.concatMap indexIndexArtist (SubsonicAPI.index ind))
                _ -> return Nothing


    return res

--getStream :: APICredentials -> Int -> IO (Maybe
getStream :: APICredentials -> Int -> String -> IO String
getStream apiC id name = do
    let uri = case parseURI $ getUrlWithoutMethod apiC "stream" ("&id="++(show id)) of 
                Nothing -> error $ "Invalid URI"
                Just u -> u
    res <- simpleHTTP $ defaultGETRequest_ uri -- :: IO (Either String Response)
    resp <- getResponseBody res
    B.writeFile name resp
    return name
    {-res <- case d of
        Left err -> do
            putStrLn err
            return Nothing
        Right ps -> do
            let sr = subsonicResponse ps
            case sr of
                (AlbumSubsonicResponse art _ _ _) -> do
                    return $ Just $ song art
                    --return $ Just (Prelude.concatMap indexIndexArtist (SubsonicAPI.index ind))
                _ -> return Nothing
    return res-}
