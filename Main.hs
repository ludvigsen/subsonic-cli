{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Maybe
import qualified Data.Text as T
import Data.IORef
import Graphics.Vty.Widgets.All
import Graphics.Vty.Attributes
import Graphics.Vty
import Control.Monad.State
import SubsonicAPI
import Control.Monad ( when, unless )
import Data.List ( intersperse )
import System.Exit ( exitFailure )
import System.IO ( hPutStrLn, stderr )
import Graphics.UI.SDL.Mixer.Music 
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer
import Control.Concurrent

username :: String
username = "admin"

password :: String 
password = "S0uThP4rK"

url :: String
url = "http://oij.me:4040"

apiC :: APICredentials
apiC = APICredentials Main.url Main.username Main.password

makeArtistList :: Widget (List T.Text FormattedText) -> [IndexArtist] ->IO ()
makeArtistList lst as = do
    clearList lst
    _ <- mapM (\s -> addToList lst (indexArtistName s) =<< plainText (indexArtistName s)) as
    return ()

makeAlbumList :: Widget (List T.Text FormattedText) -> [Album] -> IO ()
makeAlbumList lst albums = do
    clearList lst
    _ <- mapM (\s -> addToList lst (SubsonicAPI.name s) =<< plainText (SubsonicAPI.name s)) albums
    return ()

makeSongList :: Widget (List T.Text FormattedText) -> [Song] -> IO ()
makeSongList lst songs = do
    clearList lst
    _ <- mapM (\s -> addToList lst (songTitle s) =<< plainText ((T.pack $ show  $ songTrack s ) `T.append` (T.pack " ") `T.append` (songTitle s))) songs
    return ()

data CurrentView = Albums | Artists | Songs

player :: IORef [Song] -> IORef Bool -> IO()
player playlist songChange = do
    SDL.init [SDL.InitAudio]  -- init sdl
    playerLoop
        where playerLoop = do
                            pl <- readIORef playlist
                            if length pl > 0 then do
                                writeIORef songChange False
                                let currentSong = head pl
                                writeIORef playlist (tail pl)
                                file <- getStream apiC (songId currentSong) (T.unpack ((songTitle currentSong) `T.append` (T.pack ".") `T.append` (songSuffix currentSong)))
                                mus <- loadMUS file
                                result <- openAudio 22050 AudioS16Sys 2 4096
                                playMusic mus 0
                                fix $ \loop -> do
                                    sc <- readIORef songChange 
                                    if sc then do
                                        return ()
                                        else do
                                            stillPlaying <- playingMusic
                                            threadDelay 50
                                            when stillPlaying loop
                                freeMusic mus
                                closeAudio
                                else do
                                    return ()
                            threadDelay 1000000
                            playerLoop

playSong i songs playlist = do
    let newPlaylist = snd $ splitAt i songs 
    writeIORef playlist newPlaylist

main :: IO()
main = do
    -- IORefs
    currentView <- newIORef Main.Artists 
    currentAlbums <- newIORef []
    currentSongs <- newIORef []

    playlist <- newIORef []
    songChange <- newIORef False

    forkIO $ player playlist songChange

    lst <- newList (black `on` white) 1
    indexArtists <- getIndexArtists apiC
    makeArtistList lst (fromJust indexArtists)
    fg <- newFocusGroup
    _ <- addToFocusGroup fg lst
    c <- newCollection
    _ <- addToCollection c lst fg
    fg `onKeyPressed` \_ k _ ->
        case k of
            KASCII 'q' -> shutdownUi >> return True
            KEsc -> makeArtistList lst (fromJust indexArtists) >> writeIORef currentView Main.Artists >> return True
            _ -> return False
    lst `onItemActivated` \(ActivateItemEvent i _ _) -> do
        currV <- readIORef currentView
        case currV of
            Main.Artists -> do
                let currentArtist = (fromJust indexArtists) !! i
                albums <- getAlbums apiC $ aId currentArtist
                makeAlbumList lst $ fromJust albums
                writeIORef currentView Albums
                writeIORef currentAlbums $ fromJust albums
            Main.Albums -> do
                albums <- readIORef currentAlbums
                let currentAlbum = albums !! i
                songs <- getSongs apiC $ SubsonicAPI.id currentAlbum
                makeSongList lst $ fromJust songs
                writeIORef currentView Songs
                writeIORef currentSongs $ fromJust songs
            Main.Songs -> do
                songs <- readIORef currentSongs
                writeIORef songChange True
                playSong i songs playlist
                return ()
        SDL.quit
        return ()
    runUi c defaultContext
