{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module IxFileLogger
    ( LogHandle
    , initFileLogger
    , logMessage
    , logError
    , logWarn
    , logInfo
    , finishLogger
    , withFileLogger
    )
where

import           Control.Monad.Trans
import           Control.Monad.State
import           Control.Monad.Trans.Control
import qualified Control.Exception.Safe
import qualified Control.Exception.Base        as B
import qualified Control.Exception.Safe        as ES
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                , readChan
                                                , writeChan
                                                , newChan
                                                )
import           Control.Concurrent.STM         ( atomically
                                                , writeTChan
                                                , readTChan
                                                , newTChan
                                                )
import           Control.Concurrent.STM.TBMChan
import           Control.Concurrent.Async.Lifted.Safe
import           Data.String
import           Data.Text
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Maybe
import           Data.Either
import           System.IO
import           System.Directory
import           System.FilePath.Posix
import           System.Clock

import           IxLib

data LogMessage = LogMsg Text | LogFinish

data LogHandle = LogHandle {reqChan :: TBMChan LogMessage, closeChan :: TBMChan Bool, threadId :: Maybe (Async ())}

data LogState = LogState {logFname :: FilePath, logHandle :: Handle, logTimeStamp :: TimeSpec}

logFunc :: MonadIO m => LogHandle -> m ()
logFunc handle = do
    let dir   = "log"
        fname = dir </> "events.log"
    liftIO $ createDirectoryIfMissing True dir
    fh <- liftIO $ openFile fname AppendMode
    liftIO $ hSetBuffering fh LineBuffering
    ts <- liftIO $ getTime Monotonic
    let rchan   = reqChan handle
        clsChan = closeChan handle
        iState =
            LogState { logFname = fname, logHandle = fh, logTimeStamp = ts }
        loop :: (MonadIO m, MonadState LogState m) => m ()
        loop = do
            state <- get
            let fHandle = logHandle state
            msg <- liftIO $ atomically $ readTBMChan rchan
            case msg of
                Nothing  -> return ()
                Just msg -> case msg of
                    LogMsg txt -> do
                        liftIO $ do
                            tm <- formatCurrentTime
                            T.hPutStrLn fHandle $ fromString tm <> ": " <> txt
                        rotateLog handle
                        loop
                    LogFinish -> liftIO $ do
                        atomically $ writeTBMChan clsChan True
                        hClose fHandle
    void $ execStateT loop iState

rotateLog :: (MonadIO m, MonadState LogState m) => LogHandle -> m ()
rotateLog handle = do
    state <- get
    ts2   <- liftIO $ getTime Monotonic
    let oldTs = toNanoSecs . logTimeStamp $ state
        ts    = toNanoSecs ts2
        tdiff = (ts - oldTs) `div` 1000000000
        hFile = logHandle state
    fSize <- liftIO $ hFileSize hFile
    when (tdiff >= totationTimeCheck && fSize >= rotationSise)
        $ rotateLog2 handle rotationCount
  where
    rotationSise      = 10485760
    totationTimeCheck = 3600
    rotationCount     = 5

    rotateLog2 :: (MonadIO m, MonadState LogState m) => LogHandle -> Int -> m ()
    rotateLog2 handle 0 = do
        state <- get
        let hFile     = logHandle state
            hFileName = logFname state
        liftIO $ do
            hClose hFile
            removeFile hFileName
        hfile2 <- liftIO $ openFile hFileName AppendMode
        let state2 = state { logHandle = hfile2 }
        put state2

    rotateLog2 handle 1 = do
        state <- get
        let hFileName = logFname state
            hFile     = logHandle state
        rr <- liftIO $ ES.tryAny $ renameFile hFileName $ hFileName <> ".0"
        if isRight rr
            then do
                liftIO $ hClose hFile
                hfile2 <- liftIO $ openFile hFileName AppendMode
                let state2 = state { logHandle = hfile2 }
                put state2
            else rotateLog2 handle 0

    rotateLog2 handle cnt = do
        state <- get
        let hFileName  = logFname state
            hFileName2 = hFileName <> "." <> fromString (show $ cnt - 2)
            hFileName1 = hFileName <> "." <> fromString (show $ cnt - 1)
        void $ liftIO $ ES.tryAny $ renameFile hFileName2 hFileName1
        rotateLog2 handle $ cnt - 1

initFileLogger :: MonadIO m => m LogHandle
initFileLogger = do
    requestChan <- liftIO $ atomically $ newTBMChan 1000
    clsChan     <- liftIO $ atomically $ newTBMChan 10
    let h = LogHandle requestChan clsChan Nothing
    thId <- liftIO $ async $ logFunc h
    return $ h { threadId = Just thId }

logMessage :: MonadIO m => LogHandle -> Text -> m ()
logMessage handle msg = do
    let rchan = reqChan handle
    liftIO $ atomically $ writeTBMChan rchan $ LogMsg msg

logInfo :: MonadIO m => LogHandle -> Text -> m ()
logInfo handle msg = logMessage handle $ "Info: " <> msg

logWarn :: MonadIO m => LogHandle -> Text -> m ()
logWarn handle msg = logMessage handle $ "Warning: " <> msg

logError :: MonadIO m => LogHandle -> Text -> m ()
logError handle msg = logMessage handle $ "Error: " <> msg

finishLogger :: MonadIO m => LogHandle -> m ()
finishLogger handle = do
    let rchan   = reqChan handle
        clsChan = closeChan handle
        thId    = fromJust . threadId $ handle
    liftIO $ do
        atomically $ writeTBMChan rchan LogFinish
        r <- atomically $ readTBMChan clsChan
        wait thId

withFileLogger :: MonadIO m => (LogHandle -> IO ()) -> m ()
withFileLogger logfunc =
    liftIO $ ES.bracket initFileLogger finishLogger logfunc
