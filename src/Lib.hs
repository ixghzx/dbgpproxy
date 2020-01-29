{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveGeneric, ScopedTypeVariables #-}

module Lib
    ( start
    )
where

import           Data.Text
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.String.Conversions        ( cs )
import qualified Options.Applicative           as OA
import           Data.Semigroup                 ( (<>) )
import           Control.Applicative
import           Control.Concurrent.Async       ( concurrently
                                                , race
                                                )
import           Control.Monad.Trans
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.STM
import           Control.Concurrent.STM
import           Control.Exception.Safe
import           GHC.Generics
import           Network.Socket
import           Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString    as AP
import           Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8
                                               as AP8
import           Data.IP
import qualified Data.Map                      as M
import           Data.Map                       ( (!?)
                                                , (!)
                                                , Map
                                                )
import           Data.Function
import qualified Data.ByteString.Base64        as B64
import           Data.ByteString
import           Data.Maybe
import           Data.Either
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString               as BS
import qualified Data.Word8                    as W
import qualified Data.Yaml                     as Y
import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                , genericToEncoding
                                                , defaultOptions
                                                , Value(..)
                                                , (.:)
                                                )
import           Text.Read
import           Text.XML
import qualified Formatting                    as F
import           Formatting                     ( (%) )
import           Conduit
import           Data.Conduit.Network
import           Data.Streaming.Network
import           Database.SQLite.Simple
import qualified Database.SQLite.Simple        as S
import qualified Prelude                       as P
import           Prelude
import           System.Clock

import           IxLib
import           IxFileLogger

-- import Debug.Trace

data CConfig = CConfig {
  getConfigFile :: String, isVerbose :: Bool
} deriving(Show, Eq)

data ConfigData = ConfigData {debugPort :: Int, idePort :: Int} deriving(Show, Eq, Generic)

data AppCfg = AppCfg {cfgData :: ConfigData, cconfig :: CConfig} deriving(Show, Eq)

instance ToJSON ConfigData where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ConfigData

data SharedState = SharedState {xconn :: Connection, xtvar :: TVar (M.Map ByteString IdeData)}
data IdeData = IdeData {ideCmd :: ByteString, idePort2 :: Int,
    ipAddr :: Maybe (IP, PortNumber), ideKey :: ByteString, ideMulti :: Bool} deriving(Show, Eq)

data IdeMessage = IProxyInit ByteString Int ByteString Bool | IProxyStop ByteString ByteString | IUnknown ByteString deriving(Show, Eq)

data DebugState = DebugState {initial :: Bool, xcurrent :: ByteString, xcompleted :: Bool,
    xreply :: ByteString, xerror :: Bool, xIdeKey :: ByteString, xIpAddr :: Maybe (IP, PortNumber)}

samplex :: OA.Parser CConfig
samplex =
    CConfig
        <$> OA.strOption
                (  OA.long "config"
                <> OA.metavar "FILENAME"
                <> OA.value "config.yaml"
                <> OA.help "Config filename"
                )
        <*> OA.switch
                (OA.long "verbose" <> OA.short 'v' <> OA.help "Be verbose")

xshow :: Show a => a -> ByteString
xshow = cs . show

checkException
    :: (MonadIO m, MonadError ByteString m) => Either SomeException a -> m a
checkException x = do
    when (isLeft x) $ do
        let (Left e) = x
            e2       = cs . displayException $ e :: ByteString
        throwError e2
    let (Right x1) = x
    return x1

start :: MonadIO m => m ()
start = do
    aconfig <- liftIO $ OA.execParser opts
    let fname = getConfigFile aconfig
    config <-
        liftIO $ Y.decodeFileEither fname :: MonadIO m
        => m (Either Y.ParseException ConfigData)
    case config of
        Left exc ->
            liftIO $ T.putStrLn . cs . Y.prettyPrintParseException $ exc
        Right cfg -> do
            let appCfg = AppCfg { cfgData = cfg, cconfig = aconfig }
            withFileLogger $ startWithLogger appCfg
  where
    opts = OA.info
        (samplex <**> OA.helper)
        (OA.fullDesc <> OA.progDesc "dbgp proxy" <> OA.header
            "dbgpproxy — a simple implementation of a dbgp proxy"
        )

parseMsg :: Parser IdeMessage
parseMsg = parseProxyInit <|> parseProxyStop <|> parseProxyUnknown

parseProxyInit :: Parser IdeMessage
parseProxyInit = do
    cmd <- string "proxyinit"
    skipSpace
    string "-p"
    skipSpace
    port <- decimal
    skipSpace
    string "-k"
    skipSpace
    ikey <- AP8.takeWhile . AP8.inClass $ msgStringClass
    skipSpace
    string "-m"
    skipSpace
    xm <- decimal
    return $ IProxyInit cmd port ikey (xm == 1)

parseProxyStop :: Parser IdeMessage
parseProxyStop = do
    cmd <- string "proxystop"
    skipSpace
    string "-k"
    skipSpace
    ikey <- AP8.takeWhile . AP8.inClass $ msgStringClass
    return $ IProxyStop cmd ikey

msgStringClass = "0-9a-zA-Z_."

parseProxyUnknown :: Parser IdeMessage
parseProxyUnknown = IUnknown <$> takeByteString

startWithLogger :: MonadIO m => AppCfg -> LogHandle -> m ()
startWithLogger appCfg logger = do
    let idePort_   = idePort . cfgData $ appCfg
        debugPort_ = debugPort . cfgData $ appCfg
    logInfoX "Starting proxy listeners"
    logInfoX $ xsformat ("IDE listener on " % F.int) idePort_
    logInfoX $ xsformat ("dbgp listener on " % F.int) debugPort_
    sharedState <- liftIO initSharedState

    liftIO $ forkTCPServer (serverSettings debugPort_ "*6") $ conduitDebugFunc
        sharedState

    liftIO $ runTCPServer (serverSettings idePort_ "*6") $ conduitIdeFunc
        sharedState

    return ()
  where
    conduitIdeFunc sharedState appData =
        runConduit
            $  appSource appData
            .| txIdeFunc sharedState appData
            .| appSink appData

    conduitDebugFunc :: SharedState -> AppData -> IO ()
    conduitDebugFunc sharedState client = runConduit $ do
        packet <- liftIO $ appRead client
        dState <- checkInitialPacket sharedState client packet
        if xerror dState
            then do
                let errorMsg = createErrorMsg . xreply $ dState
                logErrorX . cs . xreply $ dState
                liftIO $ appWrite client errorMsg
            else do
                let msg               = xreply dState
                    replyMsg          = createReplyMsg msg
                    ipp               = xIpAddr dState
                    (Just (ip, port)) = ipp
                    port2             = fromIntegral port :: Int
                    ips               = show ip
                    ikey              = cs . xIdeKey $ dState :: Text
                xstart <- liftIO $ getTime Monotonic
                logInfoX
                    $ ("Starting debug session '" <> ikey <> "': Connecting to " :: Text
                      )
                    <> cs ips
                    <> ":"
                    <> (cs . show $ port)
                res <-
                    liftIO
                    $ tryAny
                    $ runTCPClient (clientSettings port2 $ cs ips)
                    $ \server -> do
                          liftIO $ appWrite server replyMsg
                          race
                              (runConduit $ appSource server .| appSink client)
                              (runConduit $ appSource client .| appSink server)
                          xend <- getTime Monotonic
                          let tDiff = formatTimeDiff xstart xend
                          logInfoX
                              $  "Closing session '"
                              <> ikey
                              <> "'(duration: "
                              <> cs tDiff
                              <> ")"
                when (isLeft res) $ do
                    let (Left exc) = res
                        msg        = displayException exc
                    logErrorX $ cs msg
                    liftIO $ appWrite client $ cs msg

    createErrorMsg :: ByteString -> ByteString
    createErrorMsg msg =
        let
            txId = 1
            xmsg =
                ("-i " :: ByteString)
                    <> (cs . show $ txId)
                    <> " -- "
                    <> B64.encode msg
        in
            xmsg

    createReplyMsg :: ByteString -> ByteString
    createReplyMsg msg =
        let len  = BS.length msg
            xstr = (cs . show $ len :: ByteString) <> "\0" <> msg <> "\0"
        in  xstr

    checkInitialPacket
        :: MonadIO m => SharedState -> AppData -> ByteString -> m DebugState
    checkInitialPacket sharedState client packet = do
        let dState = DebugState
                { initial    = True
                , xcompleted = False
                , xcurrent   = ""
                , xreply     = ""
                , xerror     = False
                , xIdeKey    = ""
                , xIpAddr    = Nothing
                }
        rr <- runExceptT $ do
            let (n, rest) = breakSubstring "\0" packet
                nn        = readMaybe (cs n) :: Maybe Int
            n2 <- case nn of
                Nothing -> do
                    let dState2 = dState
                            { xerror = True
                            , xreply =
                                "Closing connection, invalid protocol used"
                            }
                    throwError dState2
                Just n1 -> return n1
            let r2        = BS.stripPrefix "\0" rest
            when (isNothing r2) $ do
                let dState2 = dState
                        { xerror = True
                        , xreply = "Closing connection, invalid protocol used"
                        }
                throwError dState2
            let (Just r3)   = r2
                packetLen   = BS.length packet
                expectedLen = n2 + 2 + BS.length n
            when (packetLen < expectedLen) $ throwError dState -- packet is incomplete; more data is needed
            let (xml, _) = breakSubstring "\0" r3
                xdoc     = parseLBS def $ cs xml
            when (isLeft xdoc) $ do
                let (Left err) = xdoc
                    dState2    = dState { xerror = True
                                        , xreply = cs . displayException $ err
                                        }
                throwError dState
            let (Right doc) = xdoc
                droot       = documentRoot doc
                packetType  = nameLocalName . elementName $ droot
                attrs       = elementAttributes droot
                key         = attrs !? "idekey"
            when (packetType /= "init") $ do
                let msg =
                        ("Invalid packet type '" :: ByteString)
                            <> cs packetType
                            <> "'; must be 'init'"
                    dState2 = dState { xerror = True, xreply = msg }
                throwError dState2
            let tvar        = xtvar sharedState
                (Just ikey) = key
                bikey       = cs ikey :: ByteString
            idataMap <- liftIO $ readTVarIO tvar
            let idata = idataMap !? bikey
            when (isNothing idata) $ do
                let msg =
                        "No server with key \""
                        <> cs ikey
                        <> "\", stopping request" :: ByteString
                    dState2 = dState { xerror = True, xreply = msg }
                throwError dState2
            let attrs1 = M.insert "proxied" "true" attrs
                xaddr  = appSockAddr client
                xaddr2 = fromSockAddr xaddr
                attrs2 = if isJust xaddr2 && M.notMember "hostname" attrs
                    then
                        let Just (ip1, _) = xaddr2
                        in  M.insert "hostname" (cs . show $ ip1) attrs1
                    else attrs1
                droot2         = droot { elementAttributes = attrs2 }
                doc2           = doc { documentRoot = droot2 }
                xml2           = renderLBS def doc2
                (Just ideData) = idata
                ideAddr        = ipAddr ideData
            return dState { xerror  = False
                          , xreply  = cs xml2
                          , xIdeKey = bikey
                          , xIpAddr = ideAddr
                          }
        return $ case rr of
            Right a -> a
            Left  b -> b

    logErrorX :: MonadIO m => Text -> m ()
    logErrorX = logError logger

    logInfoX :: MonadIO m => Text -> m ()
    logInfoX = logInfo logger

    initSharedState :: MonadIO m => m SharedState
    initSharedState = do
        conn <- liftIO $ open "idedata.db"
        let sql =
                "create table if not exists idedata(id integer primary key autoincrement, key varchar(80) unique, multi boolean,"
                    <> "ip varchar(80), portnumber int)"
            sql2 = "select key, multi, ip, portnumber from idedata"
            xmap = M.empty :: M.Map ByteString IdeData
        liftIO $ execute_ conn sql
        rows <- liftIO (query_ conn sql2 :: IO [(String, Bool, String, Int)])
        let xfun (key1, mult, ip1, port1) m =
                let ip2   = read ip1 :: IP
                    portN = fromIntegral port1 :: PortNumber
                    idata = IdeData
                        { ideCmd   = "proxyinit"
                        , idePort2 = port1
                        , ideKey   = cs key1
                        , ideMulti = mult
                        , ipAddr   = Just (ip2, portN)
                        }
                    key2 = cs key1 :: ByteString
                in  M.insert key2 idata m
            xmap2 = P.foldr xfun xmap rows
        xx <- liftIO $ newTVarIO xmap2
        let v = SharedState { xconn = conn, xtvar = xx }
        return v

    addServer
        :: MonadIO m
        => IdeData
        -> SharedState
        -> m (Either ByteString ByteString)
    addServer idedata sharedState = do
        let tvar          = xtvar sharedState
            key           = ideKey idedata
            key2          = cs key :: String
            conn          = xconn sharedState
            (ip, portNum) = fromJust . ipAddr $ idedata
            port1         = idePort2 idedata
            multi         = ideMulti idedata
            ips           = show ip
            sqlI
                = "insert into idedata(key, multi, ip, portnumber) values(?,?,?,?)"
            sqlU = "update idedata set multi=?, ip=?, portnumber=? where key=?"
            sql  = "select id from idedata where key=?"

        (res :: Either ByteString ByteString) <- runExceptT $ do
            xmap <- liftIO $ readTVarIO tvar
            let alreadyReg = do
                    idata1   <- xmap !? key
                    ip1      <- ipAddr idata1
                    (ip2, _) <- Just ip1
                    let port2  = idePort2 idata1
                        multi2 = ideMulti idata1
                    guard $ ip == ip2 && port1 == port2 && multi == multi2
                    return True
            when (isJust alreadyReg) $ throwError "IDE Key already exists"

            r2 <- tryEx (query conn sql (Only key2) :: IO [Only Int])
            if P.null r2
                then tryEx $ S.execute conn sqlI (key2, multi, ips, port1)
                else tryEx $ S.execute conn sqlU (multi, ips, port1, key2)
            let portNum2 = fromIntegral port1 :: PortNumber
                ip2      = Just (ip, portNum2)
                idedata2 = idedata { ipAddr = ip2 }
            liftIO . atomically . modifyTVar tvar $ M.insert key idedata2
            return ("OK" :: ByteString)
        return res

    removeServer
        :: MonadIO m
        => IdeData
        -> SharedState
        -> m (Either ByteString ByteString)
    removeServer idedata sharedState = do
        let tvar = xtvar sharedState
            key  = ideKey idedata
            key2 = cs key :: String
            conn = xconn sharedState
            sqlD = "delete from idedata where key=?"
        rr <- liftIO $ tryAny $ S.execute conn sqlD (Only key2)
        liftIO . atomically . modifyTVar tvar $ M.delete key
        return $ Right "OK"

    tryEx :: (MonadIO m, MonadError ByteString m) => IO b -> m b
    tryEx x = do
        r <- liftIO $ tryAny x
        checkException r

    proxyError :: ByteString -> ByteString -> ByteString
    proxyError command msg =
        let z2 =
                ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" :: ByteString)
                    <> "\n<"
                    <> command
                    <> " success=\"0\"><error id=\""
                    <> xshow 0
                    <> "\"><message>"
                    <> msg
                    <> "</message></error></"
                    <> command
                    <> ">"
        in  cs z2

    proxySuccess :: IdeData -> ByteString
    proxySuccess ideData =
        let
            cmd           = ideCmd ideData
            ikey          = ideKey ideData
            idePort_      = idePort . cfgData $ appCfg
            (ip, portNum) = fromJust . ipAddr $ ideData
            msg           = case cmd of
                "proxyinit" ->
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<proxyinit success=\"1\" idekey=\""
                        <> ikey
                        <> "\" address=\""
                        <> xshow ip
                        <> "\" port=\""
                        <> xshow idePort_
                        <> "\"/>"
                _ -> proxyCmdMsg cmd ikey
        in
            msg

    proxyCmdMsg :: ByteString -> ByteString -> ByteString
    proxyCmdMsg cmd key =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<"
            <> cmd
            <> " success=\"1\" idekey=\""
            <> key
            <> "\"/>"

    txIdeFunc
        :: (MonadIO m)
        => SharedState
        -> AppData
        -> ConduitT ByteString ByteString m ()
    txIdeFunc sharedState appData = ideLoop
      where
        proxyInit :: (MonadIO m) => IdeData -> m (Either ByteString ByteString)
        proxyInit idedata = liftIO $ addServer idedata sharedState

        processMessage
            :: (MonadIO m)
            => ByteString
            -> Maybe (IP, PortNumber)
            -> m ByteString
        processMessage msg ipaddr = do
            let ideMsgE = parseOnly parseMsg msg
            case ideMsgE of
                Left err -> do
                    logErrorX $ "Faied to parse message '" <> cs msg <> "'"
                    return
                        $  ("Faied to parse message '" :: ByteString)
                        <> cs msg
                        <> "'"
                Right ideMsg -> processIdeMessage ideMsg ipaddr

        processIdeMessage
            :: (MonadIO m)
            => IdeMessage
            -> Maybe (IP, PortNumber)
            -> m ByteString
        processIdeMessage (IProxyInit xcmd xport xkey xmult) ipaddr = do
            let ideData = IdeData
                    { ideCmd   = xcmd
                    , idePort2 = xport
                    , ipAddr   = ipaddr
                    , ideKey   = xkey
                    , ideMulti = xmult
                    }
            xres <- liftIO $ proxyInit ideData
            case xres of
                Left err -> do
                    let cmd = ideCmd ideData
                    logErrorX
                        $  "Failed to run command '"
                        <> cs cmd
                        <> "' from server. Error: "
                        <> cs err
                    return $ proxyError cmd err
                Right r -> do
                    let v             = proxySuccess ideData
                        (ip, portNum) = fromJust . ipAddr $ ideData
                        portNum2      = idePort2 ideData
                    logInfoX
                        $  "Successfully registered IDE '"
                        <> (cs . ideKey $ ideData)
                        <> "' at "
                        <> "'"
                        <> (cs . show $ ip)
                        <> ":"
                        <> (cs . show $ portNum2)
                        <> "'"
                    return v

        processIdeMessage (IProxyStop cmd key) ipaddr = do
            vv <- runExceptT $ do
                let tvar = xtvar sharedState
                xmap <- liftIO $ readTVarIO tvar
                let idata = xmap !? key
                dtx <- case idata of
                    Nothing -> throwError "Server not registered"
                    Just d  -> return d
                removeServer dtx sharedState
                return $ proxyCmdMsg cmd key
            case vv of
                Left err -> do
                    logWarn logger . cs $ err
                    return $ proxyError cmd err
                Right ok -> return ok

        processIdeMessage (IUnknown msg) ipaddr = do
            let (ip, portNum) = fromJust ipaddr
            logErrorX
                $  "Unknown command '"
                <> cs msg
                <> "' from '"
                <> (cs . show $ ip)
                <> "'"
            return $ "Unknown command '" <> cs msg <> "'"

        ideLoop :: (MonadIO m) => ConduitT ByteString ByteString m ()
        ideLoop = do
            rr <- await
            case rr of
                Nothing -> do
                    logInfoX "Closing connection"
                    return ()
                Just v -> do
                    logInfoX $ "Message from IDE: " <> cs v
                    let xaddr  = appSockAddr appData
                        xaddr2 = fromSockAddr xaddr
                    rr <- processMessage v xaddr2
                    yield rr

