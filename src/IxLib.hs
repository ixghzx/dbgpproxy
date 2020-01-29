{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts, FlexibleInstances #-}
module IxLib
  ( xstring
  , shown
  , xsformat
  , xformat
  , xtext
  , formatCurrentTime
  , formatTimeDiff
  , findConfig
  , csx
  , ErrMonad(..)
  , XResponse(..)
  , ErrorResult(..)
  )
where

import           Control.Monad.Except
import System.Clock
import           Data.Aeson
import qualified Data.ByteString               as BS
import           Data.String.Conversions
import           Data.Time
import           Data.Text
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Text.Encoding.Error
import qualified Data.Text.Lazy                as TL
import qualified Formatting                    as F
import           Formatting                     ( (%) )
import qualified Data.Yaml                     as Y
import           System.IO                     as SIO

xstring :: F.Format r (String -> r)
xstring = F.string

shown :: Show a => F.Format r (a -> r)
shown = F.shown

xsformat :: F.Format Text a -> a
xsformat = F.sformat

xformat :: F.Format TL.Text a -> a
xformat = F.format

xtext :: F.Format r (TL.Text -> r)
xtext = F.text

type ErrMonad a = ExceptT a IO

data XResponse
  = XError Text
  | XRedirect Int
              String
  | XOK

data ErrorResult = ExError Text | ExSuccess deriving (Show)

class IxToText a where
  asText :: a -> Text

instance IxToText Text where
    asText = id

instance IxToText TL.Text where
    asText = TL.toStrict

instance IxToText String where
    asText = pack

instance IxToText BS.ByteString  where
    asText = T.decodeUtf8With lenientDecode

csx :: IxToText a => a -> Text
csx = asText

formatCurrentTime :: IO String
formatCurrentTime = do
  let fmt = "%F %T"
  formatTime defaultTimeLocale fmt <$> getZonedTime

formatTimeDiff :: TimeSpec -> TimeSpec -> TL.Text   
formatTimeDiff tsStart tsEnd = fmt nsDiff
  where
    nsDiff = toNanoSecs tsEnd - toNanoSecs tsStart
    ns = nsDiff
    xday = (10 ^ 9) * 60 * 60 * 24
    xhour = (10 ^ 9) * 60 * 60
    xmin = (10 ^ 9) * 60
    xsec = 10 ^ 9
    xmsec = 10 ^ 6
    xusec = 10 ^ 3

    scale :: Integer -> Maybe Double 
    scale i = if ns >=i 
      then Just $ fromIntegral ns / fromIntegral i
      else Nothing

    fmt diff
        | Just i <- scale xday = formatDays ns
        | Just i <- scale xhour = formatHours ns
        | Just i <- scale xmin = formatMins ns
        | Just i <- scale xsec = formatSecs ns
        | Just i <- scale xmsec = formatMsecs ns
        | Just i <- scale xusec = formatUsecs ns
        | otherwise = formatNsecs ns

    formatDays :: Integer -> TL.Text
    formatDays all = 
      let d = all `div` xday
          rest = all `mod` xday
          h = rest `div` xhour
          hrest = rest `mod` xhour
          m = hrest `div` xmin
          mrest = hrest `mod` xmin
          s = fromIntegral mrest / fromIntegral xsec
      in xformat (F.int % " d " % F.int % " h " % F.int % " m " % F.fixed 2 % " s") d h m s

    formatHours :: Integer -> TL.Text
    formatHours all = 
      let h = all `div` xhour
          hrest = all `mod` xhour
          m = hrest `div` xmin
          mrest = hrest `mod` xmin
          s = fromIntegral mrest / fromIntegral xsec
      in xformat (F.int % " h " % F.int % " m " % F.fixed 2 % " s") h m s 
      
    formatMins :: Integer -> TL.Text
    formatMins all = 
      let m = all `div` xmin
          mrest = all `mod` xmin
          s = fromIntegral mrest / fromIntegral xsec
      in xformat (F.int % " m " % F.fixed 3 % " s") m s      

    formatSecs :: Integer -> TL.Text
    formatSecs all = 
      let s = fromIntegral all / fromIntegral xsec
      in xformat (F.fixed 6 % " s") s
      
    formatMsecs :: Integer -> TL.Text
    formatMsecs all = 
      let ms = fromIntegral all / fromIntegral xmsec
      in xformat (F.fixed 3 % " ms") ms

    formatUsecs :: Integer -> TL.Text
    formatUsecs all = 
      let us = fromIntegral all / fromIntegral xusec
      in xformat (F.fixed 3 % " us") us

    formatNsecs :: Integer -> TL.Text
    formatNsecs = xformat (F.int % " ns")
 
findConfig
    :: (MonadIO m, MonadError ErrorResult m, FromJSON a)
    => [Text]
    -> m (a, Text)
findConfig plist = do
    r <- findCfg plist
    case r of
        Just v  -> return v
        Nothing -> throwError $ ExError "Failed to and parse config file"
  where
    findCfg :: (MonadIO m, FromJSON a) => [Text] -> m (Maybe (a, Text))
    findCfg []       = return Nothing
    findCfg (x : xs) = do
        let x2 :: SIO.FilePath = cs x
        ret :: Either Y.ParseException a <- liftIO $ Y.decodeFileEither x2
        case ret of
            Left  _ -> findCfg xs
            Right v -> return $ Just (v, csx x)