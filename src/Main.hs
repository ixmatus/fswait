{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Control.Applicative          (empty, (<|>))
import           Control.Concurrent           (threadDelay)
import qualified Control.Concurrent.STM       as STM
import qualified Control.Concurrent.STM.TMVar as TMVar
import           Control.Monad                (forever, when)
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NonEmpty
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import qualified Data.Text                    as Text
import qualified Data.Time.Units              as Time.Units
import qualified Filesystem.Path              as Path
import qualified Filesystem.Path.CurrentOS    as Path
import qualified Options.Applicative          as Options
import           Options.Generic
import           System.INotify               as INotify
import qualified System.Timeout
import           Turtle                       (ExitCode (..), d, fp, liftIO, s,
                                               void, (%), (</>))
import qualified Turtle

data Options w = Options
  { timeout :: w ::: Maybe Time.Units.Second <?> "Window to observe a filesystem event (default: 120s, negative values wait indefinitely)"
  , path    :: w ::: Path.FilePath           <?> "Observe filesystem events for path"
  , exists  :: w ::: Bool                    <?> "Return immediately if the filepath already exists"
  , events  :: w ::: NonEmpty EventVariety   <?> "Observable event"
  } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

instance ParseRecord Time.Units.Second where
  parseRecord = fmap getOnly parseRecord
instance ParseFields Time.Units.Second
instance ParseField Time.Units.Second where
  parseField h n =
    fmap (Time.Units.fromMicroseconds . (*(10^6)))
      (Options.option Options.auto $
       (  Options.metavar "Seconds"
       <> foldMap (Options.long . Text.unpack) n
       <> foldMap (Options.help . Text.unpack) h
       )
      )

deriving instance Show EventVariety
instance ParseRecord EventVariety where
  parseRecord = fmap getOnly parseRecord
instance ParseFields EventVariety
instance ParseField EventVariety where
  parseField _ _ =
        Options.flag' Access       (Options.long "access")
    <|> Options.flag' Modify       (Options.long "modify")
    <|> Options.flag' Attrib       (Options.long "attrib")
    <|> Options.flag' Close        (Options.long "close")
    <|> Options.flag' CloseWrite   (Options.long "closeWrite")
    <|> Options.flag' CloseNoWrite (Options.long "closeNoWrite")
    <|> Options.flag' Open         (Options.long "open")
    <|> Options.flag' Move         (Options.long "move")
    <|> Options.flag' MoveIn       (Options.long "moveIn")
    <|> Options.flag' MoveOut      (Options.long "moveOut")
    <|> Options.flag' MoveSelf     (Options.long "moveSelf")
    <|> Options.flag' Create       (Options.long "create")
    <|> Options.flag' Delete       (Options.long "delete")
    <|> Options.flag' OnlyDir      (Options.long "onlyDir")
    <|> Options.flag' NoSymlink    (Options.long "noSymlink")
    <|> Options.flag' MaskAdd      (Options.long "maskAdd")
    <|> Options.flag' OneShot      (Options.long "oneShot")
    <|> Options.flag' AllEvents    (Options.long "all")

main :: IO ()
main = do
  Options{..} <- unwrapRecord "Wait and observe events on the filesystem for a path, with a timeout"

  when exists $ do
    pathExists <- Turtle.testfile path
    when pathExists $ do
      Turtle.err $ Turtle.format ("exists: "%fp) path
      Turtle.exit ExitSuccess

  mvar <- STM.atomically TMVar.newEmptyTMVar

  let eventSet  = NonEmpty.toList (NonEmpty.nub events)
  let watchdir  = Path.encodeString (Path.directory path)
  let watchfile = Path.encodeString (Path.filename path)
  let timeout'  = fromMaybe (Time.Units.fromMicroseconds (120*(10^6))) timeout
  let eventsStr = Text.unwords $ fmap (Text.pack . show) eventSet

  let eventObservation =
        STM.atomically (TMVar.tryTakeTMVar mvar) >>= \case
          Nothing -> threadDelay 500000 >> eventObservation
          Just ev -> pure ev

  let writeEvent = STM.atomically . TMVar.tryPutTMVar mvar
  let fileEvent f e | f == watchfile = void (writeEvent e)
                    | otherwise      = empty

  let maybeFileEvent f e =
        case f of
          Nothing -> empty
          Just filepath
            | filepath == watchfile
              -> void (writeEvent e)
            | otherwise
              -> empty

  Turtle.err $ Turtle.format ("observing "%s%" for "%fp) eventsStr path

  let timeoutWindow | timeout' > 0 = Text.pack (show timeout')
                    | otherwise    = "indefinite"
  Turtle.err $ Turtle.format ("the window for an observation is "%s) timeoutWindow

  let doWatch =
        withINotify (\i -> do
          wid <- addWatch i eventSet watchdir $ \case
            ev@Accessed{..}   -> maybeFileEvent maybeFilePath ev
            ev@Attributes{..} -> maybeFileEvent maybeFilePath ev
            ev@Closed{..}     -> maybeFileEvent maybeFilePath ev
            ev@Created{..}    -> fileEvent filePath ev
            ev@Deleted{..}    -> fileEvent filePath ev
            ev@Modified{..}   -> maybeFileEvent maybeFilePath ev
            ev@MovedIn{..}    -> fileEvent filePath ev
            ev@MovedOut{..}   -> fileEvent filePath ev
            ev@MovedSelf{..}  -> print isDirectory
            ev@Opened{..}     -> maybeFileEvent maybeFilePath ev
            ev                -> print ev
          ev <- eventObservation
          removeWatch wid
          pure ev)

  System.Timeout.timeout
   (fromIntegral $ Time.Units.toMicroseconds timeout')
   doWatch >>= \case
    Nothing -> Turtle.exit (ExitFailure 1)
    Just ev -> Turtle.echo (Text.pack $ show ev)
