{- Copyright (c) 2006 John Goerzen
<jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Main where
import System.IO
import Config
import Database.HDBC
import qualified Drupal
import MissingH.Logging.Logger
import S9Y

main = handleSqlError $
    do updateGlobalLogger "" (setLevel DEBUG)
       infoM "" "Welcome to blogcvt."
       srcdbh <- connectDB "source"
       infoM "" "Connected to source."
       destdbh <- connectDB "destination"

       sourceinfo <- Drupal.mine srcdbh

       cats <- getcats srcdbh
       writecats destdbh cats

       disconnect srcdbh
       disconnect destdbh


    