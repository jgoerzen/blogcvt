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
import qualified S9Y

main = handleSqlError $
    do updateGlobalLogger "" (setLevel DEBUG)
       infoM "" "Welcome to blogcvt."
       srcdbh <- connectDB "source"
       destdbh <- connectDB "destination"
       infoM "" "Connected."

       sourceinfo <- Drupal.mine srcdbh

       cats <- Drupal.getcats srcdbh
       S9Y.writecats destdbh cats

       nodes <- Drupal.getNodes srcdbh sourceinfo
       S9Y.writeNodes destdbh nodes "jgoerzen" 1


       disconnect srcdbh
       disconnect destdbh


    