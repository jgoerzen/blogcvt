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

main = handleSqlError $
    do updateGlobalLogger "" (setLevel DEBUG)
       infoM "" "Welcome to blogcvt."
       srcdbh <- connectDB "source"
       destdbh <- connectDB "destination"
       infoM "" "Connected."

       Drupal.mine srcdbh
       disconnect srcdbh
       disconnect destdbh


    