{- Copyright (c) 2006 John Goerzen
<jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Config where
import MissingH.ConfigParser
import Database.HDBC.PostgreSQL
import Database.HDBC.ODBC
import Database.HDBC
import System.IO
import Monad
import MissingH.Either
import System.Directory

loadCP :: IO ConfigParser
loadCP =
    let startCP = emptyCP {accessfunc = interpolatingAccess 5}
    in do cp <- readfile startCP "blogcvt.ini"
          return $ forceEither cp

connectDB :: String -> IO Connection
connectDB section =
    do cp <- loadCP
       let dbtype = forceEither $ get cp section "dbtype"
       let connstring = forceEither $ get cp section "connstring"
       case dbtype of
         "odbc" -> connectODBC connstring
         "postgresql" -> connectPostgreSQL connstring
         x -> fail $ "Unrecognized dbtype " ++ x

