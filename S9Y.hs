{- Copyright (c) 2006 John Goerzen
<jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module S9Y where
import Database.HDBC
import System.IO
import Monad
import qualified Data.Map as Map
import Text.Printf
import MissingH.Logging.Logger

writecate :: Connection -> [(Int, (String, Int))] -> IO ()
writecats dbh catlist =
    do sth <- prepare dbh "INSERT INTO serendipity_category (categoryid, category_name, authorid, category_left, category_right, parentid) VALUES (?, ?, 0, 0, 0, ?)"
       mapM_ (insertcat sth) catlist
       finish sth
              
       sth <- prepare dbh "INSERT INTO serendipity_access (groupid, artifact_id, artifact_type, artifact_mode) VALUES (0, ?, 'category', ?)"
       mapM_ (insertaccess sth) catlist
       finish sth

       let newmax = maximum (map fst catlist) + 1
       run dbh "SELECT pg_catalog.setval(pg_catalog.pg_get_serial_sequence('serendipity_category', 'categoryid'), ?, true)" [toSql newmax]
       infoM "" $ "Category serial set to " ++ show newmax
       commit dbh
       infoM "" "Category changes finished."

    where insertcat sth (catid, (catname, parentid)) =
              do execute sth [toSql catid, toSql catname, toSql parentid]
                 infoM "" $ "Wrote category " ++ (show catid) ++
                            " " ++ catname ++ ", parent " ++ show parentid
          insertaccess sth (catid, (_, parentid)) =
              do execute sth [toSql catid, toSql "read"]
                 execute sth [toSql catid, toSql "write"]
                 infoM "" $ "Added read & write access for category " ++
                       show catid

       