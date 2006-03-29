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
import Drupal(Node(..))

writecats :: Connection -> [(Int, (String, Int))] -> IO ()
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

writeNodes :: Connection -> [Node] -> String -> Integer -> IO ()
writeNodes dbh nodes author authorid =
    do sth <- prepare dbh $
              "INSERT INTO serendipity_entries ((id, title, \"timestamp\", " ++
              "body, comments, trackbacks, extended, exflag, author, " ++
              "authorid, isdraft, allow_comments, last_modified, " ++
              "moderate_comments) VALUES (?, ?, ?, ?, 0, 0, ?, 0, " ++
              "?, ?, ?, 't', ?, 'f')"
       mapM_ (insertnode sth) nodes
       finish sth
       commit dbh

       let newmax = maximum (map nid nodes) + 1
       infoM "" $ "Setting id counter to " ++ show newmax
       run dbh "SELECT pg_catalog.setval(pg_catalog.pg_get_serial_sequence('serendipity_entries', 'id'), ?, true)" [toSql newmax]
       commit dbh
       
       infoM "" "BASE NODE INSERT COMPLETE"
       infoM "" "Setting entry properties..."
       sth <- prepare dbh "INSERT INTO serendipity_entryproperties (entryid, property, value) VALUES (?, ?, ?)"
       executeMany sth (concatMap convproperties nodes)
       finish sth
       commit dbh

       infoM "" "Setting read counters..."
       sth <- prepare dbh "INSERT INTO serendipity_karma (entryid, points, votes, lastvote, visits) VALUES (?, 0, 0, 0, ?)"
       mapM_ (setreadcount sth) nodes
       finish sth
       commit dbh

    where insertnode sth node =
              do infoM "" $ "Processing node " ++ show (nid node) ++
                            ", " ++ ntitle node
                 execute sth [toSql (nid node),
                              toSql (ntitle node),
                              toSql (ntimestamp node),
                              toSql (nteaser node),
                              toSql (nbody node),
                              toSql author,
                              toSql authorid,
                              toSql (nisdraft node),
                              toSql (nmodified node)]
          setreadcount sth node =
              do infoM "" $ "Node " ++ show (nid node) ++
                            ": read count is " ++ show (readcount node)
                 execute sth [toSql (nid node), toSql (readcount node)]
          convproperties node =
              [[toSql (nid node), toSql "ep_access", toSql "public"]] ++
              if disablenl2br node 
                 then [[toSql (nid node), toSql "ep_no_nl2br", toSql "true"]]
                 else []

writeNodeCats :: Connection -> [Node] -> [(Integer, Integer)] -> IO ()
writeNodeCats dbh nodes cats =
    do sth <- prepare dbh "INSERT INTO serendipity_entrycat (entryid, categoryid) VALUES (?, ?)"
       mapM_ (addcats sth) catstoprocess
       mapM_ (add0cat sth) nodeswithoutcats
       finish sth
       commit dbh
    where seennodes = map nid nodes
          catstoprocess = filter (\(nid, tid) -> nid `elem` seennodes) cats
          nodeswithoutcats = filter (\node -> not (nid node `elem` (map fst catstoprocess))) nodes
          addcats sth (nid, tid) =
              do infoM "" $ "Node " ++ show nid ++ ": Adding category " ++
                            show tid
                 execute sth [toSql nid, toSql tid]
          add0cat sth node =
              do infoM "" $ "Node " ++ show (nid node) ++ ": Adding 0 category"
                 execute sth [toSql (nid node), toSql (0::Integer)]