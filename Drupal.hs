{- Copyright (c) 2006 John Goerzen
<jgoerzen@complete.org>
Please see the COPYRIGHT file
-}

module Drupal where
import Database.HDBC
import System.IO
import Monad
import qualified Data.Map as Map
import Text.Printf
import MissingH.Logging.Logger
import Control.Concurrent.MVar

delta_htmlfilt = "0"
delta_phpfilt = "1"
delta_linefilt = "2"

mine dbh =
    do filters <- getfilters dbh
       infoM "" "Found filters."
       mapM_ (\(id, (name, nl)) -> 
                  printf "Filter %s (%s): Handles newlines: %s\n"
                         id name (show nl)) filters

       return filters

getfilters :: Connection -> IO [(String, (String, Bool))]
getfilters dbh =
    do res <- quickQuery dbh "select filters.format, name, delta from filters, filter_formats where filters.format = filter_formats.format" []
       return $ Map.toAscList . foldl (\x -> procres x . map fromSql) 
                Map.empty $ res
    where procres amap [format, name, delta] =
              case Map.lookup format amap of
                Nothing -> Map.insert format (name, islinefilt delta) amap
                Just (_, True) -> amap
                Just (_, False) -> Map.insert format (name, islinefilt delta)
                                   amap
                    
          islinefilt x = x == delta_linefilt

getcats :: Connection -> IO [(Int, (String, Int))]
getcats dbh =
    do res <- quickQuery dbh "select term_data.tid, name, parent from term_data, term_hierarchy where term_data.tid = term_hierarchy.tid" []
       return $ map (\[tid, name, parent] -> (fromSql tid,
                                              (fromSql name,
                                               fromSql parent)
                                             )
                    ) res
                                            

{- node notes

node.format matches to filter_formats.format

node.status:
  1: approved
  0: unpublished

node.comment: ignore for now

-}

data Node = 
   Node {nid :: Integer,
         ntitle :: String,
         ntimestamp :: Integer,
         nteaser :: String,
         nbody :: String,
         nisdraft :: Char,
         nmodified :: Integer,
         disablenl2br :: Bool,
         readcount :: Integer}
                  
getNodes :: Connection -> [(String, (String, Bool))] -> IO [Node]
getNodes dbh filters =
    do res <- quickQuery dbh ("SELECT node.nid, title, status, created, " ++ 
              "teaser, body, changed, format, totalcount " ++
              "FROM node, node_counter " ++
              "WHERE node.nid = node_counter.nid AND " ++
              "type = 'story' ORDER BY node.nid asc") []
       return $ map convres res
    where convres [inid, ititle, istatus, icreated, iteaser, ibody,
                   ichanged, iformat, itotalcount] =
              Node {nid = fromSql inid,
                    ntitle = fromSql ititle,
                    ntimestamp = fromSql icreated,
                    nteaser = fromSql iteaser,
                    nbody = fromSql ibody,
                    nisdraft = case (fromSql istatus) of
                                 '1' -> 'f'
                                 _ -> 't',
                    nmodified = fromSql ichanged,
                    disablenl2br = case lookup (fromSql iformat) filters of
                                     Nothing -> False
                                     Just (_, True) -> False
                                     _ -> True,
                    readcount = fromSql itotalcount}

getNodeCats :: Connection -> IO [(Integer, Integer)]
getNodeCats dbh =
    do res <- quickQuery dbh "SELECT DISTINCT nid, tid FROM term_node ORDER BY nid ASC" []
       return $ map (\[nid, tid] -> (fromSql nid, fromSql tid)) res

data Comment =
   Comment {cid :: Integer,     -- Comment ID
            cnid :: Integer,     -- Article ID
            pid :: Integer,     -- Comment's parent comment, or 0 if none
            subject :: String,
            cbody :: String,
            cip :: String,
            ctimestamp :: Integer,
            -- look at status, users, thread
            cname :: String,
            cmail :: String,
            curl :: String,
            cdisablenl2br :: Bool}

getComments :: Connection -> [(String, (String, Bool))] -> IO [Comment]
getComments dbh filters =
    do res <- quickQuery dbh ("SELECT cid, nid, pid, subject, comment, hostname, timestamp, name, mail, homepage, format FROM comments WHERE status = 0 ORDER BY cid ASC") []
       return $ map convcomment res
    where convcomment [icid, inid, ipid, isubject, icomment, iip, itimestamp,
                       iname, imail, ihomepage, iformat] =
             Comment {cid = fromSql icid,
                      cnid = fromSql inid,
                      pid = fromSql ipid,
                      subject = fromSql isubject,
                      cbody = fromSql icomment,
                      cip = fromSql iip,
                      ctimestamp = fromSql itimestamp,
                      cname = fromSql iname,
                      cmail = fromSql imail,
                      curl = fromSql ihomepage,
                      cdisablenl2br = case lookup (fromSql iformat) filters of
                                       Nothing -> False
                                       Just (_, True) -> False
                                       _ -> True
                  }