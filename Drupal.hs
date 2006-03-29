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
