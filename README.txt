PREREQUISITES
-------------

* ghc 6.4.1 (may work with Hugs; untested)
* HDBC 0.99.2 or above
* HDBC-postgresql and HDBC-odbc 0.99.2.1 or above
* MissingH 0.13.0 or above

ASSUMPTIONS
-----------

Serendipity is installed and configured.

Serendipity is using PostgreSQL.  Drupal is using MySQL.

There are no categories, comments, or stories in the system.

Configured plugins:
 Karma
 NL2BR
 Extended properties for entries
 
HOWTO
-----

1) Edit blogcvt.hs and replace jgoerzen with the appropriate author
2) Create a blogcvt.ini
3) make
4) dist/build/blogcvt
5) Happiness!

AFTER CONVERSION
----------------

Pull up admin interface:

1. Add and remove some random category

2. Go into permalinks and change any scheme (you can change it back
   later)

