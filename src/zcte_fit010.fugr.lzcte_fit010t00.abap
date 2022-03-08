*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.08.2020 at 19:52:04
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCTE_FIT010.....................................*
DATA:  BEGIN OF STATUS_ZCTE_FIT010                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCTE_FIT010                   .
CONTROLS: TCTRL_ZCTE_FIT010
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCTE_FIT010                   .
TABLES: ZCTE_FIT010                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
