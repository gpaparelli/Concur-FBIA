*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 01.08.2020 at 02:08:38
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCTE_FIT002.....................................*
DATA:  BEGIN OF STATUS_ZCTE_FIT002                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCTE_FIT002                   .
CONTROLS: TCTRL_ZCTE_FIT002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCTE_FIT002                   .
TABLES: ZCTE_FIT002                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
