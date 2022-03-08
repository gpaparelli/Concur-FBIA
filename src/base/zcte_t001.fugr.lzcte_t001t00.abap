*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 31.07.2020 at 21:06:57
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCTE_T001.......................................*
DATA:  BEGIN OF STATUS_ZCTE_T001                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCTE_T001                     .
CONTROLS: TCTRL_ZCTE_T001
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCTE_T001                     .
TABLES: ZCTE_T001                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
