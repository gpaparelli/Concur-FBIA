*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 21.08.2020 at 16:52:48
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCTE_T002.......................................*
DATA:  BEGIN OF STATUS_ZCTE_T002                     .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCTE_T002                     .
CONTROLS: TCTRL_ZCTE_T002
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCTE_T002                     .
TABLES: ZCTE_T002                      .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
