*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.08.2020 at 20:42:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCTE_T001_V.....................................*
TABLES: ZCTE_T001_V, *ZCTE_T001_V. "view work areas
CONTROLS: TCTRL_ZCTE_T001_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZCTE_T001_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZCTE_T001_V.
* Table for entries selected to show on screen
DATA: BEGIN OF ZCTE_T001_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZCTE_T001_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCTE_T001_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZCTE_T001_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZCTE_T001_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCTE_T001_V_TOTAL.

*.........table declarations:.................................*
TABLES: ZCTE_T001                      .
