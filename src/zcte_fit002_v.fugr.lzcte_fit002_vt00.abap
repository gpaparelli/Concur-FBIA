*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 04.08.2020 at 20:41:16
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCTE_FIT002_V...................................*
TABLES: ZCTE_FIT002_V, *ZCTE_FIT002_V. "view work areas
CONTROLS: TCTRL_ZCTE_FIT002_V
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZCTE_FIT002_V. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZCTE_FIT002_V.
* Table for entries selected to show on screen
DATA: BEGIN OF ZCTE_FIT002_V_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZCTE_FIT002_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCTE_FIT002_V_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZCTE_FIT002_V_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZCTE_FIT002_V.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCTE_FIT002_V_TOTAL.

*.........table declarations:.................................*
TABLES: ZCTE_FIT002                    .
