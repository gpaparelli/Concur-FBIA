*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZCTE_FIT002
*   generation date: 01.08.2020 at 02:08:37
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZCTE_FIT002        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
