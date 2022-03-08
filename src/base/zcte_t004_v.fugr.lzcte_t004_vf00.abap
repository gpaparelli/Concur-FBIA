*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 05.01.2021 at 14:48:06
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCTE_T004_V.....................................*
FORM GET_DATA_ZCTE_T004_V.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZCTE_T004 WHERE
(VIM_WHERETAB) .
    CLEAR ZCTE_T004_V .
ZCTE_T004_V-MANDT =
ZCTE_T004-MANDT .
ZCTE_T004_V-ENTITY_ID =
ZCTE_T004-ENTITY_ID .
ZCTE_T004_V-TAX_FIELD =
ZCTE_T004-TAX_FIELD .
ZCTE_T004_V-COL_EXP =
ZCTE_T004-COL_EXP .
ZCTE_T004_V-COL_CASHADV =
ZCTE_T004-COL_CASHADV .
ZCTE_T004_V-COL_INV =
ZCTE_T004-COL_INV .
<VIM_TOTAL_STRUC> = ZCTE_T004_V.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZCTE_T004_V .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZCTE_T004_V.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZCTE_T004_V-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZCTE_T004 WHERE
  ENTITY_ID = ZCTE_T004_V-ENTITY_ID AND
  TAX_FIELD = ZCTE_T004_V-TAX_FIELD .
    IF SY-SUBRC = 0.
    DELETE ZCTE_T004 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZCTE_T004 WHERE
  ENTITY_ID = ZCTE_T004_V-ENTITY_ID AND
  TAX_FIELD = ZCTE_T004_V-TAX_FIELD .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZCTE_T004.
    ENDIF.
ZCTE_T004-MANDT =
ZCTE_T004_V-MANDT .
ZCTE_T004-ENTITY_ID =
ZCTE_T004_V-ENTITY_ID .
ZCTE_T004-TAX_FIELD =
ZCTE_T004_V-TAX_FIELD .
ZCTE_T004-COL_EXP =
ZCTE_T004_V-COL_EXP .
ZCTE_T004-COL_CASHADV =
ZCTE_T004_V-COL_CASHADV .
ZCTE_T004-COL_INV =
ZCTE_T004_V-COL_INV .
    IF SY-SUBRC = 0.
    UPDATE ZCTE_T004 .
    ELSE.
    INSERT ZCTE_T004 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZCTE_T004_V-UPD_FLAG,
STATUS_ZCTE_T004_V-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZCTE_T004_V.
  SELECT SINGLE * FROM ZCTE_T004 WHERE
ENTITY_ID = ZCTE_T004_V-ENTITY_ID AND
TAX_FIELD = ZCTE_T004_V-TAX_FIELD .
ZCTE_T004_V-MANDT =
ZCTE_T004-MANDT .
ZCTE_T004_V-ENTITY_ID =
ZCTE_T004-ENTITY_ID .
ZCTE_T004_V-TAX_FIELD =
ZCTE_T004-TAX_FIELD .
ZCTE_T004_V-COL_EXP =
ZCTE_T004-COL_EXP .
ZCTE_T004_V-COL_CASHADV =
ZCTE_T004-COL_CASHADV .
ZCTE_T004_V-COL_INV =
ZCTE_T004-COL_INV .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZCTE_T004_V USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZCTE_T004_V-ENTITY_ID TO
ZCTE_T004-ENTITY_ID .
MOVE ZCTE_T004_V-TAX_FIELD TO
ZCTE_T004-TAX_FIELD .
MOVE ZCTE_T004_V-MANDT TO
ZCTE_T004-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZCTE_T004'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZCTE_T004 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZCTE_T004'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
