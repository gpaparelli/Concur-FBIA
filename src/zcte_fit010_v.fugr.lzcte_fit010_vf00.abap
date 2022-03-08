*---------------------------------------------------------------------*
*    view related FORM routines
*   generation date: 04.08.2020 at 20:58:52
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCTE_FIT010_V...................................*
FORM GET_DATA_ZCTE_FIT010_V.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZCTE_FIT010 WHERE
(VIM_WHERETAB) .
    CLEAR ZCTE_FIT010_V .
ZCTE_FIT010_V-MANDT =
ZCTE_FIT010-MANDT .
ZCTE_FIT010_V-ENTITY_ID =
ZCTE_FIT010-ENTITY_ID .
ZCTE_FIT010_V-ACCOUNT_ID =
ZCTE_FIT010-ACCOUNT_ID .
<VIM_TOTAL_STRUC> = ZCTE_FIT010_V.
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
FORM DB_UPD_ZCTE_FIT010_V .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZCTE_FIT010_V.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZCTE_FIT010_V-ST_DELETE EQ GELOESCHT.
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
  SELECT SINGLE FOR UPDATE * FROM ZCTE_FIT010 WHERE
  ENTITY_ID = ZCTE_FIT010_V-ENTITY_ID AND
  ACCOUNT_ID = ZCTE_FIT010_V-ACCOUNT_ID .
    IF SY-SUBRC = 0.
    DELETE ZCTE_FIT010 .
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
  SELECT SINGLE FOR UPDATE * FROM ZCTE_FIT010 WHERE
  ENTITY_ID = ZCTE_FIT010_V-ENTITY_ID AND
  ACCOUNT_ID = ZCTE_FIT010_V-ACCOUNT_ID .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZCTE_FIT010.
    ENDIF.
ZCTE_FIT010-MANDT =
ZCTE_FIT010_V-MANDT .
ZCTE_FIT010-ENTITY_ID =
ZCTE_FIT010_V-ENTITY_ID .
ZCTE_FIT010-ACCOUNT_ID =
ZCTE_FIT010_V-ACCOUNT_ID .
    IF SY-SUBRC = 0.
    UPDATE ZCTE_FIT010 .
    ELSE.
    INSERT ZCTE_FIT010 .
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
CLEAR: STATUS_ZCTE_FIT010_V-UPD_FLAG,
STATUS_ZCTE_FIT010_V-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ZCTE_FIT010_V.
  SELECT SINGLE * FROM ZCTE_FIT010 WHERE
ENTITY_ID = ZCTE_FIT010_V-ENTITY_ID AND
ACCOUNT_ID = ZCTE_FIT010_V-ACCOUNT_ID .
ZCTE_FIT010_V-MANDT =
ZCTE_FIT010-MANDT .
ZCTE_FIT010_V-ENTITY_ID =
ZCTE_FIT010-ENTITY_ID .
ZCTE_FIT010_V-ACCOUNT_ID =
ZCTE_FIT010-ACCOUNT_ID .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZCTE_FIT010_V USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZCTE_FIT010_V-ENTITY_ID TO
ZCTE_FIT010-ENTITY_ID .
MOVE ZCTE_FIT010_V-ACCOUNT_ID TO
ZCTE_FIT010-ACCOUNT_ID .
MOVE ZCTE_FIT010_V-MANDT TO
ZCTE_FIT010-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZCTE_FIT010'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZCTE_FIT010 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZCTE_FIT010'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
