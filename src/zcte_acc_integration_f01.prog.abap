*&---------------------------------------------------------------------*
*&  Include           ZCTE_ACC_INTEGRATION_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_DOCUMENTHEADER_FISCYEAR
*&---------------------------------------------------------------------*

FORM f_documentheader_fisc_year USING    pu_cols TYPE zcte_fis002
                                         pu_exec TYPE i
                               CHANGING pc_value.


  DATA: lv_date  TYPE dats,
        lv_bukrs TYPE bukrs.

  IF NOT pu_cols-col026 IS INITIAL.
     CONCATENATE pu_cols-col026(4)
                 pu_cols-col026+5(2)
                 pu_cols-col026+8(2)
            INTO lv_date.
  ELSE.
     CONCATENATE pu_cols-col183(4)
                 pu_cols-col183+5(2)
                 pu_cols-col183+8(2)
            INTO lv_date.
  ENDIF.

  IF NOT pu_cols-col192 IS INITIAL.
    lv_bukrs = pu_cols-col192.
  ELSE.
    lv_bukrs = pu_cols-col011.
  ENDIF.

  IF NOT lv_bukrs IS INITIAL AND
     NOT lv_date IS INITIAL.
    CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
       BUKRS         = lv_bukrs
       DATE          = lv_date
     IMPORTING
       CURRY         = pc_value.
  ENDIF.


*  pc_value = pu_cols-026(4).


ENDFORM.                    " F_DOCUMENTHEADER_FISCYEAR

*&---------------------------------------------------------------------*
*&      Form  F_DOCUMENTHEADER_FIS_PERIOD
*&---------------------------------------------------------------------*

FORM f_documentheader_fis_period USING    pu_cols TYPE zcte_fis002
                                          pu_exec TYPE i
                                 CHANGING pc_value.


  DATA: lv_date  TYPE dats,
        lv_bukrs TYPE bukrs.

  IF NOT pu_cols-col026 IS INITIAL.
     CONCATENATE pu_cols-col026(4)
                 pu_cols-col026+5(2)
                 pu_cols-col026+8(2)
            INTO lv_date.
  ELSE.
     CONCATENATE pu_cols-col183(4)
                 pu_cols-col183+5(2)
                 pu_cols-col183+8(2)
            INTO lv_date.
  ENDIF.

  IF NOT pu_cols-col192 IS INITIAL.
    lv_bukrs = pu_cols-col192.
  ELSE.
    lv_bukrs = pu_cols-col011.
  ENDIF.

  IF NOT lv_bukrs IS INITIAL AND
     NOT lv_date IS INITIAL.

    CALL FUNCTION 'GET_CURRENT_YEAR'
     EXPORTING
       BUKRS         = lv_bukrs
       DATE          = lv_date
     IMPORTING
       CURRM         = pc_value.
  ENDIF.


*  pc_value = pu_cols-026(4).


ENDFORM.                    " F_DOCUMENTHEADER_PERIOD

*&---------------------------------------------------------------------*
*&      Form  F_CURRENCYAMOUNT_AMT_DOCCUR
*&---------------------------------------------------------------------*

FORM f_currencyamount_amt_doccur USING    pu_cols TYPE zcte_fis002
                                          pu_exec TYPE i
                                 CHANGING pc_value.

  pc_value = pu_cols-col169+1.
  IF pu_cols-col169(1) EQ '-'.
     pc_value = ABS( pc_value ) * -1.
  ENDIF.

  IF pu_exec EQ 2.
     MULTIPLY pc_value BY -1.  " Always invert signal on second run
  ENDIF.

ENDFORM.                    " F_CURRENCYAMOUNT_AMT_DOCCUR

*&---------------------------------------------------------------------*
*&      Form  F_ACCOUNTGL_COSTCENTER
*&---------------------------------------------------------------------*

FORM f_accountgl_costcenter USING    pu_cols TYPE zcte_fis002
                                     pu_exec TYPE i
                            CHANGING pc_value.

  IF pu_cols-col193 EQ 'CC'.
     CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
         input         = pu_cols-col194
       IMPORTING
         OUTPUT        = pc_value.

  ENDIF.

ENDFORM.                    " F_ACCOUNTGL_COSTCENTER

*&---------------------------------------------------------------------*
*&      Form  F_ACCOUNTGL_WBS_ELEMENT
*&---------------------------------------------------------------------*

FORM f_accountgl_wbs_element USING    pu_cols TYPE zcte_fis002
                                      pu_exec TYPE i
                            CHANGING pc_value.

  IF pu_cols-col193 EQ 'PJ'.
     pc_value = pu_cols-col194.
  ENDIF.

ENDFORM.                    " F_ACCOUNTGL_WBS_ELEMENT

*&---------------------------------------------------------------------*
*&      Form  F_ACCOUNTGL_ORDERID
*&---------------------------------------------------------------------*

FORM f_accountgl_orderid USING    pu_cols TYPE zcte_fis002
                                  pu_exec TYPE i
                         CHANGING pc_value.

  IF pu_cols-col193 EQ 'IO'.
     pc_value = pu_cols-col194.
  ENDIF.

ENDFORM.                    " F_ACCOUNTGL_ORDERID

*&---------------------------------------------------------------------*
*&      Form  F_ACCOUNTGL_GL_ACCOUNT
*&---------------------------------------------------------------------*

FORM f_accountgl_gl_account USING    pu_cols TYPE zcte_fis002
                                     pu_exec TYPE i
                         CHANGING pc_value.

*  IF pu_cols-col185 EQ '2'.
*     pc_value = '0000213203'.
*  ELSE.
     pc_value = pu_cols-col167.
*  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input         = pc_value
   IMPORTING
      OUTPUT        = pc_value.



ENDFORM.                    " F_ACCOUNTGL_GL_ACCOUNT
