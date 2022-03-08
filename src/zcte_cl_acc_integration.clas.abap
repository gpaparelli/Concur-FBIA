class ZCTE_CL_ACC_INTEGRATION definition
  public
  final
  create public .

public section.
*"* public components of class ZCTE_CL_ACC_INTEGRATION
*"* do not include other source files here!!!

  class-data C_EXPENSE type ZCTEFI_REPORT_TYPE value 'P'. "#EC NOTEXT .
  class-data C_CASHADV type ZCTEFI_REPORT_TYPE value 'A'. "#EC NOTEXT .

  class-methods GET_GT_COLS_FROM_DB
    importing
      !IM_V_ENTITY type ZCTE_ENTITY_ID
      !IM_V_REPORT_TYPE type ZCTEFI_REPORT_TYPE
      !IM_V_REPORT_KEY type ZCTEFI_REPORT_KEY
    exporting
      !EX_I_COLS type ZCTE_FIS002_T .
  type-pools ABAP .
  methods SET_GT_COLS_FROM_FILE
    importing
      !IM_V_REPRO type ABAP_BOOL
      !IM_V_SAVE_TO_DB type ABAP_BOOL
      !IM_I_FILE type ANY TABLE
    changing
      !CH_CL_LOG_HANDLE type ref to ZCTE_CL_LOG_HANDLE
    exceptions
      IMPORT_ERROR .
  methods SET_GT_COLS_FROM_DB
    importing
      !IM_V_REPORT_TYPE type ZCTEFI_REPORT_TYPE
      !IM_V_REPORT_ID type CHAR32
      !IM_V_REPORT_KEY type ZCTEFI_REPORT_KEY
      !IM_V_BATCH_DATE type CHAR10
      !IM_V_BATCH_ID type CHAR13
    exporting
      !EX_O_LOGHANDLE type ref to ZCTE_CL_LOG_HANDLE
    exceptions
      INTERNAL_ERROR .
  methods PROCESS_EXTRACT_DOCUMENTS
    importing
      !IM_V_MANUAL_PROC type ABAP_BOOL
    exporting
      !EX_I_LOG_HANDLE type ZCTE_S003_T
    exceptions
      ENTITY_NOT_INITIALIZED .
  methods SET_ENTITY_ID
    importing
      !IM_V_ENTITY type ZCTE_ENTITY_ID
    exceptions
      NOT_FOUND .
protected section.
*"* protected components of class ZCTE_CL_ACC_INTEGRATION
*"* do not include other source files here!!!
private section.
*"* private components of class ZCTE_CL_ACC_INTEGRATION
*"* do not include other source files here!!!

  types:
    BEGIN OF ty_cols_header,
           constant(7)        TYPE c,
           batch_date(10)     TYPE c,
           record(10)         TYPE c,
           j_amount_total(23) TYPE c,
           batch_id(13)       TYPE c,
       END OF ty_cols_header .
  types:
    BEGIN OF ty_report_key,
         report_type        TYPE zcte_fit003-report_type,
         report_key         TYPE zcte_fit003-report_key,
       END OF ty_report_key .
  types TY_COLS type ZCTE_FIS002 .
  types TY_T_COLS type ZCTE_FIS002_T .
  types:
    ty_t_zcte_fit002 TYPE TABLE OF zcte_fit002 .
  types:
    ty_t_zcte_fit008 TYPE TABLE OF zcte_fit008 .
  types:
    ty_t_zcte_fit009 TYPE TABLE OF zcte_fit009 .

  data GT_COLS type ZCTE_FIS002_T .
  data:
    gt_zcte_fit003   TYPE TABLE OF zcte_fit003 .
  data:
    gt_zcte_fit005   TYPE TABLE OF zcte_fit005 .
  data GS_COLS_HEADER type TY_COLS_HEADER .
  data GS_COLS type ZCTE_FIS002 .
  data GS_ZCTE_FIT001 type ZCTE_FIT001 .
  data V_MESSAGE type BAPI_MSG .
  data V_LOGICAL_SYSTEM type TBDLS-LOGSYS .
  constants C_PIPE type C value '|'. "#EC NOTEXT
  constants:
    c_extract(7) TYPE c value 'EXTRACT'. "#EC NOTEXT
  constants:
    c_detail(6)  TYPE c value 'DETAIL'. "#EC NOTEXT
  constants C_NEW type ZCTEFI_PMNT_STATUS value 'N'. "#EC NOTEXT
  constants C_NOTPAYABLE type ZCTEFI_PMNT_STATUS value 'O'. "#EC NOTEXT
  data O_ZCTE_UTILS type ref to ZCTE_CL_UTILS .

  methods REPORT_CHECK_CONSISTENCY
    importing
      !IM_S_REPORT_KEY type TY_REPORT_KEY
      !IM_S_COLS type TY_COLS
    changing
      !CH_S_LOG type ZCTE_S003
    exceptions
      CONSISTENCY_ERROR .
  type-pools ABAP .
  methods GET_GLOBAL_DATA
    importing
      !IM_V_REPRO type ABAP_BOOL
    changing
      !CH_CL_LOG_HANDLE type ref to ZCTE_CL_LOG_HANDLE
    exceptions
      INTERNAL_ERROR .
  methods GET_AUX_MAPPING
    exporting
      !EX_I_ZCTE_FIT002 type TY_T_ZCTE_FIT002 .
  methods GET_AUX_ACCOUNT
    exporting
      !EX_I_ZCTE_FIT008 type TY_T_ZCTE_FIT008 .
  methods EXPORT_DOCUMENT_ITEMS_TO_DB
    changing
      !CH_CL_LOG_HANDLE type ref to ZCTE_CL_LOG_HANDLE
    exceptions
      INTERNAL_ERROR .
  methods GET_AUX_TEXTS
    exporting
      !EX_I_ZCTE_FIT009 type TY_T_ZCTE_FIT009 .
  methods UPDATE_BATCH_INFO_TO_DB
    importing
      !IM_V_MANUAL_PROC type ABAP_BOOL
    changing
      !CH_CL_LOG_HANDLE type ref to ZCTE_CL_LOG_HANDLE
    exceptions
      INTERNAL_ERROR .
  methods BUILD_REPORT_KEY
    importing
      !IM_S_COLS type TY_COLS
    exporting
      !EX_S_REPORT_KEY type TY_REPORT_KEY .
  methods CREATE_BAL_LOG
    importing
      !IM_S_REPORT_KEY type TY_REPORT_KEY
    exporting
      !EX_S_LOG type ZCTE_S003 .
  methods CHECK_MODIFIED_FIELD
    importing
      !IM_S_REPORT_KEY type TY_REPORT_KEY
    changing
      !CH_I_COLS type TY_T_COLS
      !CH_S_LOG type ZCTE_S003 .
  methods FILL_DOCUMENT_HEADER
    importing
      !IM_S_COLS type TY_COLS
      !IM_I_ZCTE_FIT002 type TY_T_ZCTE_FIT002
    exporting
      !EX_S_DOCUMENTHEADER type BAPIACHE09
    changing
      !CH_S_LOG type ZCTE_S003
      !CH_V_ERROR type ABAP_BOOL .
  methods FILL_DOCUMENT_ITEM
    importing
      !IM_V_EXEC type I
      !IM_V_ITEMNO type I
      !IM_S_COLS type TY_COLS
      !IM_I_ZCTE_FIT002 type TY_T_ZCTE_FIT002
    exporting
      !EX_I_ACCOUNTGL type BAPIACGL09_TAB
      !EX_I_ACCOUNTPAYABLE type BAPIACAP09_TAB
      !EX_I_CURRENCYAMOUNT type BAPIACCR09_TAB
    changing
      !CH_S_LOG type ZCTE_S003
      !CH_V_ERROR type ABAP_BOOL .
  methods PROCESS_AND_ADJUST_FIELD
    importing
      !IM_V_VALUE type ANY
    changing
      !CH_V_VALUE type ANY .
  methods TRANSLATE_TEXTS
    importing
      !IM_I_ZCTE_FIT009 type TY_T_ZCTE_FIT009
    changing
      !CH_S_COLS type TY_COLS
      !CH_S_LOG type ZCTE_S003 .
  methods CALL_BAPI_POSTING
    importing
      !IM_S_COLS type TY_COLS
      !IM_S_DOCUMENTHEADER type BAPIACHE09
      !IM_I_ACCOUNTGL type BAPIACGL09_TAB
      !IM_I_ACCOUNTPAYABLE type BAPIACAP09_TAB
      !IM_I_CURRENCYAMOUNT type BAPIACCR09_TAB
    changing
      !CH_S_LOG type ZCTE_S003 .
  methods UPDATE_DOCUMENTS_HEADER
    importing
      !IM_S_COLS type TY_COLS
      !IM_V_MSGV2 type SYMSGV
      !IM_V_PMNT_STATUS type ZCTEFI_PMNT_STATUS .
  methods UPDATE_POSTINGS_TO_DB
    importing
      !IM_V_MANUAL_PROC type ABAP_BOOL
      !IM_I_LOG_HANDLE type ZCTE_S003_T
    exceptions
      INTERNAL_ERROR .
ENDCLASS.



CLASS ZCTE_CL_ACC_INTEGRATION IMPLEMENTATION.


method BUILD_REPORT_KEY.

  IF im_s_cols-col185 EQ '1'."Adiantamento
     ex_s_report_key-report_type    = c_cashadv.
     ex_s_report_key-report_key     = im_s_cols-col187.
  ELSE. "Prestação
     ex_s_report_key-report_type    = c_expense.
     ex_s_report_key-report_key     = im_s_cols-col020.
  ENDIF.

endmethod.


method CALL_BAPI_POSTING.


  DATA: lv_obj_type     TYPE bapiache09-obj_type,
        lv_obj_key      TYPE bapiache09-obj_key,
        lv_obj_sys      TYPE bapiache09-obj_sys,
        lv_msgv2        TYPE bapiret2-message_v2,
        lv_pmnt_status  TYPE zctefi_pmnt_status,
        lv_lines        TYPE i.

  DATA: lt_return         TYPE TABLE OF bapiret2,
        lt_accountpayable TYPE bapiacap09_tab,
        lt_accountgl      TYPE bapiacgl09_tab,
        lt_currencyamount TYPE bapiaccr09_tab.

  DATA: ls_currencyamount   TYPE bapiaccr09,
        ls_currency_collect TYPE bapiaccr09,
        ls_accountpayable   TYPE bapiacap09,
        ls_return           TYPE bapiret2,
        ls_message          TYPE zcte_s002.

  DATA lt_currency_collect TYPE TABLE OF bapiaccr09.

  lt_accountpayable[] = im_i_accountpayable[].
  lt_accountgl[]      = im_i_accountgl[].
  lt_currencyamount[] = im_i_currencyamount[].


  LOOP AT lt_currencyamount[] INTO ls_currencyamount.
    CLEAR ls_currencyamount-itemno_acc.
    COLLECT ls_currencyamount INTO lt_currency_collect.
  ENDLOOP.

  READ TABLE lt_currency_collect INDEX 1 INTO ls_currency_collect.
  IF sy-subrc IS INITIAL.

    IF ls_currency_collect-amt_doccur  = '0.0000'.
      REFRESH lt_accountpayable[].

    ELSE.
      CLEAR ls_currencyamount.
      READ TABLE lt_currencyamount[] INTO ls_currencyamount INDEX 1.
      IF sy-subrc IS INITIAL.
        ls_currencyamount-amt_doccur = ls_currency_collect-amt_doccur.
        IF ls_currencyamount-amt_doccur <> 0.
          ls_currencyamount-amt_doccur = ls_currencyamount-amt_doccur * -1.
        ENDIF.

        DESCRIBE TABLE lt_currencyamount LINES lv_lines.

        ls_currencyamount-itemno_acc = lv_lines + 1.
        APPEND ls_currencyamount TO lt_currencyamount[].

        READ TABLE lt_accountpayable[] INTO ls_accountpayable INDEX 1.
        IF sy-subrc IS INITIAL.

          ls_accountpayable-itemno_acc = lv_lines + 1.
          REFRESH lt_accountpayable[].
          APPEND ls_accountpayable TO lt_accountpayable[].

        ENDIF." READ TABLE im_i_accountpayable[] INTO ls_accountpayable INDEX 1.
      ENDIF." READ TABLE im_i_currencyamount[] INTO ls_currencyamount INDEX 1.
    ENDIF." IF ls_currency_collect-amt_doccur   = '0.0000'.
  ENDIF." READ TABLE lt_currency_collect INDEX 1 INTO ls_currency_collect.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = im_s_documentheader
    IMPORTING
      obj_type       = lv_obj_type
      obj_key        = lv_obj_key
      obj_sys        = lv_obj_sys
    TABLES
      accountgl      = lt_accountgl[]
      accountpayable = lt_accountpayable[]
      currencyamount = lt_currencyamount[]
      return         = lt_return[].

  READ TABLE lt_return INTO ls_return WITH KEY type = 'S'
                                                 id = 'RW'
                                             number = '605'.
  IF sy-subrc IS INITIAL.

*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = abap_true.

    lv_msgv2 = ls_return-message_v2.

  ENDIF.

  LOOP AT lt_return INTO ls_return.

    CLEAR ls_message.
    ls_message-msgty  = ls_return-type.
    ls_message-msgid  = ls_return-id.
    ls_message-msgno  = ls_return-number.
    ls_message-msgv1  = ls_return-message_v1.
    ls_message-msgv2  = ls_return-message_v2.
    ls_message-msgv3  = ls_return-message_v3.
    ls_message-msgv4  = ls_return-message_v4.

    ch_s_log-log_obj->add_message(
      EXPORTING
         im_s_message = ls_message ).

  ENDLOOP.

  IF NOT lt_accountpayable[] IS INITIAL.
     lv_pmnt_status = c_new. " New
  ELSE.
     lv_pmnt_status = c_notpayable. " Not payable
  ENDIF.

  me->update_documents_header(
    EXPORTING
      im_s_cols        = im_s_cols
      im_v_msgv2       = lv_msgv2
      im_v_pmnt_status = lv_pmnt_status ).


endmethod.


method CHECK_MODIFIED_FIELD.

  DATA: lt_messages TYPE zcte_s002_t.

  DATA: lv_entry_id    TYPE zctefi_entry_id,
        lv_journal_key TYPE zctefi_journal_key,
        lv_alloc_key   TYPE zctefi_alloc_key.
  DATA: lv_repkey      TYPE pclkey.

  FIELD-SYMBOLS: <l_fs_zcte_fit003> TYPE zcte_fit003,
                 <l_fs_zcte_fit005> TYPE zcte_fit005,
                 <l_fs_cols>        TYPE ty_cols,
                 <l_fs_message>     TYPE zcte_s002.

  lv_repkey = im_s_report_key.

  READ TABLE gt_zcte_fit003 ASSIGNING <l_fs_zcte_fit003>
                            WITH KEY  report_key  = im_s_report_key-report_key
                                      report_type = im_s_report_key-report_type BINARY SEARCH.
  IF sy-subrc EQ 0.

     LOOP AT ch_i_cols ASSIGNING <l_fs_cols>.

       lv_entry_id     = <l_fs_cols>-col061.
       lv_journal_key  = <l_fs_cols>-col170.
       lv_alloc_key    = <l_fs_cols>-col189.

       READ TABLE gt_zcte_fit005 WITH KEY  report_id   =  <l_fs_zcte_fit003>-report_id
                                           report_key  =  <l_fs_zcte_fit003>-report_key
                                           report_type =  <l_fs_zcte_fit003>-report_type
                                           batch_id    =  <l_fs_zcte_fit003>-batch_id
                                           entry_id    =  lv_entry_id
                                           journal_key =  lv_journal_key
                                 ASSIGNING <l_fs_zcte_fit005> BINARY SEARCH.

       IF sy-subrc EQ 0.

          IF <l_fs_zcte_fit005>-vendor_id_new NE <l_fs_cols>-col057.

             MESSAGE s014 WITH <l_fs_cols>-col057 <l_fs_zcte_fit005>-vendor_id_new INTO v_message.
             APPEND INITIAL LINE TO lt_messages ASSIGNING <l_fs_message>.
             <l_fs_message> = ch_s_log-log_obj->capture_message( ).
             <l_fs_cols>-col057 = <l_fs_zcte_fit005>-vendor_id_new.

          ENDIF.

          IF <l_fs_zcte_fit005>-vendor_id_new NE <l_fs_cols>-col282.

             MESSAGE s014 WITH <l_fs_cols>-col282 <l_fs_zcte_fit005>-vendor_id_new INTO v_message.
             APPEND INITIAL LINE TO lt_messages ASSIGNING <l_fs_message>.
             <l_fs_message> = ch_s_log-log_obj->capture_message( ).
             <l_fs_cols>-col282 = <l_fs_zcte_fit005>-vendor_id_new.

          ENDIF.

          IF <l_fs_zcte_fit005>-account_new NE <l_fs_cols>-col167.

             MESSAGE s015 WITH <l_fs_cols>-col167 <l_fs_zcte_fit005>-account_new INTO v_message.
             APPEND INITIAL LINE TO lt_messages ASSIGNING <l_fs_message>.
             <l_fs_message> = ch_s_log-log_obj->capture_message( ).
             <l_fs_cols>-col167 = <l_fs_zcte_fit005>-account_new.

          ENDIF.

        ENDIF.

        READ TABLE gt_zcte_fit005 WITH KEY  report_id   =  <l_fs_zcte_fit003>-report_id
                                            report_key  =  <l_fs_zcte_fit003>-report_key
                                            report_type =  <l_fs_zcte_fit003>-report_type
                                            batch_id    =  <l_fs_zcte_fit003>-batch_id
                                            entry_id    =  lv_entry_id
                                            journal_key =  lv_journal_key
                                            alloc_key   =  lv_alloc_key
                                  ASSIGNING <l_fs_zcte_fit005> BINARY SEARCH.

         IF sy-subrc EQ 0.

           IF <l_fs_zcte_fit005>-cost_obj_tp_new NE <l_fs_cols>-col195.

             MESSAGE s016 WITH <l_fs_cols>-col195 <l_fs_zcte_fit005>-cost_obj_tp_new INTO v_message.
             APPEND INITIAL LINE TO lt_messages ASSIGNING <l_fs_message>.
             <l_fs_message> = ch_s_log-log_obj->capture_message( ).
             <l_fs_cols>-col195 = <l_fs_zcte_fit005>-cost_obj_tp_new.

           ENDIF.

           IF <l_fs_zcte_fit005>-cost_obj_new NE <l_fs_cols>-col196.

              MESSAGE s017 WITH <l_fs_cols>-col196 <l_fs_zcte_fit005>-cost_obj_new INTO v_message.
              APPEND INITIAL LINE TO lt_messages ASSIGNING <l_fs_message>.
              <l_fs_message> = ch_s_log-log_obj->capture_message( ).
              <l_fs_cols>-col196 = <l_fs_zcte_fit005>-cost_obj_new.

           ENDIF.
        ENDIF.

     ENDLOOP.
  ENDIF.

  IF NOT lt_messages IS INITIAL.

    MESSAGE s011 WITH <l_fs_zcte_fit003>-report_key INTO v_message.
    ch_s_log-log_obj->capture_and_add_message( ).

    SORT lt_messages.
    delete ADJACENT DUPLICATES FROM lt_messages.

    LOOP AT lt_messages ASSIGNING <l_fs_message>.
        ch_s_log-log_obj->add_message(
          EXPORTING
              im_s_message = <l_fs_message> ).
    ENDLOOP.

  ENDIF.

endmethod.


method CREATE_BAL_LOG.

  DATA  l_s_context   TYPE ZCTE_FIS001.
  DATA: l_v_extnumber TYPE  bal_s_log-extnumber.

  l_v_extnumber = im_s_report_key.
  ex_s_log-extnumber = l_v_extnumber.

  CREATE OBJECT ex_s_log-log_obj
    EXPORTING
      im_object    = 'ZCTE'
      im_subobject = 'ZCTE01'
      im_extnumber = l_v_extnumber
    EXCEPTIONS
      not_created  = 1
      OTHERS       = 2.

  IF sy-subrc EQ 0.

    l_s_context-report_type = im_s_report_key-report_type.
    l_s_context-report_id   = im_s_report_key-report_key.
    SHIFT l_s_context-report_id LEFT DELETING LEADING '0'.

    CALL METHOD ex_s_log-log_obj->set_object
      EXPORTING
         im_s_context = l_s_context
         im_reftabname = 'ZCTE_FIS001'.

    CALL METHOD ex_s_log-log_obj->set_display_profile( ).

  ENDIF.

endmethod.


method EXPORT_DOCUMENT_ITEMS_TO_DB.

  TYPES:  BEGIN OF l_ty_cols_item,
            entity_id   TYPE zcte_t001-entity_id,
            report_type TYPE zcte_fit003-report_type,
            report_key  TYPE zcte_fit003-report_key,
            line_item   TYPE numc3,
          END OF l_ty_cols_item.

  DATA: lt_cols     TYPE TABLE OF ty_cols,
        lt_cols_aux TYPE TABLE OF ty_cols.

  DATA: ls_cols_item TYPE l_ty_cols_item,
        ls_cols      TYPE ty_cols,
        ls_cols_aux  TYPE ty_cols.

  DATA: lv_item    TYPE numc3,
        lv_message TYPE BAPI_MSG.

  FIELD-SYMBOLS: <l_fs_cols>     TYPE ty_cols,
                 <l_fs_cols_aux> TYPE ty_cols.

  lt_cols[] = gt_cols[].

  LOOP AT lt_cols INTO ls_cols.

    lt_cols_aux[] = lt_cols[].

    IF ls_cols-col020 IS NOT INITIAL. "Report Key
      DELETE lt_cols_aux WHERE col020 <> ls_cols-col020.
      DELETE lt_cols WHERE col020 = ls_cols-col020.

      ls_cols_item-report_key  = ls_cols-col020.
      ls_cols_item-report_type = c_expense.

    ELSEIF ls_cols-col187 IS NOT INITIAL.. " Cash Advance Key
      DELETE lt_cols_aux WHERE col185 <> ls_cols-col185 OR
                               col187 <> ls_cols-col187.

      DELETE lt_cols WHERE col185 = ls_cols-col185 AND
                           col187 = ls_cols-col187 .

*      UNPACK ls_cols-col187 TO ls_cols_item-report_id(15).
      ls_cols_item-report_type = c_cashadv.
      ls_cols_item-report_key   =  ls_cols-col187.

    ENDIF."IF ls_cols-col020 IS NOT INITIAL. "Report Key

    ls_cols_item-entity_id = o_zcte_utils->gs_zcte_t001-entity_id.
    CLEAR lv_item.
    LOOP AT lt_cols_aux INTO ls_cols_aux.

      ADD 1 TO lv_item.
      ls_cols_item-line_item = lv_item.

      TRY.
        EXPORT ls_cols_aux FROM ls_cols_aux
        TO DATABASE zcte_fit004(i0) ID ls_cols_item.

        IF sy-subrc NE 0.
          MESSAGE e007 WITH ls_cols_item-report_key INTO v_message.
          ch_cl_log_handle->capture_and_add_message( ).
          ROLLBACK WORK.
          RAISE internal_error.
        ENDIF.

      CATCH cx_sy_export_buffer_no_memory cx_sy_export_no_shared_memory cx_root.
        MESSAGE e007 WITH ls_cols_item-report_key INTO v_message.
        ch_cl_log_handle->capture_and_add_message( ).
        MESSAGE e010 INTO v_message.
        ch_cl_log_handle->capture_and_add_message( ).
        ROLLBACK WORK.
        RAISE internal_error.
      ENDTRY.

    ENDLOOP.
  ENDLOOP.



endmethod.


method FILL_DOCUMENT_HEADER.

  TYPES: BEGIN OF lty_single_line,
            single(255) TYPE c,
         END OF lty_single_line.

  DATA: ls_zcte_fit002 TYPE zcte_fit002.

  DATA: lv_message     TYPE bapi_msg,
        lv_column_name TYPE name_komp,
        lv_form        TYPE char50,
        lv_col1        TYPE char3,
        lv_col2        TYPE char3.

  DATA: lt_single_line TYPE TABLE OF lty_single_line,
        ls_single_line TYPE lty_single_line.


  LOOP AT im_i_zcte_fit002 INTO ls_zcte_fit002 WHERE bapi_table = 'DOCUMENTHEADER' AND operation <> ''.

    SHIFT ls_zcte_fit002-mapping LEFT DELETING LEADING space.

    FIELD-SYMBOLS: <fs_name_field> TYPE zcte_fit002-bapi_field,
                  <fs_value>       TYPE ANY.


    ASSIGN COMPONENT 'BAPI_FIELD' OF STRUCTURE ls_zcte_fit002 TO <fs_name_field>.
    IF sy-subrc IS INITIAL.

      ASSIGN COMPONENT <fs_name_field> OF STRUCTURE ex_s_documentheader TO <fs_value>.
      IF sy-subrc IS INITIAL.

        FIELD-SYMBOLS <fs_cols_value> TYPE ANY.

        CASE ls_zcte_fit002-operation.
            "--------------------

          WHEN 'A'. "Abap Code

             CONCATENATE 'F'
                         ls_zcte_fit002-bapi_table
                         ls_zcte_fit002-bapi_field
                         INTO lv_form SEPARATED BY '_'.

             TRY.
                PERFORM (LV_FORM) IN PROGRAM ZCTE_ACC_INTEGRATION USING    im_s_cols
                                                                 1
                                                        CHANGING <fs_value> IF FOUND.
             CATCH CX_SY_DYN_CALL_ILLEGAL_FORM.
                MESSAGE e006 WITH lv_form INTO v_message.
                ch_s_log-log_obj->capture_and_add_message( ).
                ch_v_error = abap_true.
             ENDTRY.

          WHEN 'C'. " Column

            CONCATENATE 'COL' ls_zcte_fit002-mapping(3) INTO lv_column_name.
            ASSIGN COMPONENT lv_column_name OF STRUCTURE im_s_cols TO <fs_cols_value>.
            IF sy-subrc IS INITIAL.

              me->process_and_adjust_field(
                EXPORTING
                  im_v_value = <fs_cols_value>
                CHANGING
                  ch_v_value = <fs_value> ).

            ENDIF."ASSIGN COMPONENT lv_column_name
            "--------------------
          WHEN 'F'. " Fixed

            <fs_value> = ls_zcte_fit002-mapping.

            "--------------------
          WHEN 'P'. " First found

            SPLIT ls_zcte_fit002-mapping AT ';' INTO lv_col1
                                                     lv_col2.

              CONCATENATE 'COL' lv_col1 INTO lv_column_name.
              UNASSIGN <fs_cols_value>.
              ASSIGN COMPONENT lv_column_name OF STRUCTURE im_s_cols TO <fs_cols_value>.
              IF <fs_cols_value> IS ASSIGNED AND <fs_cols_value> IS INITIAL.
                 UNASSIGN <fs_cols_value>.
                 CONCATENATE 'COL' lv_col2 INTO lv_column_name.
                 ASSIGN COMPONENT lv_column_name OF STRUCTURE im_s_cols TO <fs_cols_value>.
              ENDIF.


             IF <fs_cols_value> IS ASSIGNED AND NOT
                <fs_cols_value> IS INITIAL.

                me->process_and_adjust_field(
                  EXPORTING
                    im_v_value = <fs_cols_value>
                  CHANGING
                    ch_v_value = <fs_value> ).

            ENDIF. "IF <fs_cols_value> IS ASSIGNED AND NOT <fs_cols_value> IS INITIAL.

            "--------------------
          WHEN 'X'. " Concatenate values

            SPLIT ls_zcte_fit002-mapping AT ';' INTO TABLE lt_single_line.
            LOOP AT lt_single_line INTO ls_single_line.

              IF ls_single_line(1) = '"'.
                REPLACE ALL OCCURRENCES OF '"' IN ls_single_line WITH space.
                CONDENSE ls_single_line NO-GAPS.
                DATA: lv_strlen TYPE i.
                lv_strlen = STRLEN( ls_single_line ).
                CONCATENATE <fs_value> ls_single_line(lv_strlen) INTO <fs_value>.
                CONTINUE.
              ENDIF.

              CONCATENATE 'COL' ls_single_line(3) INTO lv_column_name.
              ASSIGN COMPONENT lv_column_name OF STRUCTURE im_s_cols TO <fs_cols_value>.
              IF sy-subrc IS INITIAL.

                 me->process_and_adjust_field(
                   EXPORTING
                     im_v_value = <fs_cols_value>
                   CHANGING
                     ch_v_value = <fs_value> ).

              ENDIF."ASSIGN COMPONENT lv_column_name
            ENDLOOP."LOOP AT lt_single_line
        ENDCASE.

      ELSE.

        MESSAGE e005 WITH <fs_name_field> 'DOCUMENTHEADER' INTO v_message.
        ch_s_log-log_obj->capture_and_add_message( ).
        ch_v_error = abap_true.

      ENDIF."ASSIGN COMPONENT <fs_name_field>
    ENDIF."ASSIGN COMPONENT 'bapi_field'
  ENDLOOP."LOOP AT im_i_zcte_fit002

  ex_s_documentheader-username = sy-uname.


endmethod.


method FILL_DOCUMENT_ITEM.

  TYPES: BEGIN OF lty_single_line,
            single(255) TYPE c,
         END OF lty_single_line.

  DATA: ls_zcte_fit002 TYPE zcte_fit002.

  DATA: lv_message     TYPE bapi_msg,
        lv_column_name TYPE name_komp,
        lv_form        TYPE char50.

  DATA: lt_single_line TYPE TABLE OF lty_single_line,
        ls_single_line TYPE lty_single_line.

  DATA: ls_accountgl      TYPE bapiacgl09,
        ls_accountpayable TYPE bapiacap09,
        ls_currencyamount TYPE bapiaccr09.


  DATA: lv_dref    TYPE REF TO data,
        lv_struct  TYPE w_tabname,
        lv_tabname TYPE w_tabname.

  DATA lv_bapi_table TYPE zcte_fit002-bapi_table.

  FIELD-SYMBOLS: <fs_struct> TYPE ANY,
                 <fs_table>  TYPE STANDARD TABLE.

  DATA lt_zcte_fit002 TYPE TABLE OF zcte_fit002.

  lt_zcte_fit002[] = im_i_zcte_fit002[].
  DELETE lt_zcte_fit002  WHERE bapi_table = 'DOCUMENTHEADER' OR operation = ''.

  IF im_v_exec EQ 1.
    DELETE lt_zcte_fit002  WHERE doc_type EQ 'C'.
    IF im_s_cols-col126 EQ 'CBCP'.  " Credit card item
      IF  im_s_cols-col068 NE 'Y'.  " not personal
          DELETE lt_zcte_fit002 WHERE bapi_table EQ 'ACCOUNTPAYABLE'.
      ENDIF.
    ENDIF.
  ELSE.
    DELETE lt_zcte_fit002  WHERE doc_type NE 'C'.
  ENDIF.

  LOOP AT lt_zcte_fit002 INTO ls_zcte_fit002.

    IF sy-tabix = 1.
      lv_bapi_table = ls_zcte_fit002-bapi_table.
    ELSE.
      IF lv_bapi_table <> ls_zcte_fit002-bapi_table.
        lv_bapi_table = ls_zcte_fit002-bapi_table.
        FIELD-SYMBOLS <fs_itemno> TYPE ANY.
        ASSIGN COMPONENT 'ITEMNO_ACC' OF STRUCTURE <fs_struct> TO <fs_itemno>.
        <fs_itemno> = im_v_itemno.
        APPEND <fs_struct> TO <fs_table>.
      ENDIF.
    ENDIF.

    SHIFT ls_zcte_fit002-mapping LEFT DELETING LEADING space.

    FIELD-SYMBOLS: <fs_name_field> TYPE zcte_fit002-bapi_field,
                   <fs_value>      TYPE ANY.

    ASSIGN COMPONENT 'BAPI_FIELD' OF STRUCTURE ls_zcte_fit002 TO <fs_name_field>.
    IF sy-subrc IS INITIAL.

*      lv_struct = 'LS_' && ls_zcte_fit002-bapi_table."**COMPONENT SAP_ABA 7.3 or HIGHER
      CONCATENATE 'LS_' ls_zcte_fit002-bapi_table INTO lv_struct.

      TRANSLATE lv_struct TO UPPER CASE.
      ASSIGN (lv_struct) TO <fs_struct>.

      CONCATENATE 'EX_I_' ls_zcte_fit002-bapi_table '[]' INTO lv_tabname.

      TRANSLATE lv_tabname TO UPPER CASE.
      ASSIGN (lv_tabname) TO <fs_table>.

      ASSIGN COMPONENT <fs_name_field> OF STRUCTURE <fs_struct> TO <fs_value>.
      IF sy-subrc IS INITIAL.

        FIELD-SYMBOLS <fs_cols_value> TYPE ANY.

        CASE ls_zcte_fit002-operation.

          WHEN 'A'. "Código Abap

             CONCATENATE 'F'
                         ls_zcte_fit002-bapi_table
                         ls_zcte_fit002-bapi_field
                         INTO lv_form SEPARATED BY '_'.

             TRY.
                PERFORM (LV_FORM) IN PROGRAM ZCTE_ACC_INTEGRATION USING im_s_cols
                                                                        im_v_exec
                                                                  CHANGING <fs_value> IF FOUND.
             CATCH CX_SY_DYN_CALL_ILLEGAL_FORM.
                MESSAGE e006 WITH lv_form INTO v_message.
                ch_s_log-log_obj->capture_and_add_message( ).
                ch_v_error = abap_true.
             ENDTRY.

            "--------------------
          WHEN 'C'. " Coluna

*            lv_column_name  = 'COL' && ls_zcte_fit002-mapping(3).  "**COMPONENT SAP_ABA 7.3 or HIGHER
            CONCATENATE 'COL' ls_zcte_fit002-mapping(3) INTO lv_column_name.

            ASSIGN COMPONENT lv_column_name OF STRUCTURE im_s_cols TO <fs_cols_value>.
            IF sy-subrc IS INITIAL.

                me->process_and_adjust_field(
                  EXPORTING
                    im_v_value = <fs_cols_value>
                  CHANGING
                    ch_v_value = <fs_value> ).

            ENDIF."ASSIGN COMPONENT lv_column_name

            "--------------------
          WHEN 'F'. " Fixo

            <fs_value> = ls_zcte_fit002-mapping.

            "--------------------
          WHEN 'P'. " First found

            DATA: lv_col1(3)    TYPE c,
                  lv_col2(3)    TYPE c.

            SPLIT ls_zcte_fit002-mapping AT ';' INTO lv_col1
                                                        lv_col2.

*              lv_column_name  = 'COL' && lv_col_report_key. "**COMPONENT SAP_ABA 7.3 or HIGHER
              CONCATENATE 'COL' lv_col1 INTO lv_column_name.
              UNASSIGN <fs_cols_value>.
              ASSIGN COMPONENT lv_column_name OF STRUCTURE im_s_cols TO <fs_cols_value>.
              IF <fs_cols_value> IS ASSIGNED AND <fs_cols_value> IS INITIAL.
                 UNASSIGN <fs_cols_value>.
*              lv_column_name  = 'COL' && lv_col_cash_adv_name. "**COMPONENT SAP_ABA 7.3 or HIGHER
                 CONCATENATE 'COL' lv_col2 INTO lv_column_name.
                 ASSIGN COMPONENT lv_column_name OF STRUCTURE im_s_cols TO <fs_cols_value>.
              ENDIF.


             IF <fs_cols_value> IS ASSIGNED AND NOT
                <fs_cols_value> IS INITIAL.

                 me->process_and_adjust_field(
                   EXPORTING
                     im_v_value = <fs_cols_value>
                   CHANGING
                     ch_v_value = <fs_value> ).
            ENDIF. "IF <fs_cols_value> IS ASSIGNED AND NOT <fs_cols_value> IS INITIAL.

            "--------------------
          WHEN 'X'. " Concatenar Fixo e Coluna

            SPLIT ls_zcte_fit002-mapping AT ';' INTO TABLE lt_single_line.
            LOOP AT lt_single_line INTO ls_single_line.

              IF ls_single_line(1) = '"'.
                REPLACE ALL OCCURRENCES OF '"' IN ls_single_line WITH space.
                CONDENSE ls_single_line NO-GAPS.
                DATA: lv_strlen TYPE i.
                lv_strlen = STRLEN( ls_single_line ).
*                <fs_value> = <fs_value> && ls_single_line(lv_strlen). "**COMPONENT SAP_ABA 7.3 or HIGHER
                CONCATENATE <fs_value> ls_single_line(lv_strlen) INTO <fs_value>.
                CONTINUE.
              ENDIF.

*              lv_column_name  = 'COL' && ls_single_line(3)."**COMPONENT SAP_ABA 7.3 or HIGHER
              CONCATENATE 'COL' ls_single_line(3) INTO lv_column_name.

              ASSIGN COMPONENT lv_column_name OF STRUCTURE im_s_cols TO <fs_cols_value>.
              IF sy-subrc IS INITIAL.
                 me->process_and_adjust_field(
                   EXPORTING
                     im_v_value = <fs_cols_value>
                   CHANGING
                     ch_v_value = <fs_value> ).
              ENDIF."ASSIGN COMPONENT lv_column_name
            ENDLOOP."LOOP AT lt_single_line
        ENDCASE.

      ELSE.

        MESSAGE e005 WITH <fs_name_field> <fs_struct> INTO v_message.
        ch_s_log-log_obj->capture_and_add_message( ).
        ch_v_error = abap_true.

      ENDIF."ASSIGN COMPONENT <fs_name_field>
    ENDIF."ASSIGN COMPONENT 'bapi_field'

    AT LAST.
      ASSIGN COMPONENT 'ITEMNO_ACC' OF STRUCTURE <fs_struct> TO <fs_itemno>.
      <fs_itemno> = im_v_itemno.
      APPEND <fs_struct> TO <fs_table>.
    ENDAT.

  ENDLOOP."LOOP AT im_i_zcte_fit002

endmethod.


method GET_AUX_ACCOUNT.

  SELECT * FROM zcte_fit008 INTO TABLE ex_i_zcte_fit008
     WHERE entity_id = o_zcte_utils->gs_zcte_t001-entity_id.
  SORT ex_i_zcte_fit008 BY pmnt_type.

endmethod.


method GET_AUX_MAPPING.

  SELECT * FROM zcte_fit002 INTO TABLE ex_i_zcte_fit002
    WHERE entity_id = o_zcte_utils->gs_zcte_t001-entity_id.
  SORT ex_i_zcte_fit002 BY bapi_table DESCENDING.

endmethod.


method GET_AUX_TEXTS.

  SELECT * FROM zcte_fit009 INTO TABLE ex_i_zcte_fit009
    WHERE entity_id EQ o_zcte_utils->gs_zcte_t001-entity_id
      AND language  EQ sy-langu.

  SORT ex_i_zcte_fit009 BY source_text.


endmethod.


method GET_GLOBAL_DATA.


  DATA lv_data TYPE datum.

  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
   IMPORTING
     OWN_LOGICAL_SYSTEM                   = v_logical_system
   EXCEPTIONS
     OWN_LOGICAL_SYSTEM_NOT_DEFINED       = 1
     OTHERS                               = 2.

   IF sy-subrc NE 0.
     ch_cl_log_handle->capture_and_add_message( ).
     RAISE internal_error.
   ENDIF.

  CONCATENATE gs_cols_header-batch_date(4)
              gs_cols_header-batch_date+5(2)
              gs_cols_header-batch_date+8(2) INTO lv_data.


  SELECT * UP TO 1 ROWS
    FROM zcte_fit001
    INTO gs_zcte_fit001
    WHERE entity_id     = o_zcte_utils->gs_zcte_t001-entity_id
      AND batch_id      = gs_cols_header-batch_id
      AND batch_date_id = lv_data
      ORDER BY batch_repro DESCENDING.
   ENDSELECT.

  IF sy-subrc IS INITIAL.

    IF im_v_repro EQ 'X'.
      MESSAGE i015(zcte_base) INTO v_message. "File will be reprocessed
      ch_cl_log_handle->capture_and_add_message( ).
    ELSE.
      MESSAGE e016(zcte_base) INTO v_message. "File cannot be reprocessed
      ch_cl_log_handle->capture_and_add_message( ).
      RAISE internal_error.
    ENDIF.


    SELECT * FROM zcte_fit003
      INTO TABLE gt_zcte_fit003
      WHERE entity_id = o_zcte_utils->gs_zcte_t001-entity_id
        AND batch_id EQ gs_zcte_fit001-batch_id.
    IF sy-subrc NE 0 AND  im_v_repro EQ 'X'.
       MESSAGE e009 INTO v_message. " Record not found on ZCTE_FIT003.
       ch_cl_log_handle->capture_and_add_message( ).
       RAISE internal_error.
    ENDIF.

    IF NOT gt_zcte_fit003 IS INITIAL.

       SORT gt_zcte_fit003 BY report_key report_type.

       SELECT * FROM zcte_fit005
         INTO TABLE gt_zcte_fit005
         FOR ALL ENTRIES IN gt_zcte_fit003
         WHERE entity_id   = o_zcte_utils->gs_zcte_t001-entity_id
           AND report_id   =  gt_zcte_fit003-report_id
           AND report_key  =  gt_zcte_fit003-report_key
           AND report_type =  gt_zcte_fit003-report_type
           AND batch_id    =  gt_zcte_fit003-batch_id.
       IF sy-subrc EQ 0.

          SORT gt_zcte_fit005 BY report_id report_key report_type batch_id entry_id journal_key alloc_key ASCENDING
                                 change_nr DESCENDING.

          DELETE ADJACENT DUPLICATES FROM      gt_zcte_fit005
                                     COMPARING report_id report_key report_type batch_id entry_id journal_key alloc_key.
       ENDIF.
    ENDIF.
  ENDIF.





endmethod.


method GET_GT_COLS_FROM_DB.



  TYPES:  BEGIN OF l_ty_cols_item,
            entity_id   TYPE zcte_fit003-entity_id,
            report_type TYPE zcte_fit003-report_type,
            report_key  TYPE zcte_fit003-report_key,
            line_item   TYPE numc3,
          END OF l_ty_cols_item.


  TYPES: BEGIN OF l_ty_zcte_fit004,
           srtfd TYPE pclkey,
         END OF l_ty_zcte_fit004.


  DATA: li_zcte_fit004 TYPE STANDARD TABLE OF l_ty_zcte_fit004.

  DATA: ls_cols_item TYPE l_ty_cols_item,
        ls_cols_sel  TYPE l_ty_cols_item.
  DATA: ls_cols_aux  TYPE zcte_fis002.


  DATA: l_v_param     TYPE pclkey,
        l_v_key       TYPE char32.


  FIELD-SYMBOLS: <l_fs_zcte_fit004> TYPE l_ty_zcte_fit004.

  ls_cols_sel-report_type = im_v_report_type.
  ls_cols_sel-report_key  = im_v_report_key.

  CONCATENATE  im_v_entity
               ls_cols_sel-report_type
               ls_cols_sel-report_key
              '%'
  INTO l_v_param RESPECTING BLANKS.


  SELECT srtfd
    FROM zcte_fit004
    INTO TABLE li_zcte_fit004
    WHERE srtfd LIKE l_v_param.

  IF sy-subrc EQ 0.

     LOOP AT  li_zcte_fit004 ASSIGNING <l_fs_zcte_fit004>.
        ls_cols_item = <l_fs_zcte_fit004>-srtfd.

         CLEAR ls_cols_aux.
         TRY.
           IMPORT ls_cols_aux TO ls_cols_aux
           FROM DATABASE zcte_fit004(i0) ID ls_cols_item.
         CATCH cx_root.

         ENDTRY.

        IF NOT ls_cols_aux IS INITIAL.
          APPEND ls_cols_aux TO ex_i_cols.
        ENDIF.
     ENDLOOP.

  ENDIF.


endmethod.


method PROCESS_AND_ADJUST_FIELD.

  DATA: lo_descr_ref    TYPE REF TO cl_abap_typedescr,
        lv_type_kind(1) TYPE c,
        lv_length       TYPE i,
        lv_help_id      TYPE c LENGTH abap_max_help_id_ln,
        lv_editmask     TYPE c LENGTH abap_max_edit_mask_ln,
        lo_abap_descr   TYPE REF TO cl_abap_elemdescr.

  lo_descr_ref = cl_abap_typedescr=>describe_by_data( ch_v_value ).

  lv_type_kind = lo_descr_ref->type_kind.
  lv_length    = lo_descr_ref->length.

  lo_abap_descr ?= lo_descr_ref.
  lv_help_id     = lo_abap_descr->help_id.
  lv_editmask    = lo_abap_descr->edit_mask.

  CASE lv_type_kind.
    WHEN 'D'. "DATE
      CONCATENATE im_v_value(4) im_v_value+5(2) im_v_value+8(2) INTO ch_v_value.
    WHEN OTHERS.
      IF ch_v_value IS INITIAL.
        ch_v_value = im_v_value.
      ELSE.
        CONCATENATE ch_v_value im_v_value INTO ch_v_value.
      ENDIF.
  ENDCASE.

  CASE lv_editmask.
    WHEN '==ALPHA' .

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ch_v_value
        IMPORTING
          output = ch_v_value.
  ENDCASE.

  IF lv_help_id = 'BUKRS'.
    TRANSLATE ch_v_value TO UPPER CASE.
  ENDIF.

endmethod.


method PROCESS_EXTRACT_DOCUMENTS.

  DATA ls_documentheader  TYPE  bapiache09.

  DATA lt_cols_aux TYPE ty_t_cols.

  DATA: ls_cols       TYPE ty_cols,
        ls_cols_aux   TYPE ty_cols,
        ls_report_key TYPE ty_report_key,
        ls_log        TYPE zcte_s003.

  DATA: lt_zcte_fit002       TYPE ty_t_zcte_fit002,
        lt_zcte_fit002_aux   TYPE ty_t_zcte_fit002,
        lt_zcte_fit008       TYPE ty_t_zcte_fit008,
        lt_zcte_fit009       TYPE ty_t_zcte_fit009,
        lt_accountgl         TYPE TABLE OF bapiacgl09,
        lt_accountpayable    TYPE TABLE OF bapiacap09,
        lt_currencyamount    TYPE TABLE OF bapiaccr09.

  DATA: lv_itemno       TYPE i,
        lv_repeat       TYPE i,
        lv_error        TYPE abap_bool.

  FIELD-SYMBOLS: <l_fs_zcte_fit008> TYPE zcte_fit008,
                 <l_fs_zcte_fit009> TYPE zcte_fit009,
                 <l_fs_logs>        TYPE zcte_s003.


  IF NOT o_zcte_utils IS BOUND.
     RAISE entity_not_initialized.
  ENDIF.

  me->get_aux_mapping(
    IMPORTING
      ex_i_zcte_fit002 = lt_zcte_fit002 ).

  me->get_aux_account(
    IMPORTING
      ex_i_zcte_fit008 = lt_zcte_fit008 ).

  me->get_aux_texts(
    IMPORTING
      ex_i_zcte_fit009 = lt_zcte_fit009 ).


  LOOP AT gt_cols INTO ls_cols.

    CLEAR lv_itemno.

    lt_cols_aux[] = gt_cols[].
    lt_zcte_fit002_aux = lt_zcte_fit002.

    IF ls_cols-col020 IS NOT INITIAL. "Report Key
      DELETE lt_cols_aux WHERE col020 <> ls_cols-col020.
      DELETE gt_cols WHERE col020 = ls_cols-col020.
      DELETE lt_zcte_fit002_aux WHERE doc_type EQ c_cashadv. "Cash Adv
*      lv_report_key = ls_cols-col020.
*      lv_reptype    = c_expense.

    ELSEIF ls_cols-col187 IS NOT INITIAL.. " Cash Advance Key
      DELETE lt_cols_aux WHERE col185 <> ls_cols-col185 OR
                               col187 <> ls_cols-col187.

      DELETE gt_cols WHERE col185 = ls_cols-col185 AND
                           col187 = ls_cols-col187 .
      DELETE lt_zcte_fit002_aux WHERE doc_type EQ c_expense. "Expense Report

    ENDIF."IF ls_cols-col020 IS NOT INITIAL. "Report Key

    CLEAR lv_error.
    LOOP AT lt_cols_aux INTO ls_cols_aux.

      IF sy-tabix EQ 1.

** Build new report key for this report
        me->build_report_key(
          EXPORTING
            im_s_cols = ls_cols_aux
          IMPORTING
            ex_s_report_key = ls_report_key ).

** Create a new log object
        CLEAR ls_log.
        me->create_bal_log(
          EXPORTING
             im_s_report_key = ls_report_key
          IMPORTING
             ex_s_log        = ls_log ).

        IF ls_report_key-report_type EQ c_expense.
           MESSAGE i012 WITH ls_report_key-report_key INTO v_message. " Report start of process
           ls_log-log_obj->capture_and_add_message( ).
        ELSE.
          MESSAGE i013 WITH ls_report_key-report_key INTO v_message. " Cash Advance start of process
          ls_log-log_obj->capture_and_add_message( ).
        ENDIF.


** Perform checks before posting
        me->report_check_consistency(
          EXPORTING
            im_s_report_key = ls_report_key
            im_s_cols       = ls_cols_aux
          CHANGING
            ch_s_log        = ls_log
          EXCEPTIONS
            consistency_error  = 1
            OTHERS             = 2 ).

         IF sy-subrc NE 0.
           lv_error = abap_true.
           EXIT.
         ENDIF.

* * Check if modified fields exists for this posting ================================
         me->check_modified_field(
           EXPORTING
             im_s_report_key  = ls_report_key
           CHANGING
             ch_s_log         = ls_log
             ch_i_cols        = lt_cols_aux[] ).


        me->fill_document_header(
         EXPORTING
           im_i_zcte_fit002 = lt_zcte_fit002_aux
           im_s_cols        = ls_cols_aux
         IMPORTING
           ex_s_documentheader = ls_documentheader
         CHANGING
           ch_s_log         = ls_log
           ch_v_error       = lv_error ).


      ENDIF.  " IF sy-tabix = 1

** Check if there is translated texts available ======================================
    IF NOT lt_zcte_fit009[] IS INITIAL.
       me->translate_texts(
        EXPORTING
          im_i_zcte_fit009 = lt_zcte_fit009
        CHANGING
          ch_s_cols        = ls_cols_aux
          ch_s_log         = ls_log ).
    ENDIF.
** ===================================================================================

      IF ls_cols_aux-col126 EQ 'CBCP' AND  " Ignore CC debit line if its company card personal
         ls_cols_aux-col068 EQ 'Y'    AND
         ls_cols_aux-col165 NE 'Company'.
         CONTINUE.
      ENDIF.

      IF ls_cols_aux-col126 EQ 'CBCP' AND                 " Add a second run for the CC account item
         ls_cols_aux-col068 NE 'Y'.                       " if its personal expense
        lv_repeat = 2.
      ELSE.
        lv_repeat = 1.
      ENDIF.


      DO lv_repeat TIMES.

        IF lv_error EQ abap_true.
          EXIT.
        ENDIF.

        ADD 1 TO lv_itemno.

        IF sy-index = 2.
           READ TABLE lt_zcte_fit008 ASSIGNING <l_fs_zcte_fit008>
                                     WITH KEY pmnt_type = ls_cols_aux-col250
                                     BINARY SEARCH.
           IF sy-subrc EQ 0.
              ls_cols_aux-col167 = <l_fs_zcte_fit008>-account.   "Move transitory account
           ENDIF.
        ENDIF.

        me->fill_document_item(
          EXPORTING
            im_v_exec           = sy-index
            im_v_itemno         = lv_itemno
            im_s_cols           = ls_cols_aux
            im_i_zcte_fit002    = lt_zcte_fit002_aux
          IMPORTING
            ex_i_accountgl      = lt_accountgl
            ex_i_accountpayable = lt_accountpayable
            ex_i_currencyamount = lt_currencyamount
          CHANGING
            ch_s_log            = ls_log
            ch_v_error          = lv_error ).

      ENDDO.


    ENDLOOP.  "LOOP AT lt_cols_aux INTO ls_cols_aux.

    IF lv_error EQ abap_false.


      me->call_bapi_posting(
        EXPORTING
          im_s_cols           = ls_cols_aux
          im_s_documentheader = ls_documentheader
          im_i_accountgl      = lt_accountgl
          im_i_accountpayable = lt_accountpayable
          im_i_currencyamount = lt_currencyamount
        CHANGING
          ch_s_log            = ls_log ).

    ENDIF.

    REFRESH: lt_accountgl[],
             lt_accountpayable[],
             lt_currencyamount[].

    CLEAR ls_documentheader.

    APPEND ls_log TO ex_i_log_handle.

  ENDLOOP."  LOOP AT gt_cols INTO ls_cols

  me->update_postings_to_db(
    EXPORTING
      im_v_manual_proc = im_v_manual_proc
      im_i_log_handle  = ex_i_log_handle
    EXCEPTIONS
      internal_error = 1
      OTHERS         = 2 ).

endmethod.


method REPORT_CHECK_CONSISTENCY.

  FIELD-SYMBOLS: <l_fs_field>       TYPE any,
                 <l_fs_zcte_fit003> TYPE zcte_fit003.


  CASE im_s_report_key-report_type.

    WHEN c_expense.

**       Check if logical system is correct
      ASSIGN COMPONENT o_zcte_utils->v_er_logsys OF STRUCTURE im_s_cols TO <l_fs_field>.
      IF sy-subrc EQ 0 AND <l_fs_field> NE v_logical_system.
         MESSAGE e001 WITH <l_fs_field> INTO v_message. "Report is from a different environment
         ch_s_log-log_obj->capture_and_add_message( ).
         RAISE consistency_error.
      ENDIF.

*  * Check if entry has been posted already ============================================
      UNASSIGN <l_fs_zcte_fit003>.
      READ TABLE gt_zcte_fit003 ASSIGNING <l_fs_zcte_fit003>
                                WITH KEY report_key  = im_s_report_key-report_key
                                         report_type = im_s_report_key-report_type BINARY SEARCH.
      IF sy-subrc EQ 0 AND NOT <l_fs_zcte_fit003>-acc_belnr IS INITIAL.

         MESSAGE e003 WITH im_s_report_key-report_key <l_fs_zcte_fit003>-acc_belnr INTO v_message. "Report has already been posted
         ch_s_log-log_obj->capture_and_add_message( ).
         RAISE consistency_error.
      ENDIF.

   WHEN c_cashadv. " Cash advance ============================================================

** Check if logical system is correct
      ASSIGN COMPONENT o_zcte_utils->v_ad_logsys OF STRUCTURE im_s_cols TO <l_fs_field>.
      IF sy-subrc EQ 0 AND <l_fs_field> NE v_logical_system.
         MESSAGE e002 WITH <l_fs_field> INTO v_message.
         ch_s_log-log_obj->capture_and_add_message( ).
         RAISE consistency_error.
      ENDIF.

** Check if entry has been posted already ============================================
      UNASSIGN <l_fs_zcte_fit003>.
      READ TABLE gt_zcte_fit003 ASSIGNING <l_fs_zcte_fit003>
                                WITH KEY report_key  = im_s_report_key-report_key
                                         report_type = im_s_report_key-report_type BINARY SEARCH.

      IF sy-subrc EQ 0 AND NOT <l_fs_zcte_fit003>-acc_belnr IS INITIAL.

         MESSAGE e004 WITH im_s_report_key-report_key <l_fs_zcte_fit003>-acc_belnr INTO v_message. "Cash advance has already been posted
         ch_s_log-log_obj->capture_and_add_message( ).
         RAISE consistency_error.
      ENDIF.

  ENDCASE.



endmethod.


method SET_ENTITY_ID.

  IF NOT o_zcte_utils IS BOUND.
    CREATE OBJECT o_zcte_utils.
  ENDIF.

  CALL METHOD o_zcte_utils->set_entity
    EXPORTING
      im_v_entity = im_v_entity
    EXCEPTIONS
      not_found     = 1
      OTHERS        = 2 .

  IF sy-subrc NE 0.
    RAISE not_found.
  ENDIF.


endmethod.


method SET_GT_COLS_FROM_DB.


  DATA: lv_extnumber TYPE  bal_s_log-extnumber.
  DATA  ls_context   TYPE ZCTE_FIS001.

  CONCATENATE im_v_report_type
              im_v_report_key
         INTO lv_extnumber.

  CREATE OBJECT ex_o_loghandle
    EXPORTING
      im_object    = 'ZCTE'
      im_subobject = 'ZCTE01'
      im_extnumber = lv_extnumber
    EXCEPTIONS
      not_created  = 1
      OTHERS       = 2.

  IF sy-subrc NE 0.
    RAISE internal_error.

  ENDIF.

  ls_context-report_type = 'File'.

  CALL METHOD ex_o_loghandle->set_object
    EXPORTING
       im_s_context = ls_context
       im_reftabname = 'ZCTE_S001'.

  CLEAR gs_cols_header.

  gs_cols_header-batch_date = im_v_batch_date.
  gs_cols_header-batch_id   = im_v_batch_id.


  me->get_gt_cols_from_db(
    EXPORTING
      im_v_entity      =  o_zcte_utils->gs_zcte_t001-entity_id
      im_v_report_type =  im_v_report_type
      im_v_report_key  =  im_v_report_key
    IMPORTING
      ex_i_cols = gt_cols[] ).

   me->get_global_data(
     EXPORTING
       im_v_repro = abap_true
     CHANGING
       ch_cl_log_handle = ex_o_loghandle
     EXCEPTIONS
       internal_error = 1
       OTHERS         = 2 ).

   IF sy-subrc NE 0.
      RAISE internal_error.
   ENDIF.

  me->update_batch_info_to_db(
      EXPORTING
        im_v_manual_proc = abap_true
      CHANGING
        ch_cl_log_handle = ex_o_loghandle
      EXCEPTIONS
        internal_error = 1
        OTHERS         = 2 ).

   IF sy-subrc NE 0.
      RAISE internal_error.
   ENDIF.



endmethod.


method SET_GT_COLS_FROM_FILE.

*&---------------------------------------------------------------------*
*& Types
*&---------------------------------------------------------------------*
   TYPES:  BEGIN OF lty_single_line,
            line(255) TYPE c,
           END OF lty_single_line.

*&---------------------------------------------------------------------*
*& Internal Tables
*&---------------------------------------------------------------------*

   DATA: lt_single_line   TYPE TABLE OF lty_single_line,
         lt_file          TYPE TABLE OF string.

*&---------------------------------------------------------------------*
*& Workareas
*&---------------------------------------------------------------------*
   DATA: ls_cols        TYPE ty_cols,
         ls_data        TYPE lty_single_line,
         ls_message     TYPE zcte_s002.
*&---------------------------------------------------------------------*
*& Variables
*&---------------------------------------------------------------------*

  DATA: lv_table_string  TYPE string.

*&---------------------------------------------------------------------*
*& Field-symbols
*&---------------------------------------------------------------------*

  FIELD-SYMBOLS: <fs>          TYPE ANY.

*&---------------------------------------------------------------------*


  IF im_i_file[] IS INITIAL.
     RAISE import_error.
  ENDIF.

  IF o_zcte_utils->gs_zcte_t001-entity_id IS INITIAL.
    RAISE import_error.
  ENDIF.

  CLEAR gt_cols[].

  lt_file[] = im_i_file[].


  READ TABLE lt_file INTO lv_table_string INDEX 1.
  IF sy-subrc IS INITIAL.

    CLEAR gs_cols_header.
    SPLIT lv_table_string AT c_pipe INTO gs_cols_header-constant
                                         gs_cols_header-batch_date
                                         gs_cols_header-record
                                         gs_cols_header-j_amount_total
                                         gs_cols_header-batch_id .

    IF gs_cols_header-constant = c_extract.

       CLEAR lv_table_string .
       LOOP AT lt_file INTO lv_table_string FROM 2 .

         SPLIT lv_table_string AT c_pipe INTO TABLE lt_single_line.

         LOOP AT lt_single_line INTO ls_data.
           ASSIGN COMPONENT sy-tabix OF STRUCTURE ls_cols TO <fs>.
           <fs> = ls_data.
         ENDLOOP.
         APPEND ls_cols TO gt_cols.
         CLEAR ls_cols.
       ENDLOOP.

     ELSE.
       MESSAGE e005(zcte_base) INTO v_message. " File Header not found
       ch_cl_log_handle->capture_and_add_message( ).
       RAISE import_error.
     ENDIF.
   ENDIF.

   me->get_global_data(
     EXPORTING
       im_v_repro = im_v_repro
     CHANGING
       ch_cl_log_handle = ch_cl_log_handle
     EXCEPTIONS
       internal_error = 1
       OTHERS         = 2 ).

   IF sy-subrc NE 0.
      RAISE import_error.
   ENDIF.

  IF im_v_save_to_db EQ abap_true.
     me->export_document_items_to_db(
      CHANGING
        ch_cl_log_handle = ch_cl_log_handle
      EXCEPTIONS
        internal_error = 1
        OTHERS         = 2 ).
    IF sy-subrc NE 0.
      RAISE import_error.
    ENDIF.
  ENDIF.


  me->update_batch_info_to_db(
      EXPORTING
        im_v_manual_proc = abap_false
      CHANGING
        ch_cl_log_handle = ch_cl_log_handle
      EXCEPTIONS
        internal_error = 1
        OTHERS         = 2 ).

  IF sy-subrc NE 0.
     RAISE import_error.
  ENDIF.


endmethod.


method TRANSLATE_TEXTS.

  FIELD-SYMBOLS: <l_fs_zcte_fit009>  TYPE zcte_fit009.

  READ TABLE im_i_zcte_fit009 WITH KEY source_text = ch_s_cols-col063
                             ASSIGNING <l_fs_zcte_fit009> BINARY SEARCH.

  IF sy-subrc EQ 0.
        MESSAGE i008 WITH ch_s_cols-col063 <l_fs_zcte_fit009>-target_text INTO v_message.
        ch_s_log-log_obj->capture_and_add_message( ).
        ch_s_cols-col063 =  <l_fs_zcte_fit009>-target_text.
  ENDIF.


endmethod.


method UPDATE_BATCH_INFO_TO_DB.

  DATA ls_zcte_fit001 TYPE zcte_fit001.

  IF gs_cols_header IS INITIAL.
    RAISE internal_error.
  ENDIF.

  ls_zcte_fit001-entity_id      = o_zcte_utils->gs_zcte_t001-entity_id.
  ls_zcte_fit001-batch_id       = gs_cols_header-batch_id.

  CONCATENATE gs_cols_header-batch_date(4)
              gs_cols_header-batch_date+5(2)
              gs_cols_header-batch_date+8(2) INTO ls_zcte_fit001-batch_date_id.

  ls_zcte_fit001-batch_date_pro = sy-datum.
  ls_zcte_fit001-batch_time_pro = sy-uzeit.
  ls_zcte_fit001-batch_repro    = gs_zcte_fit001-batch_repro + 1.
  ls_zcte_fit001-manual_proc    = im_v_manual_proc.

*  READ TABLE gt_logs ASSIGNING <fs_logs> WITH KEY extnumber = gv_ext.
*  IF sy-subrc EQ 0.
     ls_zcte_fit001-batch_log_id = ch_cl_log_handle->save_log( ).
*  ENDIF.

  MODIFY zcte_fit001 FROM ls_zcte_fit001.
  IF NOT sy-subrc IS INITIAL.
    ROLLBACK WORK.
    MESSAGE e018 DISPLAY LIKE 'I'. "INTO v_message.
*    ch_cl_log_handle->capture_and_add_message( ). " Unable to update table zcte_fit001
    MESSAGE e010."INTO v_message.
*    ch_cl_log_handle->capture_and_add_message( ).  " Critical error - process aborted
    RAISE internal_error.
  ENDIF.






endmethod.


method UPDATE_DOCUMENTS_HEADER.

  DATA: ls_zcte_fit003 TYPE zcte_fit003,
        ls_zcte_fit007 TYPE zcte_fit007.

  IF im_s_cols-col185 EQ '1'."Adiantamento

     ls_zcte_fit003-report_type    = c_cashadv. "Cash Advance
     ls_zcte_fit003-report_name    = im_s_cols-col259.
     ls_zcte_fit003-report_key     = im_s_cols-col187.
  ELSE. "Prestação
     ls_zcte_fit003-report_type    = c_expense. "Expense report
     ls_zcte_fit003-report_name    = im_s_cols-col027.
     ls_zcte_fit003-report_id      = im_s_cols-col019.
     ls_zcte_fit003-report_key     = im_s_cols-col020.
  ENDIF.


  ls_zcte_fit003-entity_id      = o_zcte_utils->gs_zcte_t001-entity_id.
  ls_zcte_fit003-employee_id    = im_s_cols-col005.
  ls_zcte_fit003-emp_last_name  = im_s_cols-col006.
  ls_zcte_fit003-emp_first_name = im_s_cols-col007.
  ls_zcte_fit003-batch_id       = gs_cols_header-batch_id.
  ls_zcte_fit003-acc_bukrs      = im_v_msgv2+10(4).
  ls_zcte_fit003-acc_belnr      = im_v_msgv2(10).
  ls_zcte_fit003-acc_gjahr      = im_v_msgv2+14(4).
  ls_zcte_fit003-pmnt_status    = im_v_pmnt_status.


  READ TABLE gt_zcte_fit003 WITH KEY report_key  = ls_zcte_fit003-report_key
                                     report_type = ls_zcte_fit003-report_type
                            BINARY SEARCH TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
     MODIFY gt_zcte_fit003 FROM ls_zcte_fit003 INDEX sy-tabix.
  ELSE.
     APPEND ls_zcte_fit003 TO gt_zcte_fit003.
  ENDIF.


endmethod.


method UPDATE_POSTINGS_TO_DB.


  DATA: lt_zcte_fit007 TYPE STANDARD TABLE OF zcte_fit007.

  DATA: lv_repkey TYPE pclkey,
        lv_error  TYPE abap_bool.

  FIELD-SYMBOLS: <l_fs_zcte_fit003> TYPE zcte_fit003,
                 <l_fs_zcte_fit007> TYPE zcte_fit007,
                 <fs_logs>          TYPE zcte_s003.


  IF NOT gt_zcte_fit003 IS INITIAL.

    LOOP AT gt_zcte_fit003 ASSIGNING <l_fs_zcte_fit003>.

      CONCATENATE  <l_fs_zcte_fit003>-report_type
                   <l_fs_zcte_fit003>-report_key
            INTO lv_repkey.

      READ TABLE im_i_log_handle WITH KEY extnumber = lv_repkey ASSIGNING <fs_logs>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lt_zcte_fit007 ASSIGNING <l_fs_zcte_fit007>.

      <l_fs_zcte_fit007>-entity_id     = o_zcte_utils->gs_zcte_t001-entity_id.
      <l_fs_zcte_fit007>-report_id     = <l_fs_zcte_fit003>-report_id.
      <l_fs_zcte_fit007>-report_key    = <l_fs_zcte_fit003>-report_key.
      <l_fs_zcte_fit007>-report_type   = <l_fs_zcte_fit003>-report_type.
      <l_fs_zcte_fit007>-log_id        = <fs_logs>-log_obj->save_log( ).
      <l_fs_zcte_fit007>-log_date_proc   = sy-datum.
      <l_fs_zcte_fit007>-log_time_proc   = sy-uzeit.
      <l_fs_zcte_fit007>-manual_proc     = im_v_manual_proc.
      <l_fs_zcte_fit007>-proc_user       = sy-uname.

      SELECT log_repro UP TO 1 ROWS
        FROM zcte_fit007
        INTO <l_fs_zcte_fit007>-log_repro
        WHERE entity_id    = o_zcte_utils->gs_zcte_t001-entity_id
          AND report_id    = <l_fs_zcte_fit007>-report_id
          AND report_key   = <l_fs_zcte_fit007>-report_key
          AND report_type  = <l_fs_zcte_fit007>-report_type
          ORDER BY log_repro DESCENDING.
       ENDSELECT.
       ADD 1 TO <l_fs_zcte_fit007>-log_repro.

    ENDLOOP.

    CLEAR lv_error.
" Add routine to enqueue tables before commit. =======================================
"======================================================================================
"======================================================================================

     MODIFY zcte_fit003 FROM TABLE gt_zcte_fit003.
     IF sy-subrc NE 0.
        lv_error = abap_true.
        MESSAGE e019 DISPLAY LIKE 'I'. "INTO v_message.
*        ch_cl_log_handle->capture_and_add_message( ). " Unable to update table zcte_fit003
        MESSAGE e010. "INTO v_message.
*        ch_cl_log_handle->capture_and_add_message( ).  " Critical error - process aborted
     ENDIF.

     IF lv_error EQ abap_false.
       MODIFY zcte_fit007 FROM TABLE lt_zcte_fit007.
       IF sy-subrc NE 0.
          lv_error = abap_true.
          MESSAGE e020 DISPLAY LIKE 'I'. "INTO v_message.
*          ch_cl_log_handle->capture_and_add_message( ). " Unable to update table zcte_fit007
          MESSAGE e010 INTO v_message.
*          ch_cl_log_handle->capture_and_add_message( ).  " Critical error - process aborted
       ENDIF.
     ENDIF.
  ENDIF.


  IF lv_error EQ abap_true.
    ROLLBACK WORK.
  ELSE.


*    bapi transaction commit

    COMMIT WORK AND WAIT.
  ENDIF.
*

endmethod.
ENDCLASS.
