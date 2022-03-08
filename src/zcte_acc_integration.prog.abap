*&---------------------------------------------------------------------*
*& Report  ZCTE_ACC_INTEGRATION
*&---------------------------------------------------------------------*
*&                  SAP BRAZIL
*&   DEVELOPER:  Gilliatti Paparelli  2020/04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZCTE_ACC_INTEGRATION.

TABLES sscrfields.

DATA: gt_zcte_t001 TYPE zcte_t001_t,
      gt_logs      TYPE zcte_s003_t.

DATA: go_zcte_utils   TYPE REF TO zcte_cl_utils.

SELECTION-SCREEN: BEGIN OF BLOCK file WITH FRAME TITLE text-t00.
  PARAMETERS: rb_pc     TYPE c RADIOBUTTON GROUP src DEFAULT 'X' USER-COMMAND cmd,
              rb_os     TYPE c RADIOBUTTON GROUP src.

  SELECTION-SCREEN SKIP.

  PARAMETERS p_from TYPE zcte_inbound_dir.
  PARAMETERS p_to TYPE zcte_backup_dir.

  SELECTION-SCREEN SKIP.

  PARAMETERS p_repro AS CHECKBOX.
  PARAMETERS p_log AS CHECKBOX DEFAULT 'X'.

  SELECTION-SCREEN SKIP.

  PARAMETERS: p_entity TYPE zcte_entity_id AS LISTBOX VISIBLE LENGTH 12 OBLIGATORY USER-COMMAND entity.

SELECTION-SCREEN: END OF BLOCK file.



SELECTION-SCREEN: BEGIN OF BLOCK button WITH FRAME TITLE text-t03.
SELECTION-SCREEN: BEGIN OF LINE,
PUSHBUTTON 2(20) bt USER-COMMAND button.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: END OF BLOCK button.

INCLUDE zcte_acc_integration_f01.

*-------------------------------------------------------------------------*

INITIALIZATION.
  bt = text-t02.
  PERFORM f_get_entity_list.
*-------------------------------------------------------------------------*


AT SELECTION-SCREEN OUTPUT.

  PERFORM f_set_directory.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_from.
  PERFORM f_sel_directory CHANGING p_from.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_to.
  PERFORM f_sel_directory CHANGING p_to.

AT SELECTION-SCREEN.
  CASE sscrfields.
    WHEN 'BUTTON'.

      CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
        EXPORTING
          action                       = 'U'
          view_name                    = 'zcte_fit002'  "maint.View
        EXCEPTIONS
          client_reference             = 1
          foreign_lock                 = 2
          invalid_action               = 3
          no_clientindependent_auth    = 4
          no_database_function         = 5
          no_editor_function           = 6
          no_show_auth                 = 7
          no_tvdir_entry               = 8
          no_upd_auth                  = 9
          only_show_allowed            = 10
          system_failure               = 11
          unknown_field_in_dba_sellist = 12
          view_not_found               = 13
          OTHERS                       = 14.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

  ENDCASE.

START-OF-SELECTION.

  PERFORM f_process_postings.

  END-OF-SELECTION.

  IF p_log EQ abap_true.
     PERFORM f_show_bal.
  ENDIF.

*---------------------------------------------------------------------*
*&      Form  SEL_DIRECTORY
*&---------------------------------------------------------------------*
FORM f_sel_directory CHANGING pc_selected_folder .

  IF rb_pc EQ abap_true.
     pc_selected_folder = zcte_cl_file_handle=>online_file_selection( ).
  ELSE.
     pc_selected_folder = zcte_cl_file_handle=>background_file_selection( ).
  ENDIF.

ENDFORM. " SEL_DIRECTORY

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_POSTINGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_PROCESS_POSTINGS .


  TYPES: BEGIN OF lty_filename,
           value   TYPE char100,
         END OF lty_filename.

  DATA: lt_filename    TYPE TABLE OF lty_filename,
        lt_table_file  TYPE TABLE OF string.
  DATA: lt_logs        TYPE zcte_s003_t.

  DATA: ls_logs TYPE zcte_s003.

  DATA: lo_acc_integration  TYPE REF TO zcte_cl_acc_integration.

  DATA: lv_mode         TYPE c,
        lv_lines        TYPE i,
        lv_save_to_db TYPE abap_bool,
        lv_ok           TYPE abap_bool,
        lv_fileto       TYPE zcte_backup_dir.


  FIELD-SYMBOLS: <l_fs_message>  TYPE zcte_S002,
                 <l_fs_filename> TYPE lty_filename.


  IF rb_pc EQ abap_true.
     lv_mode = 'O'.
  ELSE.
     lv_mode = 'B'.
  ENDIF.

  SPLIT p_from AT '\' INTO TABLE lt_filename.
  IF sy-subrc EQ 0.
    DESCRIBE TABLE lt_filename LINES lv_lines.
    READ TABLE lt_filename INDEX lv_lines ASSIGNING <l_fs_filename>.

    IF NOT <l_fs_filename> CS p_entity.
      MESSAGE e020(zcte_base) WITH p_entity. "File does not belong to entity
    ENDIF.

    CONCATENATE p_to <l_fs_filename> '_proc_' sy-datum sy-uzeit '.txt' INTO lv_fileto.
  ENDIF.

  PERFORM f_bal_log_create USING <l_fs_filename> CHANGING ls_logs.

  IF NOT ls_logs-log_obj IS BOUND.
    MESSAGE e014(zcte_base).
    EXIT.
  ENDIF.

  CALL METHOD zcte_cl_file_handle=>file_import(
   EXPORTING
     im_v_filepath    = p_from
     im_v_mode        = lv_mode
   IMPORTING
     ex_i_table       = lt_table_file
   CHANGING
     ch_cl_log_handle = ls_logs-log_obj
   EXCEPTIONS
     internal_error   = 1
     OTHERS           = 2 ).


   IF sy-subrc EQ 0.

      CALL METHOD zcte_cl_file_handle=>table_to_file_download(
        EXPORTING
          im_v_mode        = lv_mode
          im_v_filepath    = lv_fileto
        CHANGING
          ch_i_filetable   = lt_table_file
          ch_cl_log_handle = ls_logs-log_obj
        EXCEPTIONS
          internal_error      = 1
          OTHERS              = 2 ).

      IF sy-subrc EQ 0.

         CALL METHOD zcte_cl_file_handle=>file_delete_from_directory(
           EXPORTING
             im_v_mode        = lv_mode
             im_v_filepath    = p_from
           CHANGING
             ch_cl_log_handle = ls_logs-log_obj
           EXCEPTIONS
             internal_error  = 1
             OTHERS          = 2 ).

         IF sy-subrc EQ 0.

            CREATE OBJECT lo_acc_integration.

            CALL METHOD lo_acc_integration->set_entity_id(
            EXPORTING im_v_entity = p_entity ).

            lv_save_to_db = abap_true.
            CALL METHOD lo_acc_integration->set_gt_cols_from_file(
              EXPORTING
                im_v_repro        = p_repro
                im_i_file         = lt_table_file
                im_v_save_to_db   = lv_save_to_db
              CHANGING
                ch_cl_log_handle = ls_logs-log_obj
              EXCEPTIONS
                import_error = 1
                OTHERS       = 2 ).

            IF sy-subrc EQ 0.

                  CLEAR lt_logs.

                  CALL METHOD lo_acc_integration->process_extract_documents(
                  EXPORTING
                     im_v_manual_proc = abap_false
                  IMPORTING
                    ex_i_log_handle = lt_logs
                  EXCEPTIONS
                    entity_not_initialized = 1
                    OTHERS                 = 2  ).

                  IF sy-subrc EQ 0.
                    APPEND LINES OF lt_logs TO gt_logs.
                  ENDIF.

            ENDIF.
         ENDIF.
      ENDIF.
   ENDIF.


ENDFORM.                    " F_PROCESS_POSTINGS


*&---------------------------------------------------------------------*
**&      Form  F_LOG_CREATE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
FORM f_bal_log_create USING    pu_v_ext
                      CHANGING pc_s_logs  TYPE zcte_s003.

  DATA: l_v_extnumber TYPE  bal_s_log-extnumber.
  DATA  l_s_context   TYPE ZCTE_FIS001.

  FIELD-SYMBOLS: <l_fs_logs> TYPE zcte_s003.

  l_v_extnumber = pu_v_ext.

  APPEND INITIAL LINE TO gt_logs ASSIGNING <l_fs_logs>.

  <l_fs_logs>-extnumber = pu_v_ext.

  CREATE OBJECT <l_fs_logs>-log_obj
    EXPORTING
      im_object    = 'ZCTE'
      im_subobject = 'ZCTE01'
      im_extnumber = l_v_extnumber
    EXCEPTIONS
      not_created  = 1
      OTHERS       = 2.

  l_s_context-report_type = 'File'. "pu_v_tipo.

  CALL METHOD <l_fs_logs>-log_obj->set_display_profile( ).

  CALL METHOD <l_fs_logs>-log_obj->set_object
    EXPORTING
       im_s_context = l_s_context
       im_reftabname = 'ZCTE_FIS001'.

  pc_s_logs = <l_fs_logs>.


ENDFORM.                    "f_bal_log_create

*&---------------------------------------------------------------------*
**&      Form  F_SHOW_BAL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
FORM f_show_bal .

  DATA: lt_loghndl TYPE bal_t_logh.

  DATA: lv_loghndl TYPE balloghndl.

   FIELD-SYMBOLS: <l_fs_logs> TYPE zcte_s003.


  LOOP AT gt_logs ASSIGNING <l_fs_logs>.
     lv_loghndl = <l_fs_logs>-log_obj->get_log_handle( ).
     INSERT lv_loghndl INTO TABLE lt_loghndl.
  ENDLOOP.

  zcte_cl_log_handle=>display_multiple_logs(
    EXPORTING
      im_v_profile = abap_true
      im_i_logs    = lt_loghndl
    EXCEPTIONS
      display_failed = 1 ).

  IF sy-subrc NE 0.
     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFORM.                    "f_show_bal
*
**&---------------------------------------------------------------------*
**&      Form  F_UPDATE_DTAB
**&---------------------------------------------------------------------*
*FORM f_update_dtab  CHANGING pc_erro.
*
*  DATA: lt_zcte_fit007 TYPE STANDARD TABLE OF zcte_fit007.
*
*  DATA: l_V_repkey TYPE pclkey.
*
*  FIELD-SYMBOLS: <l_fs_zcte_fit003> TYPE zcte_fit003,
*                 <l_fs_zcte_fit007> TYPE zcte_fit007.
*
*  IF pc_erro EQ abap_true.
*    EXIT.
*  ENDIF.
*
*  IF NOT gt_zcte_fit003 IS INITIAL.
*
*    LOOP AT gt_zcte_fit003 ASSIGNING <l_fs_zcte_fit003>.
*
*       APPEND INITIAL LINE TO lt_zcte_fit007 ASSIGNING <l_fs_zcte_fit007>.
*
*       <l_fs_zcte_fit007>-report_id     = <l_fs_zcte_fit003>-report_id.
*       <l_fs_zcte_fit007>-report_key    = <l_fs_zcte_fit003>-report_key.
*       <l_fs_zcte_fit007>-report_type   = <l_fs_zcte_fit003>-report_type.
*
*        CONCATENATE  <l_fs_zcte_fit007>-report_type
*                     <l_fs_zcte_fit007>-report_key
*              INTO l_v_repkey.
*
*        READ TABLE gt_logs WITH KEY extnumber = l_v_repkey ASSIGNING <fs_logs>.
*        IF sy-subrc EQ 0.
*           <l_fs_zcte_fit007>-log_id = <fs_logs>-log_obj->save_log( ).
*        ENDIF.
*
*        <l_fs_zcte_fit007>-log_date_proc   = sy-datum.
*        <l_fs_zcte_fit007>-log_time_proc   = sy-uzeit.
*        <l_fs_zcte_fit007>-manual_proc     = p_memo.
*        <l_fs_zcte_fit007>-proc_user       = sy-uname.
*
*        SELECT log_repro UP TO 1 ROWS
*          FROM zcte_fit007
*          INTO <l_fs_zcte_fit007>-log_repro
*          WHERE report_id    = <l_fs_zcte_fit007>-report_id
*            AND report_key   = <l_fs_zcte_fit007>-report_key
*            AND report_type  = <l_fs_zcte_fit007>-report_type
*            ORDER BY log_repro DESCENDING.
*         ENDSELECT.
*         ADD 1 TO <l_fs_zcte_fit007>-log_repro.
*
*    ENDLOOP.
*
*     MODIFY zcte_fit003 FROM TABLE gt_zcte_fit003.
*     IF sy-subrc NE 0.
*        pc_erro = abap_true.
*     ENDIF.
*
*     IF pc_erro EQ abap_false.
*       MODIFY zcte_fit007 FROM TABLE lt_zcte_fit007.
*       IF sy-subrc NE 0.
*          pc_erro = abap_true.
*       ENDIF.
*     ENDIF.
*  ENDIF.
*
*  IF pc_erro EQ abap_true.
*    ROLLBACK WORK.
*  ELSE.
*    COMMIT WORK AND WAIT.
*  ENDIF.
*
*
*ENDFORM.                    " F_UPDATE_DTAB
*&---------------------------------------------------------------------*
*&      Form  F_GET_ENTITY_LIST
*&---------------------------------------------------------------------*
FORM F_GET_ENTITY_LIST .

DATA: lt_list           TYPE VRM_VALUES.

DATA: ls_list           TYPE VRM_VALUE,
      ls_zcte_fit001    TYPE zcte_fit001.

DATA: lv_name           TYPE VRM_ID.

FIELD-SYMBOLS: <l_fs_zcte_t001> TYPE zcte_t001.


** Entity ID
  lv_name = 'P_ENTITY'.

  IF NOT go_zcte_utils IS BOUND.
    CREATE OBJECT go_zcte_utils.
    IF sy-subrc EQ 0.
       gt_zcte_t001 = go_zcte_utils->get_entity_list( ).
    ENDIF.

  ENDIF.

  LOOP AT gt_zcte_t001 ASSIGNING <l_fs_zcte_t001>.
    ls_list-key   = <l_fs_zcte_t001>-entity_id.
    ls_list-text  = <l_fs_zcte_t001>-entity_desc.
    APPEND ls_list TO lt_list.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = lv_name
      VALUES = lt_list.


ENDFORM.                    " F_GET_ENTITY_LIST
*&---------------------------------------------------------------------*
*&      Form  F_SET_DIRECTORY
*&---------------------------------------------------------------------*
FORM F_SET_DIRECTORY .

  FIELD-SYMBOLS: <l_fs_zcte_t001> TYPE zcte_t001.

  IF rb_os EQ abap_true.

      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'P_FROM' OR 'P_TO'.
             screen-input = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.

    IF NOT p_entity IS INITIAL.
       CLEAR: p_from, p_to.
       READ TABLE gt_zcte_t001 ASSIGNING <l_fs_zcte_t001> WITH KEY entity_id = p_entity.
       IF sy-subrc EQ 0.
           IF NOT <l_fs_zcte_t001>-inbound_dir IS INITIAL.
             p_from =  <l_fs_zcte_t001>-inbound_dir.
           ELSE.
             MESSAGE e017(zcte_base) WITH p_entity. " No inbound directory set
           ENDIF.

           IF NOT <l_fs_zcte_t001>-backup_dir IS INITIAL.
             p_to   =  <l_fs_zcte_t001>-backup_dir.
           ELSE.
             MESSAGE e019(zcte_base) WITH p_entity. " No backup directory set
           ENDIF.
       ENDIF.
     ENDIF.

  ELSE.

    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'P_FROM' OR 'P_TO'.
           screen-input = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.


ENDFORM.                    " F_SET_DIRECTORY
