*&---------------------------------------------------------------------*
*& Report  ZCTE_ACC_MASS_INTEGRATION
*&---------------------------------------------------------------------*
*&                  SAP BRAZIL
*&   DEVELOPER:  Gilliatti Paparelli  2020/04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZCTE_ACC_MASS_INTEGRATION.

  TYPE-POOLS: vrm, abap.


  TYPES: BEGIN OF ty_files,
           filename  TYPE char100,
         END OF ty_files.

  DATA: i_files TYPE STANDARD TABLE OF ty_files.
  DATA: gt_zcte_t001 TYPE STANDARD TABLE OF zcte_t001.

  DATA: i_list   TYPE VRM_VALUES,
        wa_value TYPE VRM_VALUE.


  DATA: go_zcte_utils TYPE REF TO zcte_cl_utils.

  DATA: v_name   TYPE VRM_ID.

SELECTION-SCREEN: BEGIN OF BLOCK file WITH FRAME TITLE text-t00.
  PARAMETERS: rb_pc     TYPE c RADIOBUTTON GROUP src USER-COMMAND cmd DEFAULT 'X',
              rb_os     TYPE c RADIOBUTTON GROUP src .

  SELECTION-SCREEN SKIP.

  PARAMETERS p_from TYPE zcte_inbound_dir OBLIGATORY.
  PARAMETERS p_to TYPE zcte_backup_dir.

  SELECTION-SCREEN SKIP.

  PARAMETERS p_mask TYPE zcte_disp_name DEFAULT 'extract_CES_SAE_v3_' OBLIGATORY.
  PARAMETERS: p_entity TYPE zcte_entity_id AS LISTBOX VISIBLE LENGTH 12 OBLIGATORY USER-COMMAND entity.

  PARAMETERS p_repro AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK file.

INITIALIZATION.
  PERFORM f_get_entity_list.

AT SELECTION-SCREEN OUTPUT.

  PERFORM f_set_directory.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_from.
  PERFORM f_sel_directory CHANGING p_from.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_to.
  PERFORM f_sel_directory CHANGING p_to.

START-OF-SELECTION.
    PERFORM: f_list_folder_files,
             f_acc_report_call.

*---------------------------------------------------------------------*
*&      Form  SEL_DIRECTORY
*&---------------------------------------------------------------------*
FORM f_sel_directory CHANGING pc_selected_folder .

  IF rb_pc EQ 'X'.
    pc_selected_folder = zcte_cl_file_handle=>online_folder_selection( ).
  ELSE.
    pc_selected_folder = zcte_cl_file_handle=>background_folder_selection( ).
  ENDIF.

ENDFORM. " SEL_DIRECTORY
*&---------------------------------------------------------------------*
*&      Form  F_LIST_FOLDER_FILES
*&---------------------------------------------------------------------*
FORM f_list_folder_files .

  DATA: l_v_from  TYPE string.
  DATA: l_i_files TYPE STANDARD TABLE OF EPS2FILI.
  DATA: l_v_count TYPE i.
  DATA: l_v_dir_name TYPE EPS2FILNAM.

  FIELD-SYMBOLS: <l_fs_files>     TYPE EPS2FILI,
                 <l_fs_files_aux> TYPE ty_files.

  CLEAR i_files.

  IF rb_pc EQ abap_true.

      l_v_from = p_from.
      CALL METHOD cl_gui_frontend_services=>directory_list_files
         EXPORTING
            directory = l_v_from
         CHANGING
            count      = l_v_count
            file_table = i_files
         EXCEPTIONS
           CNTL_ERROR                       = 1
           DIRECTORY_LIST_FILES_FAILED      = 2
           WRONG_PARAMETER                  = 3
           ERROR_NO_GUI                     = 4
           NOT_SUPPORTED_BY_GUI             = 5
           OTHERS                           = 6.

   ELSE.


      l_v_dir_name = p_from.
      CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
        EXPORTING
          iv_dir_name                  = l_v_dir_name
        tables
          dir_list                     = l_i_files
       EXCEPTIONS
         INVALID_EPS_SUBDIR           = 1
         SAPGPARAM_FAILED             = 2
         BUILD_DIRECTORY_FAILED       = 3
         NO_AUTHORIZATION             = 4
         READ_DIRECTORY_FAILED        = 5
         TOO_MANY_READ_ERRORS         = 6
         EMPTY_DIRECTORY_LIST         = 7
         OTHERS                       = 8.

      IF sy-subrc <> 0.
*       Implement suitable error handling here
      ENDIF.


     IF sy-subrc EQ 0.

        LOOP AT l_i_files ASSIGNING <l_fs_files>.
          APPEND INITIAL LINE TO i_files ASSIGNING <l_fs_files_aux>.
          <l_fs_files_aux>-filename = <l_fs_files>-name.
        ENDLOOP.
     ENDIF.
   ENDIF.


   IF NOT i_files IS INITIAL.
      PERFORM f_filter_file_list.
   ENDIF.





ENDFORM.                    " F_LIST_FOLDER_FILES
*&---------------------------------------------------------------------*
*&      Form  F_FILTER_FILE_LIST
*&---------------------------------------------------------------------*
FORM f_filter_file_list .

  DATA: l_v_filter     TYPE zcte_disp_name,
        l_v_len        TYPE i.

  FIELD-SYMBOLS: <l_fs_files> TYPE ty_files.

  CONCATENATE p_mask p_entity INTO l_v_filter.

  l_v_len = strlen( l_v_filter ).

  LOOP AT i_files ASSIGNING <l_fs_files>.
     IF <l_fs_files>-filename(l_v_len) NE l_v_filter.
        DELETE i_files INDEX sy-tabix.
     ENDIF.
  ENDLOOP.


ENDFORM.                    " F_FILTER_FILE_LIST
*&---------------------------------------------------------------------*
*&      Form  F_ACC_REPORT_CALL
*&---------------------------------------------------------------------*
FORM f_acc_report_call .

  DATA: l_v_message TYPE char100,
        l_v_from    TYPE char100.

  FIELD-SYMBOLS: <l_fs_files> TYPE ty_files.

  IF i_files IS INITIAL.
     MESSAGE e500(26) WITH p_from 'Nenhum arquivo valido encontrado'.
     EXIT.
  ENDIF.

  LOOP AT i_files ASSIGNING <l_fs_files>.
     CONCATENATE p_from <l_fs_files>-filename
            INTO l_v_from SEPARATED BY '\'.


     SUBMIT ZCTE_ACC_INTEGRATION
        WITH rb_pc   = rb_pc
        WITH rb_os   = rb_os
        WITH p_from  = l_v_from
        WITH p_to    = p_to
        WITH p_repro = p_repro
        WITH p_log   = abap_false AND RETURN.

  ENDLOOP.

ENDFORM.                    " F_ACC_REPORT_CALL
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
