class ZCTE_CL_FILE_HANDLE definition
  public
  final
  create public .

public section.
*"* public components of class ZCTE_CL_FILE_HANDLE
*"* do not include other source files here!!!

  class-methods FILE_IMPORT
    importing
      !IM_V_FILEPATH type ZCTE_INBOUND_DIR
      !IM_V_MODE type CHAR1
    exporting
      !EX_I_TABLE type ANY TABLE
    changing
      !CH_CL_LOG_HANDLE type ref to ZCTE_CL_LOG_HANDLE
    exceptions
      INTERNAL_ERROR .
  class-methods TABLE_TO_FILE_DOWNLOAD
    importing
      !IM_V_MODE type C
      !IM_V_FILEPATH type ZCTE_BACKUP_DIR
    changing
      !CH_I_FILETABLE type ANY TABLE
      !CH_CL_LOG_HANDLE type ref to ZCTE_CL_LOG_HANDLE
    exceptions
      INTERNAL_ERROR .
  class-methods FILE_DELETE_FROM_DIRECTORY
    importing
      !IM_V_MODE type C
      !IM_V_FILEPATH type ZCTE_INBOUND_DIR
    changing
      !CH_CL_LOG_HANDLE type ref to ZCTE_CL_LOG_HANDLE
    exceptions
      INTERNAL_ERROR .
  class-methods ONLINE_FILE_SELECTION
    returning
      value(RE_V_FILEPATH) type ZCTE_INBOUND_DIR .
  class-methods BACKGROUND_FILE_SELECTION
    returning
      value(RE_V_FILEPATH) type ZCTE_INBOUND_DIR .
  class-methods BACKGROUND_FOLDER_SELECTION
    returning
      value(RE_V_FOLDER) type ZCTE_INBOUND_DIR .
  class-methods ONLINE_FOLDER_SELECTION
    returning
      value(RE_V_FOLDER) type ZCTE_INBOUND_DIR .
protected section.
*"* protected components of class ZCTE_CL_FILE_HANDLE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCTE_CL_FILE_HANDLE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCTE_CL_FILE_HANDLE IMPLEMENTATION.


method BACKGROUND_FILE_SELECTION.

  CONSTANTS: l_c_directory  TYPE char3  VALUE '\..',
             l_c_filemask   TYPE char10 VALUE 'directory'.


  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    EXPORTING
      directory        = l_c_directory
      filemask         = l_c_filemask
    IMPORTING
      serverfile       = re_v_filepath
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.

  IF sy-subrc NE 0.
*    Error handling here
  ENDIF.


endmethod.


method BACKGROUND_FOLDER_SELECTION.

  CONSTANTS: l_c_directory   TYPE char3  VALUE '\..',
             l_c_filemask    TYPE char10 VALUE 'directory'.


  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    EXPORTING
      directory        = l_c_directory
      filemask         = l_c_filemask
    IMPORTING
      serverfile       = re_v_folder
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.

  IF sy-subrc NE 0.
*     Error handling here
  ENDIF.



endmethod.


method FILE_DELETE_FROM_DIRECTORY.

  DATA: lv_message_string  TYPE BAPI_MSG,
        lv_filepath        TYPE string.

  DATA: lv_string_line     TYPE string,
        lv_rc              TYPE i.

  DATA lo_cx_root TYPE REF TO cx_root.

  CASE im_v_mode.

    WHEN 'O'. "Online

       lv_filepath = im_v_filepath.
       CALL METHOD cl_gui_frontend_services=>file_delete
         EXPORTING
           filename             = lv_filepath
         CHANGING
           rc                   = lv_rc
         EXCEPTIONS
           file_delete_failed   = 1
           cntl_error           = 2
           error_no_gui         = 3
           file_not_found       = 4
           access_denied        = 5
           unknown_error        = 6
           not_supported_by_gui = 7
           wrong_parameter      = 8
           OTHERS               = 9.
       IF sy-subrc <> 0.

          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message_string.

          ch_cl_log_handle->capture_and_add_message( ).

          MESSAGE e011  INTO lv_message_string. "File deletion error
          RAISE internal_error.

       ELSE.

          MESSAGE s010 INTO lv_message_string.  " File delete success
          ch_cl_log_handle->capture_and_add_message( ).

       ENDIF.

    WHEN 'B'. "Background

      TRY.
          DELETE DATASET im_v_filepath.

          IF sy-subrc IS INITIAL.
            MESSAGE s009 INTO lv_message_string.  " File delete success
            ch_cl_log_handle->capture_and_add_message( ).
          ELSE.
            MESSAGE e003 WITH im_v_filepath INTO lv_message_string. "FIle not found
            ch_cl_log_handle->capture_and_add_message( ).
            RAISE internal_error.
          ENDIF.
        CATCH cx_root INTO lo_cx_root.
          ch_cl_log_handle->capture_and_add_message( ).
          MESSAGE e011  INTO lv_message_string. "File deletion error
          RAISE internal_error.
      ENDTRY.

      TRY.
          CLOSE DATASET im_v_filepath.
          IF sy-subrc IS INITIAL.
            MESSAGE s010 INTO lv_message_string.  " File delete success
            ch_cl_log_handle->capture_and_add_message( ).
          ELSE.
            MESSAGE e008  INTO lv_message_string. "Error closing file
            ch_cl_log_handle->capture_and_add_message( ).
            RAISE internal_error.
          ENDIF.

        CATCH cx_sy_file_close INTO lo_cx_root.
          ch_cl_log_handle->capture_and_add_message( ).
          MESSAGE e011  INTO lv_message_string. "File deletion error
          ch_cl_log_handle->capture_and_add_message( ).
          RAISE internal_error.
      ENDTRY.

    WHEN OTHERS.  "Invalid mode
      RAISE internal_error.
   ENDCASE.

endmethod.


method FILE_IMPORT.


  DATA: lt_table   TYPE TABLE OF string.

  DATA lv_message  TYPE BAPI_MSG.
  DATA:lv_filepath TYPE string.

  DATA lo_cx_root TYPE REF TO cx_root.

  FIELD-SYMBOLS: <fs_struct> TYPE ANY.

  IF NOT ch_cl_log_handle IS BOUND.
     RAISE internal_error.
  ENDIF.

  IF im_v_mode EQ 'O'. "Online.

    lv_filepath = im_v_filepath.
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = lv_filepath
        filetype                = 'ASC'            " FILE Type (ASCII, Binary)
        codepage                = '4110'
      CHANGING
        data_tab                = ex_i_table
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc IS NOT INITIAL.
      ch_cl_log_handle->capture_and_add_message( ).
      RAISE internal_error.
    ENDIF.

  ELSEIF im_v_mode EQ 'B'. "Background.

    REFRESH lt_table.

    TRY.
        OPEN DATASET im_v_filepath FOR INPUT IN TEXT MODE ENCODING DEFAULT
             MESSAGE lv_message.

        IF sy-subrc IS INITIAL.
          DO.
            APPEND INITIAL LINE TO lt_table ASSIGNING <fs_struct>.
            READ DATASET im_v_filepath INTO <fs_struct>.
            IF sy-subrc IS NOT INITIAL.
              DELETE lt_table INDEX sy-tabix.
              EXIT.
            ENDIF.
          ENDDO.
        ELSE.

          MESSAGE e003 INTO lv_message WITH im_v_filepath. "File not found
          ch_cl_log_handle->capture_and_add_message( ).
          RAISE internal_error.
        ENDIF.
      CATCH cx_root INTO lo_cx_root.                       "#EC CATCH_ALL

*        MOVE lo_cx_root->get_longtext( ) TO lv_message_string.
          MESSAGE e004 INTO lv_message WITH im_v_filepath. "File read error
          ch_cl_log_handle->capture_and_add_message( ).
          RAISE internal_error.

      ENDTRY.

    TRY.
        CLOSE DATASET im_v_filepath.
    CATCH cx_sy_file_close INTO lo_cx_root.
*        lv_message = lo_cx_root->get_longtext( ).
        MESSAGE e004 INTO lv_message WITH im_v_filepath. "File read error
        ch_cl_log_handle->capture_and_add_message( ).
        RAISE internal_error.

    ENDTRY.

    ex_i_table[] = lt_table[].

  ELSE.
     RAISE internal_error.
  ENDIF.

endmethod.


method ONLINE_FILE_SELECTION.

  DATA: lt_filetable TYPE filetable,
        ls_file      TYPE file_table,
        lv_rc        TYPE i,
        l_v_title    TYPE string.

  l_v_title = text-t01.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      multiselection    = abap_false
      window_title      = l_v_title
    CHANGING
      file_table        = lt_filetable
      rc                = lv_rc
    EXCEPTIONS
      OTHERS            = 1 ).

  IF sy-subrc EQ 0.
    READ TABLE lt_filetable INTO ls_file INDEX 1.
    IF sy-subrc = 0.
       re_v_filepath = ls_file-filename.
    ENDIF.
  ENDIF.


endmethod.


method ONLINE_FOLDER_SELECTION.

DATA: l_v_folder         TYPE string.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    CHANGING
      selected_folder      = l_v_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    re_v_folder = l_v_folder.
  ENDIF.


endmethod.


method TABLE_TO_FILE_DOWNLOAD.

  DATA: lv_message_string      TYPE BAPI_MSG.
  DATA: lv_string_line         TYPE string,
        lv_filepath            TYPE string.

  DATA lo_cx_root TYPE REF TO cx_root.



  CASE im_v_mode.

    WHEN 'O'. "Online

      lv_filepath = im_v_filepath.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          filename                = lv_filepath
          filetype                = 'ASC'
          trunc_trailing_blanks   = space
*          write_lf                = space
          codepage                = '4110' "UTF-8 Unicode
        CHANGING
          data_tab                = ch_i_filetable                     " Transfer table
        EXCEPTIONS
          file_write_error        = 1                    " Cannot write to FILE
          no_batch                = 2                    " Front-End Function Cannot Be Executed in Backgrnd
          gui_refuse_filetransfer = 3                    " Incorrect Front End
          invalid_type            = 4                    " Invalid value for parameter FILETYPE
          no_authority            = 5                    " No Download Authorization
          unknown_error           = 6                    " Unknown Error
          header_not_allowed      = 7                    " Header not Allowed
          separator_not_allowed   = 8                    " Separator not Allowed
          filesize_not_allowed    = 9                    " FILE Size Must Not Be Specified
          header_too_long         = 10                   " Header Information Currently Restricted to 1023 Bytes
          dp_error_create         = 11                   " Cannot create DataProvider
          dp_error_send           = 12                   " Error Sending Data with DataProvider
          dp_error_write          = 13                   " Error Writing Data with DataProvider
          unknown_dp_error        = 14                   " Error Calling Data Provider
          access_denied           = 15                   " Access to FILE Denied
          dp_out_of_memory        = 16                   " Not Enough Memory in DataProvider
          disk_full               = 17                   " Storage Medium full
          dp_timeout              = 18                   " Timeout of DataProvider
          file_not_found          = 19                   " Cannot Find FILE
          dataprovider_exception  = 20                   " General Exception Error in DataProvider
          control_flush_error     = 21                   " Error in Control Framework
          not_supported_by_gui    = 22                   " GUI does not support this
          error_no_gui            = 23                   " GUI not Available
          OTHERS                  = 24.
      IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ch_cl_log_handle->capture_and_add_message( ).
          RAISE internal_error.
      ENDIF.


      MESSAGE s012 WITH im_v_filepath INTO lv_message_string.
      ch_cl_log_handle->capture_and_add_message( ).

    WHEN 'B'. "Background
      TRY.
          OPEN DATASET im_v_filepath FOR OUTPUT IN TEXT MODE ENCODING DEFAULT
               MESSAGE lv_message_string.

          IF sy-subrc IS INITIAL.
            LOOP AT ch_i_filetable INTO lv_string_line.
              TRANSFER lv_string_line TO im_v_filepath.
            ENDLOOP.
          ELSE.
            MESSAGE e006 WITH im_v_filepath INTO lv_message_string. "Unable to read from directory
            ch_cl_log_handle->capture_and_add_message( ).
            RAISE internal_error.
          ENDIF.
        CATCH cx_root INTO lo_cx_root.
          ch_cl_log_handle->capture_and_add_message( ).
          MESSAGE e004 INTO lv_message_string.  " File read error
          ch_cl_log_handle->capture_and_add_message( ).
          RAISE internal_error.
      ENDTRY.

      TRY.
          CLOSE DATASET im_v_filepath.
          IF sy-subrc IS INITIAL.
            MESSAGE S012 WITH im_v_filepath INTO lv_message_string. "File generation success
            ch_cl_log_handle->capture_and_add_message( ).
          ELSE.
            MESSAGE E008 WITH im_v_filepath INTO lv_message_string. "File record error
            ch_cl_log_handle->capture_and_add_message( ).
            RAISE internal_error.
          ENDIF.

        CATCH cx_sy_file_close INTO lo_cx_root.
          ch_cl_log_handle->capture_and_add_message( ).
          MESSAGE e004 INTO lv_message_string.  " File read error
          ch_cl_log_handle->capture_and_add_message( ).
          RAISE internal_error.
      ENDTRY.

    WHEN OTHERS.  "Invalid mode
      RAISE internal_error.
   ENDCASE.


endmethod.
ENDCLASS.
