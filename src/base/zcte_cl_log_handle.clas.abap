class ZCTE_CL_LOG_HANDLE definition
  public
  final
  create public .

public section.
*"* public components of class ZCTE_CL_LOG_HANDLE
*"* do not include other source files here!!!

  methods CAPTURE_AND_ADD_MESSAGE .
  methods ADD_FREE_TEXT
    importing
      !IM_MSGTYP type SYMSGTY
      !IM_MESSAGE type BAPI_MSG
      !IM_CONTEXT type BAL_S_CONT optional .
  methods DISPLAY_LOG
    exceptions
      DISPLAY_FAILED .
  methods GET_LOG_HANDLE
    returning
      value(RE_LOG_HANDLE) type BALLOGHNDL .
  methods SET_DISPLAY_PROFILE .
  methods CONSTRUCTOR
    importing
      !IM_OBJECT type BALOBJ_D
      !IM_SUBOBJECT type BALSUBOBJ
      !IM_EXTNUMBER type BALNREXT
    exceptions
      NOT_CREATED .
  methods SET_OBJECT
    importing
      !IM_S_CONTEXT type ANY
      !IM_REFTABNAME type TABNAME16 .
  type-pools ABAP .
  class-methods DISPLAY_MULTIPLE_LOGS
    importing
      !IM_V_PROFILE type ABAP_BOOL optional
      !IM_I_LOGS type BAL_T_LOGH
    exceptions
      DISPLAY_FAILED .
  methods SAVE_LOG
    returning
      value(RE_LOGNUMBER) type BALOGNR .
  class-methods CAPTURE_MESSAGE
    returning
      value(RE_MESSAGE) type ZCTE_S002 .
  methods ADD_MESSAGE
    importing
      !IM_S_MESSAGE type ZCTE_S002
      !IM_V_DETLEVEL type BALLEVEL optional .
protected section.
private section.
*"* private components of class ZCTE_CL_LOG_HANDLE
*"* do not include other source files here!!!

  data G_LOG_HANDLE type BALLOGHNDL .
  data G_S_DISPLAY_PROFILE type BAL_S_PROF .
  data G_S_FCAT type BAL_S_FCAT .
ENDCLASS.



CLASS ZCTE_CL_LOG_HANDLE IMPLEMENTATION.


method ADD_FREE_TEXT.

  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_msgty       = im_msgtyp
      i_text        = im_message
      i_s_context   = im_context
    EXCEPTIONS
      log_not_found = 0
      OTHERS        = 1.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


endmethod.


method ADD_MESSAGE.

  DATA: ls_message TYPE bal_s_msg.

  CLEAR ls_message.

  ls_message-msgty    = im_s_message-msgty.
  ls_message-msgid    = im_s_message-msgid.
  ls_message-msgno    = im_s_message-msgno.
  ls_message-msgv1    = im_s_message-msgv1.
  ls_message-msgv2    = im_s_message-msgv2.
  ls_message-msgv3    = im_s_message-msgv3.
  ls_message-msgv4    = im_s_message-msgv4.
  ls_message-detlevel = im_v_detlevel.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      I_S_MSG                   = ls_message
   EXCEPTIONS
     LOG_NOT_FOUND             = 1
     MSG_INCONSISTENT          = 2
     LOG_IS_FULL               = 3
     OTHERS                    = 4.

  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.


endmethod.


method CAPTURE_AND_ADD_MESSAGE.

  DATA: ls_message TYPE bal_s_msg.

  CLEAR ls_message.

  ls_message-msgty   = sy-msgty.
  ls_message-msgid   = sy-msgid.
  ls_message-msgno   = sy-msgno.
  ls_message-msgv1   = sy-msgv1.
  ls_message-msgv2   = sy-msgv2.
  ls_message-msgv3   = sy-msgv3.
  ls_message-msgv4   = sy-msgv4.

  CALL FUNCTION 'BAL_LOG_MSG_ADD'
    EXPORTING
      I_S_MSG                   = ls_message
   EXCEPTIONS
     LOG_NOT_FOUND             = 1
     MSG_INCONSISTENT          = 2
     LOG_IS_FULL               = 3
     OTHERS                    = 4.

  IF SY-SUBRC <> 0.
*   Implement suitable error handling here
  ENDIF.



endmethod.


method CAPTURE_MESSAGE.

  DATA: lv_message TYPE bapi_msg.

  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO lv_message.

  CLEAR re_message.
  re_message-msgty   = sy-msgty.
  re_message-msgid   = sy-msgid.
  re_message-msgno   = sy-msgno.
  re_message-msgv1   = sy-msgv1.
  re_message-msgv2   = sy-msgv2.
  re_message-msgv3   = sy-msgv3.
  re_message-msgv4   = sy-msgv4.
  re_message-msgtext = lv_message.


endmethod.


method CONSTRUCTOR.

  DATA: ls_log TYPE bal_s_log.

  ls_log-extnumber = im_extnumber.
  ls_log-object    = im_object.
  ls_log-subobject = im_subobject.
  ls_log-aluser    = sy-uname.
  ls_log-alprog    = sy-repid.


  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
      e_log_handle = g_log_handle
    EXCEPTIONS
      OTHERS       = 1.

  IF sy-subrc NE 0.
    RAISE not_created.
  ENDIF.







endmethod.


method DISPLAY_LOG.

  DATA: lt_loghndl TYPE bal_t_logh.

  APPEND g_log_handle TO lt_loghndl.

  IF NOT g_s_display_profile IS INITIAL.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = g_s_display_profile
        i_t_log_handle       = lt_loghndl
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc NE 0.
       RAISE display_failed.
    ENDIF.

  ELSE.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_t_log_handle       = lt_loghndl
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.
    IF sy-subrc NE 0.
       RAISE display_failed.
    ENDIF.

  ENDIF.




endmethod.


method DISPLAY_MULTIPLE_LOGS.


DATA: L_S_DISPLAY_PROFILE   Type  BAL_S_PROF,
      L_S_FCAT              Type  BAL_S_FCAT.


  IF im_v_profile EQ abap_true.

*    create a display profile
     WRITE 'Processing Log' TO l_s_display_profile-root_text.

     l_s_display_profile-head_size = 57.
     l_s_display_profile-show_all = 'X'.
     l_s_display_profile-exp_level = 1.

     CLEAR l_s_fcat.
     l_s_fcat-ref_table = 'BAL_S_SHOW'.
     l_s_fcat-ref_field = 'ALDATE'.
     l_s_fcat-cltxt_add = 'X'.
     l_s_fcat-coltext   = 'Run date'.
     APPEND l_s_fcat TO l_s_display_profile-lev1_fcat.
     CLEAR l_s_fcat.
     l_s_fcat-ref_table = 'BAL_S_SHOW'.
     l_s_fcat-ref_field = 'ALTIME'.
     APPEND l_s_fcat TO l_s_display_profile-lev1_fcat.
     CLEAR l_s_fcat.

     l_s_fcat-ref_table = 'ZCTE_FIS001'.
     l_s_fcat-ref_field = 'REPORT_TYPE'.
     l_s_fcat-coltext   = 'Report type'.
     l_s_fcat-cltxt_add = 'X'.
     APPEND l_s_fcat TO l_s_display_profile-lev2_fcat.
     CLEAR l_s_fcat.
     l_s_fcat-ref_table = 'ZCTE_FIS001'.
     l_s_fcat-ref_field = 'REPORT_ID'.
     l_s_fcat-coltext   = 'Nº'.
     l_s_fcat-cltxt_add = 'X'.
     APPEND l_s_fcat TO l_s_display_profile-lev3_fcat.

     l_s_fcat-ref_table = 'BAL_S_SHOW'.
     l_s_fcat-ref_field = 'T_MSG'.
     l_s_fcat-outputlen = 70.

     APPEND l_s_fcat TO l_s_display_profile-mess_fcat.
     l_s_display_profile-disvariant-report = sy-repid.
*    when you use also other ALV lists in your report,
*    please specify a handle to distinguish between the display
*    variants of these different lists, e.g:
     l_s_display_profile-disvariant-handle = 'LOG'.


      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_s_display_profile  = l_s_display_profile
          i_t_log_handle       = im_i_logs
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          no_data_available    = 3
          no_authority         = 4
          OTHERS               = 5.
      IF sy-subrc NE 0.
         RAISE display_failed.
      ENDIF.

    ELSE.

      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_t_log_handle       = im_i_logs
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          no_data_available    = 3
          no_authority         = 4
          OTHERS               = 5.
      IF sy-subrc NE 0.
         RAISE display_failed.
      ENDIF.

    ENDIF.


endmethod.


method GET_LOG_HANDLE.

  re_log_handle = g_log_handle.

endmethod.


method SAVE_LOG.

  DATA lt_log        TYPE  bal_t_logh.
  DATA lt_lognumbers TYPE BAL_T_LGNM.

  FIELD-SYMBOLS: <l_fs_lognumber> TYPE bal_s_lgnm.
  APPEND g_log_handle TO lt_log.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_save_all       = ' '
      i_t_log_handle   = lt_log
    IMPORTING
      e_new_lognumbers = lt_lognumbers
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.

    IF NOT lt_lognumbers IS INITIAL.
       READ TABLE lt_lognumbers INDEX 1 ASSIGNING <l_fs_lognumber>.
       IF sy-subrc EQ 0.
          re_lognumber = <l_fs_lognumber>-lognumber.
       ENDIF.
    ENDIF.
  ENDIF.


endmethod.


method SET_DISPLAY_PROFILE.


*    create a display profile
     WRITE 'Processing Log' TO g_s_display_profile-root_text.

     g_s_display_profile-head_size = 57.
     g_s_display_profile-show_all = 'X'.
     g_s_display_profile-exp_level = 1.

     CLEAR g_s_fcat.
     g_s_fcat-ref_table = 'BAL_S_SHOW'.
     g_s_fcat-ref_field = 'ALDATE'.
     g_s_fcat-cltxt_add = 'X'.
     g_s_fcat-coltext   = 'Run date'.
     APPEND g_s_fcat TO g_s_display_profile-lev1_fcat.
     CLEAR g_s_fcat.
     g_s_fcat-ref_table = 'BAL_S_SHOW'.
     g_s_fcat-ref_field = 'ALTIME'.
     APPEND g_s_fcat TO g_s_display_profile-lev1_fcat.
     CLEAR g_s_fcat.

     g_s_fcat-ref_table = 'ZCTE_S001'.
     g_s_fcat-ref_field = 'LEVEL1'.
     g_s_fcat-coltext   = 'Level 01'.
     g_s_fcat-cltxt_add = 'X'.
*     g_s_fcat-is_extern  = 'X'.
     APPEND g_s_fcat TO g_s_display_profile-lev2_fcat.

     g_s_fcat-ref_table = 'ZCTE_S001'.
     g_s_fcat-ref_field = 'LEVEL2'.
     g_s_fcat-coltext   = 'Level 02'.
     g_s_fcat-cltxt_add = 'X'.
*     g_s_fcat-is_extern  = 'X'.
     APPEND g_s_fcat TO g_s_display_profile-lev3_fcat.

     g_s_fcat-ref_table = 'ZCTE_S001'.
     g_s_fcat-ref_field = 'LEVEL3'.
     g_s_fcat-coltext   = 'Level 03'.
     g_s_fcat-cltxt_add = 'X'.
*     g_s_fcat-is_extern  = 'X'.
     APPEND g_s_fcat TO g_s_display_profile-lev4_fcat.

     g_s_fcat-ref_table = 'ZCTE_S001'.
     g_s_fcat-ref_field = 'LEVEL4'.
     g_s_fcat-coltext   = 'Level 04'.
     g_s_fcat-cltxt_add = 'X'.
*     g_s_fcat-is_extern  = 'X'.
     APPEND g_s_fcat TO g_s_display_profile-lev5_fcat.

     g_s_fcat-ref_table = 'ZCTE_S001'.
     g_s_fcat-ref_field = 'LEVEL5'.
     g_s_fcat-coltext   = 'Level 05'.
     g_s_fcat-cltxt_add = 'X'.
*     g_s_fcat-is_extern  = 'X'.
     APPEND g_s_fcat TO g_s_display_profile-lev6_fcat.

     g_s_fcat-ref_table = 'ZCTE_S001'.
     g_s_fcat-ref_field = 'LEVEL6'.
     g_s_fcat-coltext   = 'Level 06'.
     g_s_fcat-cltxt_add = 'X'.
*     g_s_fcat-is_extern  = 'X'.
     APPEND g_s_fcat TO g_s_display_profile-lev7_fcat.


     g_s_fcat-ref_table = 'BAL_S_SHOW'.
     g_s_fcat-ref_field = 'T_MSG'.
     g_s_fcat-outputlen = 70.

     APPEND g_s_fcat TO g_s_display_profile-mess_fcat.
     g_s_display_profile-disvariant-report = sy-repid.
*    when you use also other ALV lists in your report,
*    please specify a handle to distinguish between the display
*    variants of these different lists, e.g:
     g_s_display_profile-disvariant-handle = 'LOG'.


endmethod.


method SET_OBJECT.

DATA: l_s_mdef             TYPE bal_s_mdef.


*    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_GET'
*         IMPORTING
*              e_s_msg_defaults = l_s_mdef

    l_s_mdef-log_handle      = g_log_handle.
    l_s_mdef-context-tabname = im_reftabname.
    l_s_mdef-context-value   = im_s_context.

    CALL FUNCTION 'BAL_GLB_MSG_DEFAULTS_SET'
         EXPORTING
              i_s_msg_defaults = l_s_mdef
         EXCEPTIONS
              OTHERS           = 0.



endmethod.
ENDCLASS.
