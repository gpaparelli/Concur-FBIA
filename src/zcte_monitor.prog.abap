*&---------------------------------------------------------------------*
*& Report  ZCTE_MONITOR
*&---------------------------------------------------------------------*
*&                  SAP BRAZIL
*&   DEVELOPER:  Gilliatti Paparelli  2020/04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZCTE_MONITOR LINE-SIZE 100 MESSAGE-ID zcte_base.

*-----------------------------------------------------------------------
* Includes
*-----------------------------------------------------------------------

TYPE-POOLS: SLIS.

*-----------------------------------------------------------------------
* Types
*-----------------------------------------------------------------------


TYPES: BEGIN OF ty_zcte_fit003_disp.
        INCLUDE STRUCTURE zcte_fit003.
TYPES:  trans_type         TYPE char20,
        posting_status     TYPE icon-id,
        change_status      TYPE icon-id,
        pmnt_status_text   TYPE char40,
       END   OF ty_zcte_fit003_disp.

TYPES: BEGIN OF ty_disp_header,
         entity_id      TYPE zcte_entity_id,
         report_name    TYPE char40,
         emp_name       TYPE char64,
         vendor_id      TYPE char10,
         ad_amount      TYPE dmbtr,
         acc_bukrs      TYPE bukrs,
         acc_belnr      TYPE belnr_d,
         acc_gjahr      TYPE gjahr,
       END   OF ty_disp_header.

TYPES: BEGIN OF ty_disp_item,
         entry_id         TYPE zctefi_entry_id,
         journal_key      TYPE zctefi_journal_key,
         expense_name     TYPE char64,
         expense_date     TYPE dats,
         meio_pgto        TYPE char64,
         conta            TYPE char10,
         pessoal          TYPE char1,
         montante         TYPE dmbtr,
         moeda            TYPE waers,
       END   OF ty_disp_item.

TYPES: BEGIN OF ty_disp_alloc,
         entry_id         TYPE zctefi_entry_id,
         alloc_key        TYPE zctefi_alloc_key,
         perc             TYPE ZCTEFI_PERC,
         alloc1           TYPE char40,
         alloc2           TYPE char40,
         alloc3           TYPE char40,
         alloc4           TYPE char40,
         alloc5           TYPE char40,
         alloc6           TYPE char40,
         deb_cred         TYPE char1,
         montante         TYPE dmbtr,
         moeda            TYPE waers,
       END   OF ty_disp_alloc,

       BEGIN OF ty_disp_alloc_name,
         alloc1          TYPE zcte_t002-display_name,
         alloc2          TYPE zcte_t002-display_name,
         alloc3          TYPE zcte_t002-display_name,
         alloc4          TYPE zcte_t002-display_name,
         alloc5          TYPE zcte_t002-display_name,
         alloc6          TYPE zcte_t002-display_name,
       END OF ty_disp_alloc_name,

       BEGIN OF ty_tabs,
         subscreen   LIKE sy-dynnr,
         prog        LIKE sy-repid,
         pressed_tab LIKE sy-ucomm,
      END OF ty_tabs.


*-----------------------------------------------------------------------
* Tables
*-----------------------------------------------------------------------

DATA: "gt_zcte_t001           TYPE STANDARD TABLE OF zcte_t001,
      gt_zcte_t002           TYPE STANDARD TABLE OF zcte_t002,
      gt_zcte_fit001         TYPE STANDARD TABLE OF zcte_fit001,
      gt_zcte_fit001_disp    TYPE STANDARD TABLE OF zcte_fit001,
      gt_zcte_fit003         TYPE STANDARD TABLE OF zcte_fit003,
      gt_zcte_fit003_disp    TYPE STANDARD TABLE OF ty_zcte_fit003_disp,
      gt_zcte_fit005         TYPE STANDARD TABLE OF zcte_fit005,
      gt_zcte_fit005_disp    TYPE STANDARD TABLE OF zcte_fit005,
      gt_zcte_fit005_upd     TYPE STANDARD TABLE OF zcte_fit005,
      gt_zcte_fit007         TYPE STANDARD TABLE OF zcte_fit007,
      gt_zcte_fit007_disp    TYPE STANDARD TABLE OF zcte_fit007,
      gt_disp_item           TYPE STANDARD TABLE OF ty_disp_item,
      gt_disp_alloc          TYPE STANDARD TABLE OF ty_disp_alloc,
      gt_alloc               TYPE STANDARD TABLE OF ty_disp_alloc.

** Estruturas para o alv grid

DATA: t_fieldcat  TYPE lvc_t_fcat,
      t_fieldcat2 TYPE lvc_t_fcat,
      t_fieldcat3 TYPE lvc_t_fcat,
      afield      TYPE lvc_s_fcat,
      afield2     TYPE slis_fieldcat_alv,
      t_listcat   TYPE SLIS_T_FIELDCAT_ALV,
      t_layout    TYPE lvc_s_layo,
      t_layout2   TYPE lvc_s_layo,
      t_cab       TYPE SLIS_LISTHEADER OCCURS 0 WITH HEADER LINE,
      t_events    TYPE SLIS_ALV_EVENT OCCURS 0 WITH HEADER LINE,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      g_custom_container2  TYPE REF TO cl_gui_custom_container,
      g_custom_container3  TYPE REF TO cl_gui_custom_container,
      g_container          TYPE scrfname VALUE 'ALV_CONTAINER',
      g_container2         TYPE scrfname VALUE 'LOG_CONTAINER',
      g_container3         TYPE scrfname VALUE 'LOG_CONTAINER2',
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_grid2              TYPE REF TO cl_gui_alv_grid,
      g_grid3              TYPE REF TO cl_gui_alv_grid.

DATA: g_s_display_profile  TYPE bal_s_prof,
      g_subscreen_prog     TYPE sy-repid,
      g_subscreen_dynp     TYPE sy-dynnr.


DATA: go_zcte_utils TYPE REF TO zcte_cl_utils.

*-----------------------------------------------------------------------
* Estruturas
*-----------------------------------------------------------------------

  DATA: gs_disp_header     TYPE ty_disp_header,
        gs_disp_item       TYPE ty_disp_item,
        gs_disp_alloc      TYPE ty_disp_alloc,
        gs_tabs            TYPE ty_tabs,
        gs_disp_alloc_name TYPE ty_disp_alloc_name.

  CONTROLS: tc_item  TYPE TABLEVIEW USING SCREEN '0200',
            tc_alloc TYPE TABLEVIEW USING SCREEN '0200',
            ts_tabs  TYPE TABSTRIP.

*-----------------------------------------------------------------------
* Variaveis
*-----------------------------------------------------------------------

  DATA: gv_rpt_type    TYPE zcte_fit003-report_type,
        gv_srch_days   TYPE zctefi_days_amount VALUE '15',
        gv_entity      TYPE zcte_t001-entity_id,
        gv_entity_desc TYPE zcte_t001-entity_desc,
        gv_okcode      TYPE sy-ucomm,
        gv_screen      TYPE sy-dynnr,
        gv_disp        TYPE char10.

*-----------------------------------------------------------------------
* Constantes
*-----------------------------------------------------------------------
  CONSTANTS:
    BEGIN OF c_tabs,
      tab1 LIKE sy-ucomm VALUE '&TS_DOC',
      tab2 LIKE sy-ucomm VALUE '&TS_LOG',
    END OF c_tabs.

*-----------------------------------------------------------------------
* Classes
*-----------------------------------------------------------------------

CLASS lcl_event_handler DEFINITION .
  PUBLIC SECTION .
    METHODS:

    HANDLE_HOTSPOT_CLICK_G1
        FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID E_COLUMN_ID,

    HANDLE_DOUBLE_CLICK_G1
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW E_COLUMN,

    HANDLE_HOTSPOT_CLICK_G2
        FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID E_COLUMN_ID,

    HANDLE_DOUBLE_CLICK_G2
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW E_COLUMN,

    HANDLE_HOTSPOT_CLICK_G3
        FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW_ID E_COLUMN_ID,

    HANDLE_DOUBLE_CLICK_G3
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW E_COLUMN,

    HANDLE_TOOLBAR
    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

    HANDLE_USER_COMMAND
        FOR EVENT user_command OF cl_gui_alv_grid
            IMPORTING e_ucomm.


ENDCLASS.             "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD HANDLE_DOUBLE_CLICK_G1.
    PERFORM drill_down USING e_row e_column 'G1'.
  ENDMETHOD.

  METHOD HANDLE_HOTSPOT_CLICK_G1.
    PERFORM drill_down USING e_row_id e_column_id 'G1'.
  ENDMETHOD.

  METHOD HANDLE_DOUBLE_CLICK_G2.
    PERFORM drill_down USING e_row e_column 'G2'.
  ENDMETHOD.

  METHOD HANDLE_HOTSPOT_CLICK_G2.
    PERFORM drill_down USING e_row_id e_column_id 'G2'.
  ENDMETHOD.

  METHOD HANDLE_DOUBLE_CLICK_G3.
    PERFORM drill_down USING e_row e_column 'G3'.
  ENDMETHOD.

  METHOD HANDLE_HOTSPOT_CLICK_G3.
    PERFORM drill_down USING e_row_id e_column_id 'G3'.
  ENDMETHOD.

  METHOD handle_toolbar.
    PERFORM handle_toolbar USING e_object ."e_interactive.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM handle_user_command USING e_ucomm.
  ENDMETHOD.


ENDCLASS.       "LCL_EVENT_HANDLER IMPLEMENTATION

DATA: g_handler TYPE REF TO lcl_event_handler.

*-----------------------------------------------------------------------
* Tela de seleção
*-----------------------------------------------------------------------

SELECTION-SCREEN BEGIN OF BLOCK bl_1 WITH FRAME TITLE text-020.
*PARAMETERS:
SELECTION-SCREEN uline.

*SKIP.

SELECTION-SCREEN uline.

SELECTION-SCREEN END OF BLOCK bl_1.


*&---------------------------------------------------------------------*
*&    Main
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM: f_inicializa_display.

  PERFORM: f_seleciona_dados,
           monta_fieldcat,
           chama_alv_grid.

END-OF-SELECTION.

  CALL SCREEN 100.

*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
FORM f_seleciona_dados.


 DATA: lt_pmnt_status_text    TYPE STANDARD TABLE OF dd07v,
       lt_zcte_t001           TYPE STANDARD TABLE OF zcte_t001.

  CONSTANTS: l_c_red_light    TYPE icon-id VALUE '@0A@',
             l_c_green_light  TYPE icon-id VALUE '@08@',
             l_c_icon_warning TYPE icon-id VALUE '@AH@'.

  DATA: lv_srch_date TYPE dats.
  DATA: lv_clause TYPE char255.

  FIELD-SYMBOLS: <l_fs_zcte_fit003_disp>  TYPE     ty_zcte_fit003_disp,
                 <l_fs_pmnt_status_text>  TYPE     dd07v,
                 <l_fs_zcte_t001>         TYPE     zcte_t001.

  IF NOT go_zcte_utils IS BOUND.
     CREATE OBJECT go_zcte_utils.
  ENDIF.

  lt_zcte_t001[] = go_zcte_utils->get_entity_list( ).
  IF NOT lt_zcte_t001 IS INITIAL.
    READ TABLE lt_zcte_t001 INDEX 1 ASSIGNING <l_fs_zcte_t001>.
    IF sy-subrc EQ 0.

       CALL METHOD go_zcte_utils->set_entity
       EXPORTING
         im_v_entity = <l_fs_zcte_t001>-entity_id
       EXCEPTIONS
         not_found = 1
         OTHERS    = 2.

       IF sy-subrc EQ 0.
          gv_entity      = go_zcte_utils->gs_zcte_t001-entity_id.
          gv_entity_desc = go_zcte_utils->gs_zcte_t001-entity_desc.
       ENDIF.
    ENDIF.
  ENDIF.


  IF gt_zcte_t002 IS INITIAL.
      SELECT *
        FROM zcte_t002 INTO TABLE gt_zcte_t002.
      IF sy-subrc EQ 0.
        SORT gt_zcte_t002 BY org_unit.
      ENDIF.
  ENDIF.


  lv_srch_date  = sy-datum - gv_srch_days.

  REFRESH: gt_zcte_fit001,
           gt_zcte_fit003.

  CLEAR lv_clause.

  lv_clause = 'entity_id = gv_entity'.

  IF gv_srch_days GT 1.
     CONCATENATE lv_clause
                 'AND batch_date_pro GE lv_srch_date'
            INTO lv_clause SEPARATED BY space.
  ENDIF.

  SELECT *
    FROM zcte_fit001
    INTO TABLE gt_zcte_fit001
    WHERE (lv_clause).

  IF sy-subrc EQ 0.

  lv_clause = 'entity_id = gt_zcte_fit001-entity_id AND batch_id EQ gt_zcte_fit001-batch_id'.

  IF gv_rpt_type IS NOT INITIAL.
     CONCATENATE lv_clause
                 'AND report_type EQ gv_rpt_type'
     INTO lv_clause SEPARATED BY space.
  ENDIF.


*  * Get domain status for payment status
   CALL FUNCTION 'DD_DOMVALUES_GET'
     EXPORTING
       domname              = 'ZCTEFI_PMNT_STATUS'
       text                 = abap_true
       langu                = sy-langu
     TABLES
       dd07v_tab            = lt_pmnt_status_text
    EXCEPTIONS
      WRONG_TEXTFLAG       = 1
      OTHERS               = 2.

   IF sy-subrc EQ 0.
      SORT lt_pmnt_status_text BY domvalue_l.
   ENDIF.


    SELECT *
      FROM zcte_fit003
      INTO TABLE gt_zcte_fit003
      FOR ALL ENTRIES IN gt_zcte_fit001
      WHERE (lv_clause).

    IF sy-subrc EQ 0.

       SELECT *
         FROM zcte_fit005
         INTO TABLE gt_zcte_fit005
         FOR ALL ENTRIES IN gt_zcte_fit003
         WHERE entity_id   = gt_zcte_fit003-entity_id
           AND report_id   = gt_zcte_fit003-report_id
           AND report_key  = gt_zcte_fit003-report_key
           AND report_type = gt_zcte_fit003-report_type
           AND batch_id    = gt_zcte_fit003-batch_id.

        IF sy-subrc EQ 0.
           SORT gt_zcte_fit005 BY entity_id report_id report_key report_type batch_id.
        ENDIF.


       gt_zcte_fit003_disp = gt_zcte_fit003.
       LOOP AT gt_zcte_fit003_disp ASSIGNING <l_fs_zcte_fit003_disp>.
          IF <l_fs_zcte_fit003_disp>-acc_belnr IS INITIAL.
             <l_fs_zcte_fit003_disp>-posting_status = l_c_red_light.
          ELSE.
             <l_fs_zcte_fit003_disp>-posting_status = l_c_green_light.
          ENDIF.

          IF <l_fs_zcte_fit003_disp>-report_type EQ zcte_cl_acc_integration=>c_cashadv.
              <l_fs_zcte_fit003_disp>-trans_type = 'Cash Advance'.
          ELSEIF <l_fs_zcte_fit003_disp>-report_type EQ zcte_cl_acc_integration=>c_expense.
              <l_fs_zcte_fit003_disp>-trans_type = 'Expense Report'.
          ENDIF.

          READ TABLE lt_pmnt_status_text ASSIGNING <l_fs_pmnt_status_text>
                                         WITH KEY domvalue_l = <l_fs_zcte_fit003_disp>-pmnt_status
                                         BINARY SEARCH.
          IF sy-subrc EQ 0.
             <l_fs_zcte_fit003_disp>-pmnt_status_text = <l_fs_pmnt_status_text>-ddtext.
          ENDIF.



          READ TABLE gt_zcte_fit005      WITH KEY entity_id    =  <l_fs_zcte_fit003_disp>-entity_id
                                                  report_id    =  <l_fs_zcte_fit003_disp>-report_id
                                                  report_key   =  <l_fs_zcte_fit003_disp>-report_key
                                                  report_type  =  <l_fs_zcte_fit003_disp>-report_type
                                                  batch_id     =  <l_fs_zcte_fit003_disp>-batch_id
                                     BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
             <l_fs_zcte_fit003_disp>-change_status = l_c_icon_warning.
          ENDIF.
       ENDLOOP.


       SELECT *
         FROM zcte_fit007
         INTO TABLE gt_zcte_fit007
         FOR ALL ENTRIES IN gt_zcte_fit003
         WHERE report_id   EQ gt_zcte_fit003-report_id
           AND report_key  EQ gt_zcte_fit003-report_key
           AND report_type EQ gt_zcte_fit003-report_type.
        IF sy-subrc EQ 0.
           SORT gt_zcte_fit007 BY report_id report_key report_type.
        ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " f_seleciona_dados

*&---------------------------------------------------------------------*
*&      Form  monta_fieldcat
*&---------------------------------------------------------------------*
FORM monta_fieldcat.

  DATA: v_pos TYPE i,
        v_tab TYPE i.

  CLEAR v_pos.
  REFRESH t_fieldcat.

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'POSTING_STATUS'   " Campo
                                'Status' " texto cabeçalho da coluna
                                '1'.     " Processar na primeira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'BATCH_ID' " Campo
                                'Batch ID' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'TRANS_TYPE'   " Campo
                                'Report type' " texto cabeçalho da coluna
                                '1'.     " Processar na primeira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'REPORT_ID' " Campo
                                'Report ID' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab


  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'REPORT_KEY' " Campo
                                'Report Key' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab


  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'REPORT_NAME' " Campo
                                'Report Name' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab


  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'ACC_BUKRS' " Campo
                                'Company Code' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab


  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'ACC_BELNR' " Campo
                                'Document' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'ACC_GJAHR' " Campo
                                'Year' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'CHANGE_STATUS' " Campo
                                'Doc. Changes' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'EMPLOYEE_ID' " Campo
                                'Employee ID' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'EMP_FIRST_NAME' " Campo
                                'Emp. first name' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'EMP_LAST_NAME' " Campo
                                'Last Name' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab


  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'PMNT_DATE' " Campo
                                'Payment Date' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

    ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'PMNT_AMOUNT' " Campo
                                'Paid amount' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

    ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'PMNT_CURR' " Campo
                                'Pmnt currency' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

    ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'PMNT_AUGBL' " Campo
                                'Clearing Document' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

    ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'PMNT_STATUS' " Campo
                                'Payment Status (code)' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab

    ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'PMNT_STATUS_TEXT' " Campo
                                'Payment Status' " texto cabeçalho da coluna
                                '1'.         " Processar na primeira tab


"==============================================================================

  CLEAR v_pos.

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'BATCH_ID' " Campo
                                'Batch ID' " texto cabeçalho da coluna
                                '2'     . " Processar na segunda tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'BATCH_LOG_ID' " Campo
                                'Batch Log ID' " texto cabeçalho da coluna
                                '2'        . " Processar na segunda tab


  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'BATCH_DATE_ID' " Campo
                                'Batch date' " texto cabeçalho da coluna
                                '2'.         " Processar na segunda tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'BATCH_DATE_PRO' " Campo
                                'Processing date' " texto cabeçalho da coluna
                                '2' .        " Processar na segunda tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'BATCH_TIME_PRO' " Campo
                                'Processing time' " texto cabeçalho da coluna
                                '2' .        " Processar na segunda tab


  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'BATCH_REPRO' " Campo
                                'Processing number' " texto cabeçalho da coluna
                                '2'.         " Processar na segunda tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'MANUAL_PROC' " Campo
                                'Manual Proc.' " texto cabeçalho da coluna
                                '2' .        " Processar na segunda tab



"==============================================================================

  CLEAR v_pos.

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'REPORT_ID' " Campo
                                'Report ID' " texto cabeçalho da coluna
                                '3'     . " Processar na terceira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'REPORT_KEY' " Campo
                                'Report KEY' " texto cabeçalho da coluna
                                '3'     . " Processar na terceira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'LOG_ID' " Campo
                                'Log ID' " texto cabeçalho da coluna
                                '3'     . " Processar na terceira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'LOG_DATE_PROC' " Campo
                                'Processing log date' " texto cabeçalho da coluna
                                '3'     . " Processar na terceira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'LOG_TIME_PROC' " Campo
                                'Processing log time' " texto cabeçalho da coluna
                                '3'     . " Processar na terceira tab

    ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'LOG_REPRO' " Campo
                                'Number of reprocessing' " texto cabeçalho da coluna
                                '3'     . " Processar na terceira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'MANUAL_PROC' " Campo
                                'Manual process' " texto cabeçalho da coluna
                                '3'     . " Processar na terceira tab

  ADD 1 TO v_pos.
  PERFORM f_fieldcat_fill USING v_pos    " Posição coluna
                                'PROC_USER' " Campo
                                'Processing User.' " texto cabeçalho da coluna
                                '3'     . " Processar na terceira tab


ENDFORM.                    " monta_fieldcat
*&---------------------------------------------------------------------*
*&      Form  f_fieldcat_fill
*&---------------------------------------------------------------------*
FORM f_fieldcat_fill USING p_pos
                           p_campo
                           p_cab
                           p_tab.

      CLEAR afield.
      afield-col_pos = p_pos.
      afield-fieldname = p_campo.
      afield-seltext = p_cab.
      afield-scrtext_l = p_cab.
      afield-scrtext_m = p_cab.
      afield-scrtext_s = p_cab.

      IF p_campo EQ 'ACC_BELNR' OR
         p_campo EQ 'BATCH_LOG_ID' OR
         p_campo EQ 'BATCH_ID' OR
         p_campo EQ 'REPORT_KEY' OR
         p_campo EQ 'LOG_ID' OR
         p_campo EQ 'CHANGE_STATUS'.

        afield-hotspot = abap_true.
      ENDIF.

      IF p_campo EQ 'MANUAL_PROC'.
        afield-checkbox = abap_true.
      ENDIF.

    IF p_tab EQ '1'.
      afield-emphasize = 'C300'.
      APPEND afield TO t_fieldcat.
    ELSEIF p_tab EQ '2'.
      afield-emphasize = 'C200'.
      APPEND afield TO t_fieldcat2.
    ELSE.
      afield-emphasize = 'C200'.
      APPEND afield TO t_fieldcat3.
    ENDIF.

ENDFORM.                    " f_fieldcat_fill

*&---------------------------------------------------------------------*
*&      Form  chama_alv_grid
*&---------------------------------------------------------------------*
FORM chama_alv_grid.

  DATA: EVENT_RECEIVER      TYPE REF TO lcl_event_handler .

  DATA: lt_sort      TYPE LVC_T_SORT,
        ls_sort      TYPE LVC_S_SORT.

  t_layout-zebra = abap_true.
  t_layout-CWIDTH_OPT = abap_true.


  ls_sort-spos = 1.
  ls_sort-fieldname = 'TRANS_TYPE'.
  ls_sort-down = abap_true.
  ls_sort-up   = abap_false.
  APPEND ls_sort TO lt_sort.

  ls_sort-spos = 2.
  ls_sort-fieldname = 'REPORT_KEY'.
  ls_sort-down = abap_false.
  ls_sort-up   = abap_true.
  APPEND ls_sort TO lt_sort.


  t_layout2-zebra = abap_true.
  t_layout2-CWIDTH_OPT = abap_true.
  t_layout2-no_toolbar = abap_true.

  CREATE OBJECT g_custom_container
  EXPORTING container_name = g_container.

  CREATE OBJECT g_custom_container2
  EXPORTING container_name = g_container2.

  CREATE OBJECT g_custom_container3
  EXPORTING container_name = g_container3.

**  Objeto que trata os drill downs
  CREATE OBJECT EVENT_RECEIVER.

  CREATE OBJECT g_grid
  EXPORTING i_parent = g_custom_container. "o_parent_grid.

  CREATE OBJECT g_grid2
  EXPORTING i_parent = g_custom_container2. "o_parent_grid2.

  CREATE OBJECT g_grid3
  EXPORTING i_parent = g_custom_container3.

*Chama o ALV Grid montado baseado no fieldcat e tabela de dados
  CALL METHOD g_grid->set_table_for_first_display
    EXPORTING
      is_layout       = t_layout
    CHANGING
      it_sort         = lt_sort
      it_outtab       = gt_zcte_fit003_disp[]
      it_fieldcatalog = t_fieldcat[].


  CALL METHOD g_grid2->set_table_for_first_display
    EXPORTING
      is_layout       = t_layout2
    CHANGING
      it_outtab       = gt_zcte_fit001_disp[]
      it_fieldcatalog = t_fieldcat2[].


  CALL METHOD g_grid3->set_table_for_first_display
    CHANGING
      it_outtab       = gt_zcte_fit007_disp[]
      it_fieldcatalog = t_fieldcat3[].

  SET HANDLER  EVENT_RECEIVER->HANDLE_DOUBLE_CLICK_G1
               EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK_G1
               EVENT_RECEIVER->HANDLE_TOOLBAR
               EVENT_RECEIVER->HANDLE_USER_COMMAND FOR g_grid.

  CALL METHOD g_grid->set_toolbar_interactive.

  SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK_G2 FOR g_grid2.
  SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK_G2 FOR g_grid2.

  SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK_G3 FOR g_grid3.
  SET HANDLER EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK_G3 FOR g_grid3.


ENDFORM.                    " chama_alv_grid

*&---------------------------------------------------------------------*
*&      Form  HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*
FORM handle_toolbar USING i_object TYPE REF TO cl_alv_event_toolbar_set .

  DATA: ls_toolbar TYPE stb_button.

  CLEAR ls_toolbar.
  ls_toolbar-function  = '&DET'.
  ls_toolbar-butn_type = 0.
*  ls_toolbar-icon =
*  ls_toolbar-quickinfo.
  ls_toolbar-text = 'Details'.
  ls_toolbar-disabled = abap_false.
  APPEND ls_toolbar TO i_object->mt_toolbar.

  CLEAR ls_toolbar.
  ls_toolbar-function  = '&REPRO'.
  ls_toolbar-butn_type = 0.
*  ls_toolbar-icon =
*  ls_toolbar-quickinfo.
  ls_toolbar-text = 'Reprocessing'.
  ls_toolbar-disabled = abap_false.
  APPEND ls_toolbar TO i_object->mt_toolbar.



ENDFORM.                    " HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM handle_user_command USING pu_ucomm    TYPE sy-ucomm.
*                        selfield TYPE slis_selfield.

  DATA: lv_resp TYPE c.

  DATA: lt_rows TYPE lvc_t_row,
        lt_cols TYPE zcte_fis002_t.

  FIELD-SYMBOLS: <l_fs_rows>         TYPE lvc_s_row,
                 <l_fs_zcte_fit003>  TYPE ty_zcte_fit003_disp.


*get selected row
  CALL METHOD g_grid->get_selected_rows
           IMPORTING et_index_rows = lt_rows.

  CALL METHOD cl_gui_cfw=>flush.


  IF NOT lt_rows IS INITIAL.
     READ TABLE lt_rows ASSIGNING <l_fs_rows> INDEX 1.
     READ TABLE gt_zcte_fit003_disp ASSIGNING <l_fs_zcte_fit003> INDEX <l_fs_rows>-index.
  ELSE.
    EXIT.
  ENDIF.


  CASE pu_ucomm.

    WHEN '&DET'.
      PERFORM f_recover_extract  TABLES lt_cols
                                 USING <l_fs_zcte_fit003>.

      IF <l_fs_zcte_fit003>-report_type EQ zcte_cl_acc_integration=>c_expense. "prestação de contas
         PERFORM f_display_details TABLES lt_cols
                                   USING <l_fs_zcte_fit003>.
      ELSEIF <l_fs_zcte_fit003>-report_type EQ zcte_cl_acc_integration=>c_cashadv. "Adto
         PERFORM f_ad_display_details TABLES lt_cols
                                      USING <l_fs_zcte_fit003>.
      ENDIF.

    WHEN '&REPRO'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question               = 'Confirm reprocessing?'
          display_cancel_button       = abap_false
       IMPORTING
         ANSWER                      = lv_resp
       EXCEPTIONS
         TEXT_NOT_FOUND              = 1
         OTHERS                      = 2.

      IF sy-subrc EQ 0 AND lv_resp EQ 1.

        PERFORM f_call_acc_report TABLES lt_cols
                                   USING <l_fs_zcte_fit003>.

        PERFORM f_refresh_all.
      ENDIF.

  ENDCASE.



ENDFORM.                    " user_command

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*

MODULE STATUS_0100 OUTPUT.

  PERFORM f_status_0100.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN '&SRCH'.

      PERFORM: f_refresh_all.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  drill_down
*&---------------------------------------------------------------------*
FORM drill_down using p_linha p_coluna p_grid.


  FIELD-SYMBOLS: <l_fs_zcte_fit001> TYPE zcte_fit001,
                 <l_fs_zcte_fit007> TYPE zcte_fit007,
                 <l_fs_zcte_fit005> TYPE zcte_fit005,
                 <l_fs_zcte_fit003> TYPE ty_zcte_fit003_disp.


  CASE p_grid.

  WHEN 'G1'.

      CASE p_coluna.

       WHEN 'BATCH_ID'.

            READ TABLE gt_zcte_fit003_disp ASSIGNING <l_fs_zcte_fit003> INDEX p_linha.
            gt_zcte_fit001_disp = gt_zcte_fit001.
            DELETE gt_zcte_fit001_disp WHERE batch_id NE <l_fs_zcte_fit003>-batch_id.
            SORT gt_zcte_fit001_disp BY batch_repro DESCENDING.

            PERFORM f_update_alv USING 'G2'.
            gs_tabs-pressed_tab = c_tabs-tab2.
            gv_screen        = '0102'.

    " Trigger PAI
            CALL METHOD cl_gui_cfw=>set_new_ok_code
              EXPORTING
                new_code = 'DUMMY'.

       WHEN 'REPORT_KEY'.

            CLEAR gt_zcte_fit007_disp.
            READ TABLE gt_zcte_fit003_disp ASSIGNING <l_fs_zcte_fit003> INDEX p_linha.

            LOOP AT gt_zcte_fit007 ASSIGNING <l_fs_zcte_fit007>
                                       WHERE report_id   EQ <l_fs_zcte_fit003>-report_id
                                         AND report_key  EQ <l_fs_zcte_fit003>-report_key
                                         AND report_type EQ <l_fs_zcte_fit003>-report_type.

               APPEND <l_fs_zcte_fit007> TO gt_zcte_fit007_disp.
            ENDLOOP.

            SORT gt_zcte_fit007_disp BY log_repro DESCENDING.

            PERFORM f_update_alv USING 'G3'.
            gs_tabs-pressed_tab = c_tabs-tab2.
            gv_screen        = '0103'.

    " Trigger PAI
            CALL METHOD cl_gui_cfw=>set_new_ok_code
              EXPORTING
                new_code = 'DUMMY'.


       WHEN 'ACC_BELNR'.

        READ TABLE gt_zcte_fit003_disp ASSIGNING <l_fs_zcte_fit003> INDEX p_linha.

        IF NOT <l_fs_zcte_fit003>-acc_belnr IS INITIAL.

          SET PARAMETER ID 'BLN' FIELD <l_fs_zcte_fit003>-acc_belnr.
          SET PARAMETER ID 'BUK' FIELD <l_fs_zcte_fit003>-acc_bukrs.
          SET PARAMETER ID 'GJR' FIELD <l_fs_zcte_fit003>-acc_gjahr.

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.

      WHEN 'CHANGE_STATUS'.

         READ TABLE gt_zcte_fit003_disp ASSIGNING <l_fs_zcte_fit003> INDEX p_linha.
         IF sy-subrc EQ 0 AND NOT <l_fs_zcte_fit003>-change_status IS INITIAL.

            CLEAR gt_zcte_fit005_disp.
            LOOP AT gt_zcte_fit005 ASSIGNING <l_fs_zcte_fit005>
                                       WHERE entity_id    EQ <l_fs_zcte_fit003>-entity_id
                                         AND report_id    EQ <l_fs_zcte_fit003>-report_id
                                         AND report_key   EQ <l_fs_zcte_fit003>-report_key
                                         AND report_type  EQ <l_fs_zcte_fit003>-report_type
                                         AND batch_id     EQ <l_fs_zcte_fit003>-batch_id.


               IF <l_fs_zcte_fit005>-vendor_id_old   EQ <l_fs_zcte_fit005>-vendor_id_new   AND
                  <l_fs_zcte_fit005>-account_old     EQ <l_fs_zcte_fit005>-account_new     AND
                  <l_fs_zcte_fit005>-cost_obj_tp_old EQ <l_fs_zcte_fit005>-cost_obj_tp_new AND
                  <l_fs_zcte_fit005>-cost_obj_old    EQ <l_fs_zcte_fit005>-cost_obj_new .
                  CONTINUE.
               ENDIF.
               APPEND <l_fs_zcte_fit005> TO gt_zcte_fit005_disp.
            ENDLOOP.

            IF NOT gt_zcte_fit005_disp[] IS INITIAL.
               PERFORM f_display_change_log.
            ENDIF.
         ENDIF.

      ENDCASE.
*         ENDIF.

    WHEN 'G2'.

      CASE p_coluna.

          WHEN 'BATCH_LOG_ID'.

             READ TABLE gt_zcte_fit001_disp ASSIGNING <l_fs_zcte_fit001> INDEX p_linha.
             PERFORM f_prepare_log_display USING <l_fs_zcte_fit001>-batch_log_id.
      ENDCASE.

    WHEN 'G3'.

      CASE p_coluna.

          WHEN 'LOG_ID'.

             READ TABLE gt_zcte_fit007_disp ASSIGNING <l_fs_zcte_fit007> INDEX p_linha.
             PERFORM f_prepare_log_display USING <l_fs_zcte_fit007>-log_id.
      ENDCASE.


  ENDCASE.

ENDFORM.                    " drill_down

*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_ALV
*&---------------------------------------------------------------------*

FORM f_update_alv USING pu_grid.

  IF g_grid IS BOUND AND
     ( pu_grid EQ 'G1' OR pu_grid IS INITIAL ).

    CALL METHOD g_grid->refresh_table_display(
      EXCEPTIONS
        finished = 1
        OTHERS   = 2 ).
  ENDIF.

  IF g_grid2 IS BOUND AND
     ( pu_grid EQ 'G2' OR pu_grid IS INITIAL ).

    CALL METHOD g_grid2->refresh_table_display(
      EXCEPTIONS
        finished = 1
        OTHERS   = 2 ).
  ENDIF.

  IF g_grid3 IS BOUND AND
     ( pu_grid EQ 'G3' OR pu_grid IS INITIAL ).

    CALL METHOD g_grid3->refresh_table_display(
      EXCEPTIONS
        finished = 1
        OTHERS   = 2 ).
  ENDIF.

ENDFORM.                    " F_UPDATE_ALV
*&---------------------------------------------------------------------*
*&      Form  F_INICIALIZA_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_inicializa_display .


* get a display profile (or build it yourself)
  CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
       IMPORTING
            e_s_display_profile = g_s_display_profile.

 g_s_display_profile-use_grid = 'X'.
* g_subscreen_prog      = 'SAPLSBAL_DISPLAY'.
* g_subscreen_dynp      = '0101'.

  gs_tabs-prog      = sy-repid.
  gs_tabs-subscreen = '0101'.
  gv_screen         = '0102'.


*  CALL FUNCTION 'BAL_DSP_OUTPUT_INIT'
*       EXPORTING
*            i_s_display_profile = g_s_display_profile
*       EXCEPTIONS
*            OTHERS              = 1.
*  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " F_INICIALIZA_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  F_SHOW_DETAILS
*&---------------------------------------------------------------------*
FORM f_recover_extract  TABLES   pt_cols             TYPE zcte_fis002_t
                        USING    pu_zcte_fit003_disp TYPE ty_zcte_fit003_disp.


  CALL METHOD zcte_cl_acc_integration=>get_gt_cols_from_db
    EXPORTING
      im_v_entity      =  pu_zcte_fit003_disp-entity_id
      im_v_report_type =  pu_zcte_fit003_disp-report_type
      im_v_report_key  =  pu_zcte_fit003_disp-report_key
    IMPORTING
      ex_i_cols = pt_cols[].


ENDFORM.                    " F_RECOVER_EXTRACT
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DETAILS
*&---------------------------------------------------------------------*
FORM f_display_details  TABLES   pt_cols             TYPE zcte_fis002_t
                        USING    pu_zcte_fit003_disp TYPE ty_zcte_fit003_disp.


  CONSTANTS: lc_orgunit TYPE char8 VALUE 'ORG_UNIT'.

  DATA: lt_fieldcat_det TYPE SLIS_T_FIELDCAT_ALV.

  DATA: lv_item_ant TYPE i,
        lv_index    TYPE n,
        lv_field    TYPE char20.

  FIELD-SYMBOLS:  <l_fs_cols>         TYPE zcte_fis002,
                  <l_fs_disp_item>    TYPE ty_disp_item,
                  <l_fs_alloc>        TYPE ty_disp_alloc,
                  <l_fs_zcte_fit005>  TYPE zcte_fit005,
                  <l_fs_zcte_t002>    TYPE zcte_t002,
                  <l_fs_field>        TYPE any.


  CLEAR: gs_disp_header,gt_disp_item, gt_alloc, gt_disp_alloc, gt_zcte_fit005_upd.

  SORT pt_cols BY col061.

  CALL METHOD go_zcte_utils->set_entity
    EXPORTING
      im_v_entity = pu_zcte_fit003_disp-entity_id
    EXCEPTIONS
      NOT_FOUND  = 1
      OTHERS     = 2 .

  IF sy-subrc EQ 0.
    LOOP AT pt_cols ASSIGNING <l_fs_cols>.

       APPEND INITIAL LINE TO gt_zcte_fit005_upd ASSIGNING <l_fs_zcte_fit005>.
       <l_fs_zcte_fit005>-entity_id           =  pu_zcte_fit003_disp-entity_id.
       <l_fs_zcte_fit005>-report_id           =  pu_zcte_fit003_disp-report_id.
       <l_fs_zcte_fit005>-report_key          =  pu_zcte_fit003_disp-report_key.
       <l_fs_zcte_fit005>-report_type         =  pu_zcte_fit003_disp-report_type.
       <l_fs_zcte_fit005>-batch_id            =  pu_zcte_fit003_disp-batch_id.
       <l_fs_zcte_fit005>-entry_id            =  <l_fs_cols>-col061.
       <l_fs_zcte_fit005>-journal_key         =  <l_fs_cols>-col170.
       <l_fs_zcte_fit005>-alloc_key           =  <l_fs_cols>-col189.
       <l_fs_zcte_fit005>-vendor_id_old       =  <l_fs_cols>-col057.
       <l_fs_zcte_fit005>-account_old         =  <l_fs_cols>-col167.
*       <l_fs_zcte_fit005>-cost_obj_tp_old     =  <l_fs_cols>-col195.
*       <l_fs_zcte_fit005>-cost_obj_old        =  <l_fs_cols>-col196.

       AT FIRST.
          CLEAR gs_disp_header.
          gs_disp_header-entity_id      = pu_zcte_fit003_disp-entity_id.
          gs_disp_header-report_name    = pu_zcte_fit003_disp-report_name.
          gs_disp_header-acc_bukrs      = pu_zcte_fit003_disp-acc_bukrs.
          gs_disp_header-acc_belnr      = pu_zcte_fit003_disp-acc_belnr.
          gs_disp_header-acc_gjahr      = pu_zcte_fit003_disp-acc_gjahr.
          CONCATENATE pu_zcte_fit003_disp-emp_first_name
                      pu_zcte_fit003_disp-emp_last_name
                 INTO gs_disp_header-emp_name SEPARATED BY space.
          gs_disp_header-vendor_id      = <l_fs_cols>-col057.

       ENDAT.

       APPEND INITIAL LINE TO gt_alloc ASSIGNING <l_fs_alloc>.

       <l_fs_alloc>-entry_id            = <l_fs_cols>-col061.
       <l_fs_alloc>-alloc_key           = <l_fs_cols>-col189.
       <l_fs_alloc>-perc                = <l_fs_cols>-col190.
       <l_fs_alloc>-alloc1              = <l_fs_cols>-col191.
       <l_fs_alloc>-alloc2              = <l_fs_cols>-col192.
       <l_fs_alloc>-alloc3              = <l_fs_cols>-col193.
       <l_fs_alloc>-alloc4              = <l_fs_cols>-col194.
       <l_fs_alloc>-alloc5              = <l_fs_cols>-col195.
       <l_fs_alloc>-alloc6              = <l_fs_cols>-col196.
       <l_fs_alloc>-deb_cred            = <l_fs_cols>-col168.
       <l_fs_alloc>-montante            = <l_fs_cols>-col211.
       <l_fs_alloc>-moeda               = <l_fs_cols>-col022.


       CASE 'COST OBJECT TYPE'.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit1.
           <l_fs_zcte_fit005>-cost_obj_tp_old     = <l_fs_alloc>-alloc1.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit2.
           <l_fs_zcte_fit005>-cost_obj_tp_old     = <l_fs_alloc>-alloc2.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit3.
           <l_fs_zcte_fit005>-cost_obj_tp_old     = <l_fs_alloc>-alloc3.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit4.
           <l_fs_zcte_fit005>-cost_obj_tp_old     = <l_fs_alloc>-alloc4.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit5.
           <l_fs_zcte_fit005>-cost_obj_tp_old     = <l_fs_alloc>-alloc5.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit6.
           <l_fs_zcte_fit005>-cost_obj_tp_old     = <l_fs_alloc>-alloc6.
       ENDCASE.

       CASE 'COST OBJECT'.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit1.
           <l_fs_zcte_fit005>-cost_obj_old     = <l_fs_alloc>-alloc1.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit2.
           <l_fs_zcte_fit005>-cost_obj_old     = <l_fs_alloc>-alloc2.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit3.
           <l_fs_zcte_fit005>-cost_obj_old     = <l_fs_alloc>-alloc3.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit4.
           <l_fs_zcte_fit005>-cost_obj_old     = <l_fs_alloc>-alloc4.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit5.
           <l_fs_zcte_fit005>-cost_obj_old     = <l_fs_alloc>-alloc5.
         WHEN go_zcte_utils->gs_zcte_t001-org_unit6.
           <l_fs_zcte_fit005>-cost_obj_old     = <l_fs_alloc>-alloc6.
       ENDCASE.


       IF  <l_fs_cols>-col061 EQ lv_item_ant.
         CONTINUE.
       ENDIF.
       lv_item_ant = <l_fs_cols>-col061.

       APPEND INITIAL LINE TO gt_disp_item ASSIGNING <l_fs_disp_item>.

       <l_fs_disp_item>-entry_id            = <l_fs_cols>-col061.
       <l_fs_disp_item>-journal_key         = <l_fs_cols>-col170.
       <l_fs_disp_item>-expense_name        = <l_fs_cols>-col063.
       CONCATENATE <l_fs_cols>-col064(4)
                   <l_fs_cols>-col064+5(2)
                   <l_fs_cols>-col064+8(2)
              INTO <l_fs_disp_item>-expense_date.

       <l_fs_disp_item>-meio_pgto           = <l_fs_cols>-col250.
       <l_fs_disp_item>-conta               = <l_fs_cols>-col167.
       <l_fs_disp_item>-pessoal             = <l_fs_cols>-col068.
       <l_fs_disp_item>-montante            = <l_fs_cols>-col124."col123.
       <l_fs_disp_item>-moeda               = <l_fs_cols>-col022.
    ENDLOOP.

    gv_disp = 'DISPLAY'.

    DO 6 TIMES.
      lv_index = sy-index.
      CONCATENATE lc_orgunit lv_index INTO  lv_field.
      ASSIGN COMPONENT lv_field OF STRUCTURE go_zcte_utils->gs_zcte_t001 TO <l_fs_field>.
      IF sy-subrc EQ 0.
         READ TABLE gt_zcte_t002 ASSIGNING <l_fs_zcte_t002>
                                 WITH KEY org_unit    = <l_fs_field>
                                 BINARY SEARCH.
         IF sy-subrc EQ 0.
            ASSIGN COMPONENT sy-index OF STRUCTURE gs_disp_alloc_name TO <l_fs_field>.
            IF sy-subrc EQ 0.
              <l_fs_field> = <l_fs_zcte_t002>-display_name.
            ENDIF.
         ENDIF.

      ENDIF.

    ENDDO.

    SORT gt_alloc     BY entry_id alloc_key.
    SORT gt_disp_alloc BY entry_id alloc_key.
    SORT gt_disp_item BY entry_id journal_key.

    CALL SCREEN 0200 STARTING AT 10 10.
  ENDIF.

ENDFORM.                    " F_DISPLAY_DETAILS
*&---------------------------------------------------------------------*
*&      Form  F_AD_DISPLAY_DETAILS
*&---------------------------------------------------------------------*
FORM f_ad_display_details  TABLES   pt_cols             TYPE zcte_fis002_t
                           USING    pu_zcte_fit003_disp TYPE ty_zcte_fit003_disp.

  DATA: lt_fieldcat_det TYPE SLIS_T_FIELDCAT_ALV.

  DATA: lv_item_ant TYPE i.

  FIELD-SYMBOLS:  <l_fs_cols>         TYPE zcte_fis002.

  CLEAR: gs_disp_header.

  CHECK NOT pt_cols[] IS INITIAL.
  READ TABLE pt_cols ASSIGNING <l_fs_cols> INDEX 1.

  gs_disp_header-entity_id      = pu_zcte_fit003_disp-entity_id.
  gs_disp_header-report_name    = pu_zcte_fit003_disp-report_name.
  gs_disp_header-acc_bukrs      = pu_zcte_fit003_disp-acc_bukrs.
  gs_disp_header-acc_belnr      = pu_zcte_fit003_disp-acc_belnr.
  gs_disp_header-acc_gjahr      = pu_zcte_fit003_disp-acc_gjahr.

  CONCATENATE pu_zcte_fit003_disp-emp_first_name
              pu_zcte_fit003_disp-emp_last_name
         INTO gs_disp_header-emp_name SEPARATED BY space.

  gs_disp_header-vendor_id      = <l_fs_cols>-col282.
  gs_disp_header-ad_amount      = <l_fs_cols>-col169.

  gv_disp = 'DISPLAY'.

  CALL SCREEN 0201 STARTING AT 10 10.

ENDFORM.                    " F_AD_DISPLAY_DETAILS

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS '0200'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE gv_okcode.

  WHEN '&OK'.
    LEAVE TO SCREEN 0.

  WHEN '&ALLOC'.
    PERFORM f_build_alloc_disp.

  WHEN '&EDIT'.
    IF gs_disp_header-acc_belnr IS INITIAL.
       PERFORM f_change_display.
    ENDIF.

  WHEN '&SAVE'.
    IF gs_disp_header-acc_belnr IS INITIAL.
       PERFORM f_save_changed_report.
    endif.
  ENDCASE.

  CLEAR gv_okcode.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_BUILD_ALLOC_DISP
*&---------------------------------------------------------------------*
FORM f_build_alloc_disp .

  DATA: lv_line TYPE i.

  FIELD-SYMBOLS: <l_fs_disp_item>  TYPE ty_disp_item,
                 <l_fs_disp_alloc> TYPE ty_disp_alloc.

  CLEAR gt_disp_alloc.
  GET CURSOR LINE lv_line.
  READ TABLE gt_disp_item INDEX lv_line ASSIGNING <l_fs_disp_item>.

  IF <l_fs_disp_item> IS ASSIGNED.
     LOOP AT gt_alloc ASSIGNING <l_fs_disp_alloc> WHERE entry_id EQ <l_fs_disp_item>-entry_id.
        IF NOT <l_fs_disp_alloc>-perc IS INITIAL.
           APPEND <l_fs_disp_alloc> TO gt_disp_alloc.
        ENDIF.
     ENDLOOP.
  ENDIF.

ENDFORM.                    " F_BUILD_ALLOC_DISP
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_DISPLAY
*&---------------------------------------------------------------------*
FORM f_change_display .


     IF gv_disp EQ 'EDIT'.
        gv_disp = 'DISPLAY'.
     ELSE.
        gv_disp = 'EDIT'.
     ENDIF.

ENDFORM.                    " F_CHANGE_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  SCREEN_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_0200 OUTPUT.

  PERFORM f_screen_0200.

ENDMODULE.                 " SCREEN_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_SCREEN_0200
*&---------------------------------------------------------------------*
FORM f_screen_0200 .

  CONSTANTS: lc_orgunit TYPE char8 VALUE 'ORG_UNIT'.

  DATA: ls_cols   TYPE cxtab_column.

  DATA: lv_field TYPE char10.

  FIELD-SYMBOLS: <l_fs_field>     TYPE any.


  DESCRIBE TABLE gt_disp_item.
  tc_item-lines = sy-tfill.

  DESCRIBE TABLE gt_disp_alloc.
  tc_alloc-lines = sy-tfill.


  CALL METHOD go_zcte_utils->set_entity
    EXPORTING
      im_v_entity = gs_disp_header-entity_id
    EXCEPTIONS
      NOT_FOUND  = 1
      OTHERS     = 2 .

  IF sy-subrc EQ 0.

      LOOP AT tc_alloc-cols INTO ls_cols.
          IF ls_cols-screen-name(19) EQ 'GS_DISP_ALLOC-ALLOC'.
              CONCATENATE lc_orgunit
                         ls_cols-screen-name+19(1)
                    INTO lv_field.
             ASSIGN COMPONENT lv_field OF STRUCTURE go_zcte_utils->gs_zcte_t001 TO <l_fs_field>.
             IF sy-subrc EQ 0 AND <l_fs_field> IS INITIAL.
                ls_cols-invisible = abap_true.
                MODIFY tc_alloc-cols FROM ls_cols.
             ENDIF.
          ENDIF.
      ENDLOOP.

  ENDIF.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'G1'.
       IF gv_disp = 'EDIT'.
           screen-input  = 1.
       ELSE.
          screen-input = 0.
       ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.


ENDFORM.                    " F_SCREEN_0200
*&---------------------------------------------------------------------*
*&      Form  F_SCREEN_0200_tc
*&---------------------------------------------------------------------*
FORM f_screen_0200_tc .

  CONSTANTS: lc_orgunit TYPE char8 VALUE 'ORG_UNIT'.

  DATA: lv_field TYPE char10.

  FIELD-SYMBOLS: <l_fs_field>     TYPE any.

  CALL METHOD go_zcte_utils->set_entity
    EXPORTING
      im_v_entity = gs_disp_header-entity_id
    EXCEPTIONS
      NOT_FOUND  = 1
      OTHERS     = 2 .

  IF sy-subrc EQ 0.

      LOOP AT SCREEN.
        IF screen-group1 EQ 'G1'.
           IF gv_disp = 'EDIT'.
              IF screen-name(19) EQ 'GS_DISP_ALLOC-ALLOC'.
                  CONCATENATE lc_orgunit
                             screen-name+19(1)
                        INTO lv_field.
                 ASSIGN COMPONENT lv_field OF STRUCTURE go_zcte_utils->gs_zcte_t001 TO <l_fs_field>.
                  IF sy-subrc EQ 0.
                   CASE <l_fs_field>.
                      WHEN 'COST OBJECT TYPE' OR 'COST OBJECT'.
                         screen-input = 1.
                      WHEN space.
                         screen-invisible = abap_true.
                   ENDCASE.
                 ENDIF.
              ELSE.
                 screen-input  = 1.
              ENDIF.
           ELSE.
              screen-input = 0.
           ENDIF.
         ENDIF.
         MODIFY SCREEN.
      ENDLOOP.
  ENDIF.

ENDFORM.                    " F_SCREEN_0200_tc
*&---------------------------------------------------------------------*
*&      Module  SCREEN_TC_ITEM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_tc_item OUTPUT.

  PERFORM f_screen_0200_tc.

ENDMODULE.                 " SCREEN_TC_ITEM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_TC_ALLOC  OUTPUT
*&---------------------------------------------------------------------*
MODULE screen_tc_alloc OUTPUT.

  PERFORM f_screen_0200_tc.

ENDMODULE.                 " SCREEN_TC_ALLOC  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC_ITEM_UPDATE_LINE  INPUT
*&---------------------------------------------------------------------*
MODULE tc_item_update_line INPUT.

    MODIFY gt_disp_item FROM gs_disp_item INDEX tc_item-current_line
    TRANSPORTING conta.


ENDMODULE.                 " TC_ITEM_UPDATE_LINE  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_ALLOC_UPDATE_LINE  INPUT
*&---------------------------------------------------------------------*
MODULE tc_alloc_update_line INPUT.

  PERFORM f_tc_alloc_update_line.

ENDMODULE.                 " TC_ALLOC_UPDATE_LINE  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_SAVE_CHANGED_REPORT
*&---------------------------------------------------------------------*
FORM f_save_changed_report .

  DATA: l_v_change_flag TYPE boolean,
        l_v_change_nr   TYPE zcte_fit005-change_nr,
        l_v_resp        TYPE c.

  FIELD-SYMBOLS: <l_fs_zcte_fit005> TYPE zcte_fit005,
                 <l_fs_disp_item>   TYPE ty_disp_item,
                 <l_fs_alloc>       TYPE ty_disp_alloc.


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question               = 'Confirm save?'
      display_cancel_button       = abap_false
   IMPORTING
     ANSWER                      = l_v_resp
   EXCEPTIONS
     TEXT_NOT_FOUND              = 1
     OTHERS                      = 2.

  IF sy-subrc NE 0 OR l_v_resp NE 1.
    EXIT.
  ENDIF.

  CALL METHOD go_zcte_utils->set_entity
    EXPORTING
      im_v_entity = gs_disp_header-entity_id
    EXCEPTIONS
      NOT_FOUND  = 1
      OTHERS     = 2 .

  IF sy-subrc EQ 0.

    l_v_change_flag = abap_false.

    LOOP AT gt_zcte_fit005_upd ASSIGNING <l_fs_zcte_fit005>.

       AT FIRST.

          SELECT change_nr UP TO 1 ROWS
            FROM zcte_fit005
            INTO l_v_change_nr
            WHERE entity_id    EQ <l_fs_zcte_fit005>-entity_id
              AND report_id    EQ <l_fs_zcte_fit005>-report_id
              AND report_key   EQ <l_fs_zcte_fit005>-report_key
              AND report_type  EQ <l_fs_zcte_fit005>-report_type
              AND batch_id     EQ <l_fs_zcte_fit005>-batch_id
            ORDER BY change_nr DESCENDING.
          ENDSELECT.

          ADD 1 TO l_v_change_nr.
       ENDAT.

       <l_fs_zcte_fit005>-change_nr     = l_v_change_nr.

       <l_fs_zcte_fit005>-vendor_id_new = gs_disp_header-vendor_id.

       READ TABLE gt_disp_item WITH KEY entry_id    = <l_fs_zcte_fit005>-entry_id
                                        journal_key = <l_fs_zcte_fit005>-journal_key
                               ASSIGNING <l_fs_disp_item> BINARY SEARCH.
       IF sy-subrc EQ 0.
          <l_fs_zcte_fit005>-account_new = <l_fs_disp_item>-conta.
       ELSE.
          <l_fs_zcte_fit005>-account_new = <l_fs_zcte_fit005>-account_old.
       ENDIF.

       READ TABLE gt_alloc WITH KEY entry_id  = <l_fs_zcte_fit005>-entry_id
                                    alloc_key = <l_fs_zcte_fit005>-alloc_key
                               ASSIGNING <l_fs_alloc> BINARY SEARCH.
       IF sy-subrc EQ 0.

          CASE 'COST OBJECT TYPE'.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit1.
              <l_fs_zcte_fit005>-cost_obj_tp_new     = <l_fs_alloc>-alloc1.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit2.
              <l_fs_zcte_fit005>-cost_obj_tp_new     = <l_fs_alloc>-alloc2.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit3.
              <l_fs_zcte_fit005>-cost_obj_tp_new     = <l_fs_alloc>-alloc3.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit4.
              <l_fs_zcte_fit005>-cost_obj_tp_new     = <l_fs_alloc>-alloc4.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit5.
              <l_fs_zcte_fit005>-cost_obj_tp_new     = <l_fs_alloc>-alloc5.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit6.
              <l_fs_zcte_fit005>-cost_obj_tp_new     = <l_fs_alloc>-alloc6.
          ENDCASE.

          CASE 'COST OBJECT'.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit1.
              <l_fs_zcte_fit005>-cost_obj_new     = <l_fs_alloc>-alloc1.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit2.
              <l_fs_zcte_fit005>-cost_obj_new     = <l_fs_alloc>-alloc2.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit3.
              <l_fs_zcte_fit005>-cost_obj_new     = <l_fs_alloc>-alloc3.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit4.
              <l_fs_zcte_fit005>-cost_obj_new     = <l_fs_alloc>-alloc4.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit5.
              <l_fs_zcte_fit005>-cost_obj_new     = <l_fs_alloc>-alloc5.
            WHEN go_zcte_utils->gs_zcte_t001-org_unit6.
              <l_fs_zcte_fit005>-cost_obj_new     = <l_fs_alloc>-alloc6.
          ENDCASE.

       ENDIF.

      <l_fs_zcte_fit005>-changed_by   = sy-uname.
      <l_fs_zcte_fit005>-change_date  = sy-datum.
      <l_fs_zcte_fit005>-change_time  = sy-uzeit.


      IF <l_fs_zcte_fit005>-vendor_id_old   NE <l_fs_zcte_fit005>-vendor_id_new     OR
         <l_fs_zcte_fit005>-account_old     NE <l_fs_zcte_fit005>-account_new       OR
         <l_fs_zcte_fit005>-cost_obj_tp_old NE <l_fs_zcte_fit005>-cost_obj_tp_new   OR
         <l_fs_zcte_fit005>-cost_obj_old    NE <l_fs_zcte_fit005>-cost_obj_new.

         l_v_change_flag = abap_true.

      ENDIF.
    ENDLOOP.

    IF l_v_change_flag EQ abap_false.
       MESSAGE i500(26) WITH 'Document was not modified'.
    ELSE.

       MODIFY zcte_fit005 FROM TABLE gt_zcte_fit005_upd.
       COMMIT WORK AND WAIT.
       IF sy-subrc EQ 0.
         MESSAGE i500(26) WITH 'Document updated'.
       ENDIF.

       CLEAR gt_zcte_fit005_upd.

    ENDIF.

    PERFORM f_refresh_all.
    LEAVE TO SCREEN 0.
  ENDIF.


ENDFORM.                    " F_SAVE_CHANGED_REPORT
*&---------------------------------------------------------------------*
*&      Form  F_TC_ALLOC_UPDATE_LINE
*&---------------------------------------------------------------------*
FORM f_tc_alloc_update_line .

    FIELD-SYMBOLS: <l_fs_alloc> TYPE ty_disp_alloc.

    MODIFY gt_disp_alloc FROM gs_disp_alloc INDEX tc_alloc-current_line
    TRANSPORTING alloc1 alloc2 alloc3 alloc4 alloc5 alloc6.

    READ TABLE gt_alloc WITH KEY entry_id  = gs_disp_alloc-entry_id
                                 alloc_key = gs_disp_alloc-alloc_key
                        ASSIGNING <l_fs_alloc> BINARY SEARCH.
    IF sy-subrc EQ 0.
       <l_fs_alloc>-alloc1 =  gs_disp_alloc-alloc1.
       <l_fs_alloc>-alloc2 =  gs_disp_alloc-alloc2.
       <l_fs_alloc>-alloc3 =  gs_disp_alloc-alloc3.
       <l_fs_alloc>-alloc4 =  gs_disp_alloc-alloc4.
       <l_fs_alloc>-alloc5 =  gs_disp_alloc-alloc5.
       <l_fs_alloc>-alloc6 =  gs_disp_alloc-alloc6.
    ENDIF.

ENDFORM.                    " F_TC_ALLOC_UPDATE_LINE
*&---------------------------------------------------------------------*
*&      Form  F_CALL_ACC_REPORT
*&---------------------------------------------------------------------*
FORM f_call_acc_report  TABLES   pt_cols             TYPE zcte_fis002_t
                        USING    pu_zcte_fit003_disp TYPE ty_zcte_fit003_disp.


  DATA: lt_logs       TYPE zcte_s003_t,
        lt_logs_aux   TYPE zcte_s003_t,
        lt_loghndl    TYPE bal_t_logh.


  DATA: lo_acc_integration TYPE REF TO zcte_cl_acc_integration.

  DATA: lv_repkey     TYPE pclkey,
        lv_key        TYPE char32,
        lv_batch_dt   TYPE char10,
        lv_loghndl     TYPE balloghndl.

  FIELD-SYMBOLS: <l_fs_zcte_fit001> TYPE zcte_fit001,
                 <l_fs_logs>        TYPE zcte_s003.


  READ TABLE gt_zcte_fit001 WITH KEY batch_id = pu_zcte_fit003_disp-batch_id
                            ASSIGNING <l_fs_zcte_fit001>.


  CONCATENATE <l_fs_zcte_fit001>-batch_date_id(4)
              <l_fs_zcte_fit001>-batch_date_id+4(2)
              <l_fs_zcte_fit001>-batch_date_id+6(2)
         INTO lv_batch_dt SEPARATED BY '/'.


  CREATE OBJECT lo_acc_integration.

  CALL METHOD lo_acc_integration->set_entity_id
    EXPORTING
      im_v_entity = pu_zcte_fit003_disp-entity_id
    EXCEPTIONS
      not_found   = 1
      OTHERS      = 2.

  IF sy-subrc NE 0.
     MESSAGE a021. "Critical Error, unable to continue
  ENDIF.


  APPEND INITIAL LINE TO lt_logs ASSIGNING <l_fs_logs>.

  CALL METHOD lo_acc_integration->set_gt_cols_from_db
     EXPORTING
       im_v_report_type   = pu_zcte_fit003_disp-report_type
       im_v_report_id     = pu_zcte_fit003_disp-report_id
       im_v_report_key    = pu_zcte_fit003_disp-report_key
       im_v_batch_date    = lv_batch_dt
       im_v_batch_id      = pu_zcte_fit003_disp-batch_id
     IMPORTING
       ex_o_loghandle     = <l_fs_logs>-log_obj
     EXCEPTIONS
        internal_error     = 1
        OTHERS             = 2.

  IF sy-subrc EQ 0.

      CLEAR lt_logs_aux.
      CALL METHOD lo_acc_integration->process_extract_documents(
      EXPORTING
        im_v_manual_proc = abap_true
      IMPORTING
        ex_i_log_handle = lt_logs_aux
      EXCEPTIONS
        entity_not_initialized = 1
        OTHERS                 = 2  ).

      IF sy-subrc EQ 0.
         APPEND LINES OF lt_logs_aux TO lt_logs.
      ELSE.
         MESSAGE a021.
      ENDIF.
  ENDIF.

  IF NOT lt_logs IS INITIAL.
      LOOP AT lt_logs ASSIGNING <l_fs_logs>.
         lv_loghndl = <l_fs_logs>-log_obj->get_log_handle( ).
         INSERT lv_loghndl INTO TABLE lt_loghndl.
      ENDLOOP.

      zcte_cl_log_handle=>display_multiple_logs(
        EXPORTING
          im_i_logs = lt_loghndl
        EXCEPTIONS
          display_failed = 1 ).

      IF sy-subrc NE 0.
         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
  ENDIF.

ENDFORM.                    " F_CALL_ACC_REPORT
*&---------------------------------------------------------------------*
*&      Module  GET_TABS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_tabs INPUT.

  gv_okcode = sy-ucomm.

  CASE gv_okcode.
    WHEN c_tabs-tab1.
      gs_tabs-pressed_tab = c_tabs-tab1.
    WHEN c_tabs-tab2.
      gs_tabs-pressed_tab = c_tabs-tab2.
  ENDCASE.

ENDMODULE.                 " GET_TABS  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TABS  OUTPUT
*&---------------------------------------------------------------------*
MODULE set_tabs OUTPUT.

  ts_tabs-activetab = gs_tabs-pressed_tab.
  CASE gs_tabs-pressed_tab.
    WHEN c_tabs-tab1.
      gs_tabs-subscreen = '0101'.
      gs_tabs-prog      = sy-repid.
    WHEN c_tabs-tab2.
*      gs_tabs-subscreen = '0101'.
*      gs_tabs-prog      = 'SAPLSBAL_DISPLAY'.
      gs_tabs-subscreen = gv_screen.
      gs_tabs-prog      = sy-repid.
  ENDCASE.


ENDMODULE.                 " SET_TABS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_LOG_DISPLAY
*&---------------------------------------------------------------------*
FORM f_prepare_log_display  USING pu_log_id.


  DATA:
        li_logsearch TYPE BAL_R_LOGN,
        ls_logsearch TYPE BAL_S_LOGN,
        ls_logfilter TYPE BAL_S_LFIL.

  DATA: li_loghandle TYPE BAL_T_LOGH.

  DATA: li_balhdr    TYPE BALHDR_T,
        ls_balhdr    TYPE balhdr.

  DATA: lv_loghandle LIKE LINE OF li_loghandle.


  DATA: li_lognumber TYPE BAL_T_LOGN .

  DATA: lv_lognumber LIKE LINE OF li_lognumber.


             lv_lognumber = pu_log_id.
             INSERT lv_lognumber INTO TABLE li_lognumber.


             CALL FUNCTION 'BAL_DB_LOAD'
               EXPORTING
                 i_t_lognumber = li_lognumber
               EXCEPTIONS
                 NO_LOGS_SPECIFIED  = 1
                 LOG_NOT_FOUND      = 2
                 LOG_ALREADY_LOADED = 3.

             IF sy-subrc EQ 0.

               CALL FUNCTION 'BAL_DSP_OUTPUT_INIT'
                    EXPORTING
                         i_s_display_profile = g_s_display_profile
                    EXCEPTIONS
                         OTHERS              = 1.


              CALL FUNCTION 'BAL_DSP_OUTPUT_SET_DATA'
*                EXPORTING
*            *      i_s_log_filter         = i_s_log_filter
*            *      i_t_log_context_filter = i_t_log_context_filter
*            *      i_s_msg_filter         = i_s_msg_filter
*            *      i_t_msg_context_filter = i_t_msg_context_filter
*                  i_t_log_handle         = li_loghandle
*            *      i_t_msg_handle         = i_t_msg_handle
*            *      i_srt_by_timstmp       = i_srt_by_timstmp
*            *    IMPORTING
*            *      e_no_data_available    = l_no_data_available
*            *      e_no_authority         = l_no_authority
                EXCEPTIONS
                  profile_inconsistent   = 1
                  OTHERS                 = 2.
*         evaluate error situation

*
               ls_logsearch-sign   = 'I'.
               ls_logsearch-option = 'EQ'.
               ls_logsearch-low    = pu_log_id.
               APPEND ls_logsearch TO li_logsearch.
              ls_logfilter-lognumber = li_logsearch.


             CALL FUNCTION 'BAL_DB_SEARCH'
               EXPORTING
*                I_CLIENT                 = SY-MANDT
                 i_s_log_filter           = ls_logfilter
*                I_T_SEL_FIELD            =
              IMPORTING
                E_T_LOG_HEADER           = li_balhdr
              EXCEPTIONS
                LOG_NOT_FOUND            = 1
                NO_FILTER_CRITERIA       = 2
                OTHERS                   = 3
                       .
             IF sy-subrc <> 0.
*         Implement suitable error handling here
             ENDIF.

             READ TABLE li_balhdr INDEX 1 INTO ls_balhdr.

             CALL FUNCTION 'BAL_LOG_REFRESH'
               EXPORTING
                 i_log_handle        = ls_balhdr-log_handle
              EXCEPTIONS
                LOG_NOT_FOUND       = 1
                OTHERS              = 2
                       .
*             IF sy-subrc <> 0.
**         Implement suitable error handling here
*             ENDIF.

*           call dispatch method of control framwork
            CALL METHOD cl_gui_cfw=>dispatch.

    " Trigger PAI
            CALL METHOD cl_gui_cfw=>set_new_ok_code
              EXPORTING
                new_code = 'DUMMY'.

          ENDIF.



ENDFORM.                    " F_PREPARE_LOG_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH_ALL
*&---------------------------------------------------------------------*
FORM f_refresh_all .

      PERFORM: f_seleciona_dados,
               f_update_alv USING space.

ENDFORM.                    " F_REFRESH_ALL
*&---------------------------------------------------------------------*
*&      Module  STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0201 OUTPUT.

  SET PF-STATUS '0201'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0201  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0201 INPUT.

  CASE gv_okcode.

  WHEN '&OK'.
    LEAVE TO SCREEN 0.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0201  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_CHANGE_LOG
*&---------------------------------------------------------------------*
FORM f_display_change_log .

  DATA: lt_fieldcat TYPE SLIS_T_FIELDCAT_ALV.

  FIELD-SYMBOLS: <l_fs_fieldcat> TYPE SLIS_FIELDCAT_ALV.

   CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
*      I_PROGRAM_NAME               = sy-repid
*      I_INTERNAL_TABNAME           = 'GT_ZCTE_FIT005_DISP'
      I_STRUCTURE_NAME             = 'ZCTE_FIT005'
      I_CLIENT_NEVER_DISPLAY       = 'X'
*      I_INCLNAME                   =
*      I_BYPASSING_BUFFER           =
*      I_BUFFER_ACTIVE              =
     CHANGING
       ct_fieldcat                  = lt_fieldcat
    EXCEPTIONS
      INCONSISTENT_INTERFACE       = 1
      PROGRAM_ERROR                = 2
      OTHERS                       = 3.

   IF sy-subrc NE 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
       EXIT.
   ENDIF.

   LOOP AT lt_fieldcat ASSIGNING <l_fs_fieldcat>.
     IF <l_fs_fieldcat>-fieldname EQ 'MANDT' OR
        <l_fs_fieldcat>-fieldname EQ 'REPORT_ID' OR
        <l_fs_fieldcat>-fieldname EQ 'REPORT_KEY' OR
        <l_fs_fieldcat>-fieldname EQ 'REPORT_TYPE' OR
        <l_fs_fieldcat>-fieldname EQ 'BATCH_ID' OR
        <l_fs_fieldcat>-fieldname EQ 'CHANGE_NR' OR
        <l_fs_fieldcat>-fieldname EQ 'ENTRY_ID' OR
        <l_fs_fieldcat>-fieldname EQ 'JOURNAL_KEY' OR
        <l_fs_fieldcat>-fieldname EQ 'ALLOC_KEY'.
         <l_fs_fieldcat>-no_out = abap_true.
      ENDIF.

      CASE <l_fs_fieldcat>-fieldname.

        WHEN 'VENDOR_ID_OLD'.
           <l_fs_fieldcat>-seltext_l = 'Vendor (Old)'.
        WHEN 'VENDOR_ID_NEW'.
           <l_fs_fieldcat>-seltext_l = 'Vendor (New)'.
        WHEN 'ACCOUNT_OLD'.
           <l_fs_fieldcat>-seltext_l = 'Account (Old)'.
        WHEN 'ACCOUNT_NEW'.
           <l_fs_fieldcat>-seltext_l = 'Account (New)'.
        WHEN 'COST_OBJ_TP_OLD'.
           <l_fs_fieldcat>-seltext_l = 'Cost Obj. type (Old)'.
        WHEN 'COST_OBJ_TP_NEW'.
           <l_fs_fieldcat>-seltext_l = 'Cost Obj. type (New)'.
        WHEN 'COST_OBJ_OLD'.
           <l_fs_fieldcat>-seltext_l = 'Cost Obj. (Old)'.
        WHEN 'COST_OBJ_NEW'.
           <l_fs_fieldcat>-seltext_l = 'Cost Obj. (New)'.
        WHEN 'CHANGED_BY'.
           <l_fs_fieldcat>-seltext_l = 'Changed by'.
        WHEN 'CHANGE_DATE'.
           <l_fs_fieldcat>-seltext_l = 'Change date'.
        WHEN 'CHANGE_TIME'.
           <l_fs_fieldcat>-seltext_l = 'Change time'.
      ENDCASE.
   ENDLOOP.


   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      IT_FIELDCAT                       = lt_fieldcat
      I_SCREEN_START_COLUMN             = 2
      I_SCREEN_START_LINE               = 2
      I_SCREEN_END_COLUMN               = 200
      I_SCREEN_END_LINE                 = 15
     TABLES
       t_outtab                          = gt_zcte_fit005_disp
    EXCEPTIONS
      PROGRAM_ERROR                     = 1
      OTHERS                            = 2.

   IF sy-subrc <> 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
       EXIT.
   ENDIF.


ENDFORM.                    " F_DISPLAY_CHANGE_LOG
*&---------------------------------------------------------------------*
*&      Form  F_STATUS_0100
*&---------------------------------------------------------------------*
FORM F_STATUS_0100 .

  FIELD-SYMBOLS: <l_fs_zcte_t001> TYPE zcte_t001.

  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

  CALL METHOD go_zcte_utils->set_entity
    EXPORTING
      im_v_entity = gv_entity
    EXCEPTIONS
      NOT_FOUND  = 1
      OTHERS     = 2 .
  IF sy-subrc EQ 0.
     gv_entity      = go_zcte_utils->gs_zcte_t001-entity_id.
     gv_entity_desc = go_zcte_utils->gs_zcte_t001-entity_desc.
  ENDIF.

  CALL METHOD cl_gui_cfw=>flush.

  IF sy-ucomm EQ '&SRCH'.

     CALL FUNCTION 'BAL_DSP_OUTPUT_FREE'
          EXCEPTIONS
               OTHERS       = 1.
     IF sy-subrc <> 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

  ENDIF.


ENDFORM.                    " F_STATUS_0100
