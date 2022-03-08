*&---------------------------------------------------------------------*
*& Report  ZCTE_ORG_STRUCT_EXTRACT
*&---------------------------------------------------------------------*
*&                  SAP BRAZIL
*&   DEVELOPER:  Gilliatti Paparelli  2020/04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZCTE_ORG_STRUCT_EXTRACT MESSAGE-ID zcte_fi.

TYPE-POOLS: vrm, abap.

TABLES: sscrfields, csks, prps, aufk.

TYPES: BEGIN OF ty_file,
         line TYPE char1024,
       END OF ty_file.

TYPES: BEGIN OF ty_final,
         value    TYPE tdline,
       END OF ty_final.

TYPES: char48(48) TYPE c.

TYPES: BEGIN OF ty_csks,
          kokrs     TYPE csks-kokrs,
          kostl     TYPE csks-kostl,
          bukrs     TYPE csks-bukrs,
          gsber     TYPE csks-gsber,
       END OF ty_csks.

TYPES: BEGIN OF ty_cskt,
          kokrs TYPE cskt-kokrs,
          kostl TYPE cskt-kostl,
          spras TYPE cskt-spras,
          ktext TYPE cskt-ktext,
       END OF ty_cskt,

       BEGIN OF ty_cc_del.
          INCLUDE STRUCTURE zcte_fit006.
       TYPES:
          kostl     TYPE csks-kostl,
       END OF ty_cc_del,

       BEGIN OF ty_prps,
          pspnr     TYPE  prps-pspnr,
          post1     TYPE  prps-post1,
          pbukr     TYPE  prps-pbukr,
          pgsbr     TYPE  prps-pgsbr,
          pkokr     TYPE  prps-pkokr,
       END OF ty_prps,

       BEGIN OF ty_pj_del.
          INCLUDE STRUCTURE zcte_fit006.
       TYPES:
          pspnr     TYPE prps-pspnr,
       END OF ty_pj_del,

       BEGIN OF ty_aufk,
         aufnr    TYPE  aufk-aufnr,
         bukrs    TYPE  aufk-bukrs,
         gsber    TYPE  aufk-gsber,
         kokrs    TYPE  aufk-kokrs,
         ktext    TYPE  aufk-ktext,
       END OF ty_aufk,

       BEGIN OF ty_io_del.
          INCLUDE STRUCTURE zcte_fit006.
       TYPES:
          aufnr     TYPE aufk-aufnr,
       END OF ty_io_del,

       BEGIN OF ty_orgunit_fields,
          logical_system      TYPE  zcte_orgunit1,
          acc_area            TYPE  zcte_orgunit1,
          country             TYPE  zcte_orgunit1,
          company_code        TYPE  zcte_orgunit1,
          bus_area            TYPE  zcte_orgunit1,
          cost_obj_type       TYPE  zcte_orgunit1,
          cost_obj_id         TYPE  zcte_orgunit1,
       END OF ty_orgunit_fields.

TYPES: BEGIN OF ty_structure_orgs,
             listname      TYPE char64,
             listcategoria TYPE char64,
             level01       TYPE char32,
             level02       TYPE char32,
             level03       TYPE char32,
             level04       TYPE char32,
             level05       TYPE char32,
             level06       TYPE char32,
             level07       TYPE char32,
             level08       TYPE char32,
             level09       TYPE char32,
             level10       TYPE char32,
             value         TYPE char64,
             start         TYPE char8,
             final         TYPE char8,
             del           TYPE char1,
        END OF ty_structure_orgs.

  TYPES: BEGIN OF ty_orgunit_item,
           code    TYPE  char48,
           name    TYPE  char100,
         END OF ty_orgunit_item.

TYPES: BEGIN OF ty_t001,
             bukrs    TYPE t001-bukrs,
             butxt    TYPE t001-butxt,
             land1    TYPE t001-land1,
             landx50  TYPE t005t-landx50,
       END OF ty_t001,

       BEGIN OF ty_tka01,
             kokrs    TYPE tka01-kokrs,
             bezei    TYPE tka01-bezei,
       END OF ty_tka01,

       BEGIN OF ty_tgsbt,
             gsber    TYPE tgsbt-gsber,
             gtext    TYPE tgsbt-gtext,
       END OF ty_tgsbt.


TYPES: ty_t_cc_del     TYPE STANDARD TABLE OF ty_cc_del,
       ty_t_pj_del     TYPE STANDARD TABLE OF ty_pj_del,
       ty_t_io_del     TYPE STANDARD TABLE OF ty_io_del,
       ty_t_prps       TYPE STANDARD TABLE OF ty_prps,
       ty_t_aufk       TYPE STANDARD TABLE OF ty_aufk,
       ty_t_csks       TYPE STANDARD TABLE OF ty_csks,
       ty_t_cskt       TYPE STANDARD TABLE OF ty_cskt.

DATA:
      i_t001        TYPE STANDARD TABLE OF ty_t001,
      i_tka01       TYPE STANDARD TABLE OF ty_tka01,
      i_tgsbt        TYPE STANDARD TABLE OF ty_tgsbt,
      i_orgs        TYPE STANDARD TABLE OF ty_structure_orgs,
      i_file        TYPE STANDARD TABLE OF ty_file,
      i_log         TYPE STANDARD TABLE OF zcte_fit006,
      i_del         TYPE STANDARD TABLE OF zcte_fit006,
      i_zcte_t001   TYPE STANDARD TABLE OF zcte_t001.

DATA: s_csks            TYPE ty_csks,
      s_cskt            TYPE ty_cskt,
      s_prps            TYPE ty_prps,
      s_aufk            TYPE ty_aufk,
      s_orgs            TYPE ty_structure_orgs,
      s_orgunit_fields  TYPE ty_orgunit_fields,
      s_zcte_t001       TYPE zcte_t001.


DATA: s_log             TYPE zcte_s003.

CONSTANTS: c_trx      TYPE char13 VALUE 'Org Structure',
           c_deln     TYPE char1  VALUE 'N',
           c_dely     TYPE char1  VALUE 'Y',
           c_cust     TYPE char2  VALUE 'CC',
           c_cctx     TYPE char15 VALUE 'Cost Center',
           c_oint     TYPE char2  VALUE 'OI',
           c_oitx     TYPE char15 VALUE 'Internal Order',
           c_proj     TYPE char2  VALUE 'PJ',
           c_projx    TYPE char7  VALUE 'Project'.


 DATA: v_bukrs   TYPE t001-bukrs,
       v_name    TYPE VRM_ID,
       v_folder  TYPE string,
       v_message TYPE bapi_msg,
       i_list    TYPE VRM_VALUES,
       wa_value  TYPE VRM_VALUE.


DATA:
      v_logical_system TYPE tbdls-logsys,
      v_logsys_name    TYPE char64,
      v_erro           TYPE xfeld,
      v_pos            TYPE i.


DATA: go_zcte_utils TYPE REF TO zcte_cl_utils.

FIELD-SYMBOLS: <l_fs_field> TYPE any.


SELECTION-SCREEN BEGIN OF SCREEN 0100 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

  SELECTION-SCREEN  SKIP.

  PARAMETERS: p_cc AS CHECKBOX,
              p_pj AS CHECKBOX,
              p_io AS CHECKBOX.

  SELECTION-SCREEN ULINE /2(70).

  PARAMETERS: p_lang TYPE spras DEFAULT 'E'.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.

  PARAMETERS: p_online RADIOBUTTON GROUP g1 USER-COMMAND cmd,
              p_back   RADIOBUTTON GROUP g1 DEFAULT 'X'.

  SELECTION-SCREEN SKIP.

  PARAMETERS: p_folder TYPE zcte_outbound_dir.

  PARAMETERS: p_entity TYPE zcte_entity_id AS LISTBOX VISIBLE LENGTH 12 OBLIGATORY USER-COMMAND entity.
  PARAMETERS: p_list  TYPE tdline DEFAULT 'Org Structure'.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.
  PARAMETERS: p_log  AS CHECKBOX,
              p_dele AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B3.

SELECTION-SCREEN END OF SCREEN 0100.

SELECTION-SCREEN BEGIN OF SCREEN 0300 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK B5 WITH FRAME TITLE text-t05.

    SELECT-OPTIONS: so_kokrs FOR csks-kokrs OBLIGATORY,
                    so_bukrs FOR csks-bukrs,
                    so_gsber FOR csks-gsber,
                    so_kostl FOR csks-kostl.

  SELECTION-SCREEN END OF  BLOCK B5.

SELECTION-SCREEN END OF SCREEN 0300.

SELECTION-SCREEN BEGIN OF SCREEN 0310 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK B7 WITH FRAME TITLE text-t07.

    SELECT-OPTIONS: so_pkokr FOR prps-pkokr OBLIGATORY,
                    so_pbukr FOR prps-pbukr,
                    so_pgsbr FOR prps-pgsbr,
                    so_pspnr FOR prps-pspnr.

  SELECTION-SCREEN END OF  BLOCK B7.

SELECTION-SCREEN END OF SCREEN 0310.

SELECTION-SCREEN BEGIN OF SCREEN 0320 AS SUBSCREEN.

  SELECTION-SCREEN BEGIN OF BLOCK B8 WITH FRAME TITLE text-t07.

    SELECT-OPTIONS: so_okokr  FOR aufk-kokrs  OBLIGATORY,
                    so_oauar FOR aufk-auart OBLIGATORY,
                    so_obukr FOR aufk-bukrs,
                    so_ogsbr FOR aufk-gsber,
                    so_aufnr FOR aufk-aufnr.

  SELECTION-SCREEN END OF  BLOCK B8.

SELECTION-SCREEN END OF SCREEN 0320.


SELECTION-SCREEN: BEGIN OF TABBED BLOCK tab FOR 40 LINES,
                  TAB (20) button1 USER-COMMAND TAB1,
                  TAB (20) button2 USER-COMMAND TAB2,
                  TAB (20) button3 USER-COMMAND TAB3,
                  TAB (20) button4 USER-COMMAND TAB4,
                  END OF BLOCK tab.

INITIALIZATION.

  button1 = 'Output'              ##NO_TEXT.
  button2 = 'Cost Center'         ##NO_TEXT.
  button3 = 'Project'             ##NO_TEXT.
  button4 = 'Internal Order'      ##NO_TEXT.
  tab-prog = sy-repid.
  tab-dynnr = 100.
  tab-activetab = 'PUSH1'.

  PERFORM f_get_entity_list.

AT SELECTION-SCREEN OUTPUT.

  PERFORM f_set_directory.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
  IF p_online EQ 'X'.
    p_folder = zcte_cl_file_handle=>online_folder_selection( ).
  ELSE.
    p_folder = zcte_cl_file_handle=>background_folder_selection( ).
  ENDIF.


AT SELECTION-SCREEN.

    CASE sscrfields-ucomm.
      WHEN 'TAB1'.
        tab-dynnr = 0100.
      WHEN 'TAB2'.
        tab-dynnr = 0300.
      WHEN 'TAB3'.
        tab-dynnr = 0310.
      WHEN 'TAB4'.
        tab-dynnr = 0320.
    ENDCASE.


START-OF-SELECTION.

  CLEAR v_erro.

  PERFORM: f_create_log_obj,
           f_select_common_data,
           f_build_lists,
           f_file_gen.

END-OF-SELECTION.
  PERFORM:  f_download_csv     CHANGING v_erro,
            f_update_log_table CHANGING v_erro.

  s_log-log_obj->save_log( ).
  s_log-log_obj->display_log( ).




*&---------------------------------------------------------------------*
*&      Form  F_CREATE_LOG_OBJ
*&---------------------------------------------------------------------*
FORM F_CREATE_LOG_OBJ .


  DATA: l_v_extnumber TYPE bal_s_log-extnumber.


  IF NOT s_log-log_obj IS BOUND.

    CONCATENATE 'OS_Extract_' sy-datum sy-uzeit INTO l_v_extnumber.

    CREATE OBJECT s_log-log_obj
      EXPORTING
      im_object    = 'ZCTE'
      im_subobject = 'ZCTE03'
      im_extnumber = l_v_extnumber
    EXCEPTIONS
      not_created  = 1
      OTHERS       = 2.

    IF sy-subrc EQ 0.
*       Error handling here
    ENDIF.

  ENDIF.

ENDFORM.                    " F_CREATE_LOG_OBJ

*&---------------------------------------------------------------------*
*& Form SELECT_COMMON_DATA
*&---------------------------------------------------------------------*
FORM f_select_common_data .

  DATA: l_v_fieldname TYPE char20,
        l_v_index     TYPE n.
  CLEAR: i_log, i_del.

  FIELD-SYMBOLS: <l_fs_field> TYPE ANY.


  READ TABLE i_zcte_t001 INTO s_zcte_t001 WITH KEY entity_id = p_entity BINARY SEARCH.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH 'Entity error'.
  ENDIF.

  CLEAR s_orgunit_fields.
  DO 6 TIMES.

    l_v_index = sy-index.

    CONCATENATE 'ORG_UNIT' l_v_index INTO l_v_fieldname.
    ASSIGN COMPONENT l_v_fieldname OF STRUCTURE s_zcte_t001 TO <l_fs_field>.
    IF sy-subrc EQ 0 AND <l_fs_field> IS ASSIGNED.
       CASE <l_fs_field>.
         WHEN 'LOGICAL SYSTEM'.
           s_orgunit_fields-logical_system = l_v_fieldname.
         WHEN 'CONTROLLING AREA'.
           s_orgunit_fields-acc_area       = l_v_fieldname.
         WHEN 'COUNTRY'.
           s_orgunit_fields-country        = l_v_fieldname.
         WHEN 'COMPANY CODE'.
           s_orgunit_fields-company_code   = l_v_fieldname.
         WHEN 'BUSINESS AREA'.
           s_orgunit_fields-bus_area       = l_v_fieldname.
         WHEN 'COST OBJECT TYPE'.
           s_orgunit_fields-cost_obj_type  = l_v_fieldname.
         WHEN 'COST OBJECT'.
           s_orgunit_fields-cost_obj_id    = l_v_fieldname.
       ENDCASE.
    ENDIF.
  ENDDO.


  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system             = v_logical_system
    EXCEPTIONS
      own_logical_system_not_defined = 1
      OTHERS                         = 2.

  CONCATENATE sy-sysid sy-mandt INTO v_logsys_name SEPARATED BY space.


*** Get controlling areas

   SELECT kokrs bezei
     FROM tka01
     INTO TABLE i_tka01.
   IF sy-subrc EQ 0.
      SORT i_tka01 BY kokrs.
   ENDIF.

** Get company code and country
  SELECT a~bukrs a~butxt a~land1 b~landx50
    FROM t001 AS A
    INNER JOIN t005T AS b
    ON a~land1 EQ b~land1
    INTO TABLE i_t001
    WHERE a~bukrs IN so_bukrs
      AND b~spras EQ p_lang.

  IF sy-subrc EQ 0.
     SORT i_t001 BY bukrs.
  ENDIF.

** Get Business Area

   SELECT gsber gtext
     FROM tgsbt
     INTO TABLE i_tgsbt
     WHERE spras EQ p_lang.
   IF sy-subrc EQ 0.
     SORT i_tgsbt BY gsber.
   ENDIF.


ENDFORM.                    "SELECT_COMMON_DATA

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_LISTS
*&---------------------------------------------------------------------*
FORM f_build_lists .

  DATA: l_i_csks   TYPE STANDARD TABLE OF ty_csks,
        l_i_cskt   TYPE STANDARD TABLE OF ty_cskt,
        l_i_cc_del TYPE ty_t_cc_del,
        l_i_prps   TYPE ty_t_prps,
        l_i_pj_del TYPE ty_t_pj_del,
        l_i_aufk   TYPE STANDARD TABLE OF ty_aufk,
        l_i_io_del TYPE ty_t_io_del.


  PERFORM: f_select_cost_tables TABLES l_i_csks
                                       l_i_cskt
                                       l_i_cc_del
                                       l_i_prps
                                       l_i_pj_del
                                       l_i_aufk
                                       l_i_io_del.

  PERFORM: f_output_prepair TABLES l_i_csks
                                   l_i_cskt
                                   l_i_cc_del
                                   l_i_prps
                                   l_i_pj_del
                                   l_i_aufk
                                   l_i_io_del.


ENDFORM.                    " F_BUILD_LISTS

**&---------------------------------------------------------------------*
**&      Form  SELECT_COST_TABLES
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM f_select_cost_tables TABLES pt_i_csks   STRUCTURE s_csks
                                 pt_i_cskt   STRUCTURE s_cskt
                                 pt_i_cc_del TYPE ty_t_cc_del
                                 pt_i_prps   TYPE ty_t_prps
                                 pt_i_pj_del TYPE ty_t_pj_del
                                 pt_i_aufk   STRUCTURE s_aufk
                                 pt_i_io_del TYPE ty_t_io_del.

  IF p_cc EQ abap_true.
     PERFORM: f_get_cc TABLES pt_i_csks
                              pt_i_cskt
                              pt_i_cc_del.
  ENDIF.

  IF p_pj EQ abap_true.
      PERFORM: f_get_pj TABLES pt_i_prps
                               pt_i_pj_del.
  ENDIF.

  IF p_io EQ abap_true.
     PERFORM f_get_io TABLES pt_i_aufk
                             pt_i_io_del.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_GET_CC
*&---------------------------------------------------------------------*
FORM f_get_cc TABLES pt_i_csks   TYPE ty_t_csks
                     pt_i_cskt   TYPE ty_t_cskt
                     pt_i_cc_del TYPE ty_t_cc_del.

  TYPES: BEGIN OF l_ty_csks_del,
*            kokrs   TYPE csks-kokrs,
            kostl   TYPE csks-kostl,
            datbi   TYPE csks-datbi,
            bkzkp   TYPE csks-bkzkp,
         END OF l_ty_csks_del.

  DATA: "l_i_csks         TYPE STANDARD TABLE OF l_ty_csks,
        l_i_csks_del     TYPE STANDARD TABLE OF l_ty_csks_del.

  DATA: l_v_line  TYPE i,
        l_v_where TYPE char100.

  CONSTANTS: l_c_en           TYPE spras VALUE 'E'.

  FIELD-SYMBOLS: "<l_fs_csks_final>  TYPE ty_csks,
                 <l_fs_csks>        TYPE ty_csks,
                 <l_fs_log>         TYPE zcte_fit006,
                 <l_fs_del>         TYPE zcte_fit006,
                 <l_fs_cc_del>      TYPE ty_cc_del,
                 <l_fs_csks_del>    TYPE l_ty_csks_del,
                 <l_fs_field>       TYPE any.



  SELECT kokrs kostl bukrs gsber
    FROM csks
    INTO TABLE pt_i_csks
    WHERE kokrs IN so_kokrs
      AND kostl IN so_kostl
      AND bukrs IN so_bukrs
      AND gsber IN so_gsber
      AND datbi GE sy-datum
      AND datab LE sy-datum
      AND bkzkp EQ abap_false.

   IF sy-subrc EQ 0.

      SORT pt_i_csks BY kokrs bukrs gsber kostl.
      DELETE ADJACENT DUPLICATES FROM pt_i_csks.

      SELECT kokrs kostl spras ktext
        FROM cskt
        INTO TABLE pt_i_cskt
        FOR ALL ENTRIES IN pt_i_csks
        WHERE kokrs EQ pt_i_csks-kokrs
          AND kostl EQ pt_i_csks-kostl
          AND ( spras EQ p_lang OR
                spras EQ l_c_en ).

       SORT pt_i_cskt BY kokrs kostl spras.
       DELETE ADJACENT DUPLICATES FROM pt_i_cskt.

   ENDIF.

   IF p_dele EQ abap_true.

      CONCATENATE 'entity_id EQ p_entity AND'
                   s_orgunit_fields-cost_obj_type
                  'EQ ''CC'' AND del EQ abap_false'
             INTO l_v_where SEPARATED BY space.


      SELECT *
        FROM zcte_fit006
        INTO TABLE pt_i_cc_del ##TOO_MANY_ITAB_FIELDS
        WHERE (l_v_where).

      IF sy-subrc EQ 0.

         LOOP AT pt_i_cc_del ASSIGNING <l_fs_cc_del>.
           ASSIGN COMPONENT s_orgunit_fields-cost_obj_id OF STRUCTURE <l_fs_cc_del> TO <l_fs_field>.

           CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input         = <l_fs_field>
           IMPORTING
              OUTPUT        = <l_fs_cc_del>-kostl.
         ENDLOOP.

         SELECT kostl datbi bkzkp
           FROM csks
           INTO TABLE l_i_csks_del
           FOR ALL ENTRIES IN pt_i_cc_del
           WHERE kostl EQ pt_i_cc_del-kostl.

         IF sy-subrc EQ 0.
           " Sorting by datbi to keep only the most recent copies
            SORT l_i_csks_del BY kostl ASCENDING datbi DESCENDING.
            DELETE ADJACENT DUPLICATES FROM l_i_csks_del COMPARING kostl.

            LOOP AT pt_i_cc_del ASSIGNING <l_fs_cc_del>.

               l_v_line = sy-tabix.
               READ TABLE l_i_csks_del WITH KEY kostl = <l_fs_cc_del>-kostl
                                       ASSIGNING <l_fs_csks_del> BINARY SEARCH.
               IF sy-subrc EQ 0.
                 IF <l_fs_csks_del>-bkzkp EQ abap_false AND
                    <l_fs_csks_del>-datbi GE sy-datum.
                    DELETE pt_i_cc_del INDEX l_v_line.
                 ENDIF.
               ENDIF.
             ENDLOOP.

             LOOP AT pt_i_cc_del ASSIGNING <l_fs_cc_del>.
                APPEND INITIAL LINE TO i_del ASSIGNING <l_fs_del>.
                <l_fs_del>             = <l_fs_cc_del>.
                <l_fs_del>-del         = abap_true.
             ENDLOOP.
         ENDIF.
       ENDIF.
     ENDIF.


ENDFORM.                    " F_GET_CC

*&---------------------------------------------------------------------*
*&      Form  F_GET_PJ
*&---------------------------------------------------------------------*
FORM f_get_pj TABLES pt_i_prps   TYPE ty_t_prps
                     pt_i_pj_del TYPE ty_t_pj_del.


  TYPES:
         BEGIN OF l_ty_prps_del,
              pspnr   TYPE  prps-pspnr,
              loevm   TYPE  prps-loevm,
              belkz   TYPE  prps-belkz,
         END OF l_ty_prps_del.

  DATA: l_i_prps_del     TYPE TABLE OF l_ty_prps_del.

  DATA: l_v_index        TYPE sy-tabix,
        l_v_where        TYPE char100.


  FIELD-SYMBOLS: <l_fs_prps>           TYPE ty_prps,
                 <l_fs_prps_del>       TYPE l_ty_prps_del,
                 <l_fs_pj_del>         TYPE ty_pj_del,
                 <l_fs_log>            TYPE zcte_fit006,
                 <l_fs_del>            TYPE zcte_fit006.



  SELECT pspnr post1 pbukr pgsbr pkokr
    FROM prps
    INTO TABLE pt_i_prps
    WHERE pkokr IN so_pkokr
      AND pbukr IN so_pbukr
      AND pgsbr IN so_pgsbr
      AND pspnr IN so_pspnr
      AND loevm EQ abap_false
      AND belkz EQ abap_true.

   IF sy-subrc EQ 0.
      SORT pt_i_prps BY pkokr pbukr pgsbr pspnr.
   ENDIF.

   IF p_dele EQ abap_true.

      CONCATENATE 'entity_id EQ p_entity AND'
                   s_orgunit_fields-cost_obj_type
                  'EQ ''PJ'' AND del EQ abap_false'
             INTO l_v_where SEPARATED BY space.

     SELECT *
       FROM zcte_fit006
       INTO TABLE pt_i_pj_del ##TOO_MANY_ITAB_FIELDS
       WHERE (l_v_where).

     IF sy-subrc EQ 0.

        LOOP AT pt_i_pj_del ASSIGNING <l_fs_pj_del>.
           ASSIGN COMPONENT s_orgunit_fields-cost_obj_id OF STRUCTURE <l_fs_pj_del> TO <l_fs_field>.
           IF sy-subrc EQ 0.

           CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
            EXPORTING
              input         = <l_fs_field>
           IMPORTING
              OUTPUT        =  <l_fs_pj_del>-pspnr.

           ENDIF.
        ENDLOOP.

        SORT pt_i_pj_del BY pspnr.

        SELECT pspnr loevm belkz
          FROM prps
          INTO TABLE l_i_prps_del
          FOR ALL ENTRIES IN pt_i_pj_del
          WHERE pspnr EQ pt_i_pj_del-pspnr.

        IF sy-subrc EQ 0.

          SORT l_i_prps_del BY pspnr.
          LOOP AT pt_i_pj_del ASSIGNING <l_fs_pj_del>.

            l_v_index   = sy-tabix.
            READ TABLE l_i_prps_del WITH KEY pspnr = <l_fs_pj_del>-pspnr
                                    ASSIGNING <l_fs_prps_del> BINARY SEARCH.
            IF sy-subrc EQ 0.

              IF <l_fs_prps_del>-loevm EQ abap_false AND
                 <l_fs_prps_del>-belkz EQ abap_true.

                 DELETE pt_i_pj_del INDEX l_v_index.
              ENDIF.
            ENDIF.
          ENDLOOP.

          LOOP AT pt_i_pj_del ASSIGNING <l_fs_pj_del>.
             APPEND INITIAL LINE TO i_del ASSIGNING <l_fs_del>.
             <l_fs_del>             = <l_fs_pj_del>.
             <l_fs_del>-del         = abap_true.
          ENDLOOP.
        ENDIF.
     ENDIF.
   ENDIF.

ENDFORM.                    " F_GET_PJ

*&---------------------------------------------------------------------*
*&      Form  F_GET_IO
*&---------------------------------------------------------------------*
FORM f_get_io TABLES pt_i_aufk   TYPE ty_t_aufk
                     pt_i_io_del TYPE ty_t_io_del.


  TYPES: BEGIN OF l_ty_aufk_del,
              aufnr    TYPE  aufk-aufnr,
              autyp    TYPE  aufk-autyp,
              loekz    TYPE  aufk-loekz,
         END OF l_ty_aufk_del.

  DATA: l_i_aufk_del  TYPE STANDARD TABLE OF l_ty_aufk_del.

  DATA: l_v_index        TYPE sy-tabix,
        l_v_where        TYPE char100.


  FIELD-SYMBOLS: <l_fs_aufk>           TYPE ty_aufk,
                 <l_fs_aufk_del>       TYPE l_ty_aufk_del,
                 <l_fs_io_del>         TYPE ty_io_del,
                 <l_fs_del>            TYPE zcte_fit006,
                 <l_fs_log>            TYPE zcte_fit006.


   SELECT aufnr bukrs gsber
          kokrs ktext
    FROM aufk
    INTO TABLE pt_i_aufk
    WHERE auart IN so_oauar
      AND kokrs IN so_okokr
      AND bukrs IN so_obukr
      AND gsber IN so_ogsbr
      AND aufnr IN so_aufnr
      AND autyp EQ '01'
      AND loekz EQ space.

   IF sy-subrc EQ 0.
      SORT pt_i_aufk BY kokrs bukrs gsber aufnr.
   ENDIF.


   IF p_dele EQ abap_true.

      CONCATENATE 'entity_id EQ p_entity AND'
                   s_orgunit_fields-cost_obj_type
                  'EQ ''IO'' AND del EQ abap_false'
             INTO l_v_where SEPARATED BY space.

      SELECT *
        FROM zcte_fit006
        INTO TABLE pt_i_io_del ##TOO_MANY_ITAB_FIELDS
        WHERE (l_v_where).

      IF sy-subrc EQ 0.
         LOOP AT pt_i_io_del ASSIGNING <l_fs_io_del>.
            ASSIGN COMPONENT s_orgunit_fields-cost_obj_id OF STRUCTURE <l_fs_io_del> TO <l_fs_field>.
            IF sy-subrc EQ 0.

               CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input         = <l_fs_field>
               IMPORTING
                  OUTPUT        =  <l_fs_io_del>-aufnr.

            ENDIF.
         ENDLOOP.

         SORT pt_i_io_del BY aufnr.

         SELECT  aufnr autyp loekz
           FROM aufk
           INTO TABLE l_i_aufk_del
           FOR ALL ENTRIES IN pt_i_io_del
           WHERE aufnr = pt_i_io_del-aufnr.

         IF sy-subrc EQ 0.

            SORT l_i_aufk_del BY aufnr.

            LOOP AT pt_i_io_del ASSIGNING <l_fs_io_del>.

              l_v_index   = sy-tabix.
              READ TABLE l_i_aufk_del WITH KEY aufnr = <l_fs_io_del>-aufnr
                                      ASSIGNING <l_fs_aufk_del> BINARY SEARCH.

              IF sy-subrc EQ 0.

                 IF <l_fs_aufk_del>-autyp EQ '01' AND
                    <l_fs_aufk_del>-loekz EQ abap_false .

                    DELETE pt_i_io_del INDEX l_v_index.
                 ENDIF.
              ENDIF.
           ENDLOOP.

           LOOP AT pt_i_io_del ASSIGNING <l_fs_io_del>.
              APPEND INITIAL LINE TO i_del ASSIGNING <l_fs_del>.
              <l_fs_del>             = <l_fs_io_del>.
              <l_fs_del>-del         = abap_true.
           ENDLOOP.
         ENDIF.
       ENDIF.
    ENDIF.



ENDFORM.                    " F_GET_IO

*&---------------------------------------------------------------------*
*&      Form  OUTPUT_PREPAIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_output_prepair TABLES pt_i_csks STRUCTURE s_csks
                             pt_i_cskt STRUCTURE s_cskt
                             pt_i_cc_del  TYPE ty_t_cc_del
                             pt_i_prps    TYPE ty_t_prps
                             pt_i_pj_del  TYPE ty_t_pj_del
                             pt_i_aufk STRUCTURE s_aufk
                             pt_i_io_del  TYPE ty_t_io_del.


  DATA: l_s_orgunit1     TYPE ty_orgunit_item,
        l_s_orgunit2     TYPE ty_orgunit_item,
        l_s_orgunit3     TYPE ty_orgunit_item,
        l_s_orgunit4     TYPE ty_orgunit_item,
        l_s_orgunit5     TYPE ty_orgunit_item,
        l_s_orgunit6     TYPE ty_orgunit_item,
        l_s_del          TYPE zcte_fit006.

  DATA: l_v_pos          TYPE i VALUE 2,
        l_v_orgunit1_ant TYPE char48,
        l_v_orgunit2_ant TYPE char48,
        l_v_orgunit3_ant TYPE char48,
        l_v_orgunit4_ant TYPE char48,
        l_v_orgunit5_ant TYPE char48,
        l_v_orgunit6_ant TYPE char48.

  FIELD-SYMBOLS: <l_fs_orgs>        TYPE ty_structure_orgs,
                 <l_fs_t001>        TYPE ty_t001,
                 <l_fs_csks>        TYPE ty_csks,
                 <l_fs_cskt>        TYPE ty_cskt,
                 <l_fs_cc_del>      TYPE ty_cc_del,
                 <l_fs_prps>        TYPE ty_prps,
                 <l_fs_pj_del>      TYPE ty_pj_del,
                 <l_fs_aufk>        TYPE ty_aufk,
                 <l_fs_io_del>      TYPE ty_io_del.

  CLEAR: i_orgs, i_log.

  IF p_cc EQ abap_true.
    LOOP AT pt_i_csks ASSIGNING <l_fs_csks>.

        READ TABLE pt_i_cskt ASSIGNING <l_fs_cskt> WITH KEY kokrs = <l_fs_csks>-kokrs
                                                            kostl = <l_fs_csks>-kostl
                                                            spras = p_lang BINARY SEARCH.
        IF sy-subrc EQ 0.
          PERFORM f_build_orgunit_costcenter USING    <l_fs_csks>
                                                      <l_fs_cskt>
                                             CHANGING l_s_orgunit1
                                                      l_s_orgunit2
                                                      l_s_orgunit3
                                                      l_s_orgunit4
                                                      l_s_orgunit5
                                                      l_s_orgunit6.

          IF p_log EQ abap_true.
            PERFORM f_add_log_entry USING l_s_orgunit1
                                          l_s_orgunit2
                                          l_s_orgunit3
                                          l_s_orgunit4
                                          l_s_orgunit5
                                          l_s_orgunit6.
          ENDIF.


           IF l_v_orgunit1_ant NE l_s_orgunit1-code AND NOT
              l_s_orgunit1-code IS INITIAL.

              l_v_orgunit1_ant = l_s_orgunit1-code.
              CLEAR: l_v_orgunit2_ant,
                     l_v_orgunit3_ant,
                     l_v_orgunit4_ant,
                     l_v_orgunit5_ant,
                     l_v_orgunit6_ant.
              PERFORM f_add_list_item USING 1 l_s_orgunit1.
           ENDIF.
           IF l_v_orgunit2_ant NE l_s_orgunit2-code AND NOT
              l_s_orgunit2-code IS INITIAL.

              l_v_orgunit2_ant = l_s_orgunit2-code.
              CLEAR: l_v_orgunit3_ant,
                     l_v_orgunit4_ant,
                     l_v_orgunit5_ant,
                     l_v_orgunit6_ant.
              PERFORM f_add_list_item USING 2 l_s_orgunit2.
           ENDIF.
           IF l_v_orgunit3_ant NE l_s_orgunit3-code AND NOT
              l_s_orgunit3-code IS INITIAL.

              l_v_orgunit3_ant = l_s_orgunit3-code.
              CLEAR: l_v_orgunit4_ant,
                     l_v_orgunit5_ant,
                     l_v_orgunit6_ant.
              PERFORM f_add_list_item USING 3 l_s_orgunit3.
           ENDIF.
           IF l_v_orgunit4_ant NE l_s_orgunit4-code AND NOT
              l_s_orgunit4-code IS INITIAL.

              l_v_orgunit4_ant = l_s_orgunit4-code.
              CLEAR: l_v_orgunit5_ant,
                     l_v_orgunit6_ant.
              PERFORM f_add_list_item USING 4 l_s_orgunit4.
           ENDIF.
           IF l_v_orgunit5_ant NE l_s_orgunit5-code AND NOT
              l_s_orgunit5-code IS INITIAL.

              l_v_orgunit5_ant = l_s_orgunit5-code.
              CLEAR: l_v_orgunit6_ant.
              PERFORM f_add_list_item USING 5 l_s_orgunit5.
           ENDIF.
           IF l_v_orgunit6_ant NE l_s_orgunit6-code AND NOT
              l_s_orgunit6-code IS INITIAL.

              l_v_orgunit6_ant = l_s_orgunit6-code.
              PERFORM f_add_list_item USING 6 l_s_orgunit6.
           ENDIF.

        ENDIF.
     ENDLOOP.
  ENDIF.

  IF p_pj EQ abap_true.

    LOOP AT pt_i_prps ASSIGNING <l_fs_prps>.

        PERFORM f_build_orgunit_project USING       <l_fs_prps>
                                           CHANGING l_s_orgunit1
                                                    l_s_orgunit2
                                                    l_s_orgunit3
                                                    l_s_orgunit4
                                                    l_s_orgunit5
                                                    l_s_orgunit6.

          IF p_log EQ abap_true.
            PERFORM f_add_log_entry USING l_s_orgunit1
                                          l_s_orgunit2
                                          l_s_orgunit3
                                          l_s_orgunit4
                                          l_s_orgunit5
                                          l_s_orgunit6.
          ENDIF.

         IF l_v_orgunit1_ant NE l_s_orgunit1-code AND NOT
            l_s_orgunit1-code IS INITIAL.

            l_v_orgunit1_ant = l_s_orgunit1-code.
            CLEAR: l_v_orgunit2_ant,
                   l_v_orgunit3_ant,
                   l_v_orgunit4_ant,
                   l_v_orgunit5_ant,
                   l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 1 l_s_orgunit1.
         ENDIF.
         IF l_v_orgunit2_ant NE l_s_orgunit2-code AND NOT
            l_s_orgunit2-code IS INITIAL.

            l_v_orgunit2_ant = l_s_orgunit2-code.
            CLEAR: l_v_orgunit3_ant,
                   l_v_orgunit4_ant,
                   l_v_orgunit5_ant,
                   l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 2 l_s_orgunit2.
         ENDIF.
         IF l_v_orgunit3_ant NE l_s_orgunit3-code AND NOT
            l_s_orgunit3-code IS INITIAL.

            l_v_orgunit3_ant = l_s_orgunit3-code.
            CLEAR: l_v_orgunit4_ant,
                   l_v_orgunit5_ant,
                   l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 3 l_s_orgunit3.
         ENDIF.
         IF l_v_orgunit4_ant NE l_s_orgunit4-code AND NOT
            l_s_orgunit4-code IS INITIAL.

            l_v_orgunit4_ant = l_s_orgunit4-code.
            CLEAR: l_v_orgunit5_ant,
                   l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 4 l_s_orgunit4.
         ENDIF.
         IF l_v_orgunit5_ant NE l_s_orgunit5-code AND NOT
            l_s_orgunit5-code IS INITIAL.

            l_v_orgunit5_ant = l_s_orgunit5-code.
            CLEAR: l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 5 l_s_orgunit5.
         ENDIF.
         IF l_v_orgunit6_ant NE l_s_orgunit6-code AND NOT
            l_s_orgunit6-code IS INITIAL.

            l_v_orgunit6_ant = l_s_orgunit6-code.
            PERFORM f_add_list_item USING 6 l_s_orgunit6.
         ENDIF.
     ENDLOOP.
  ENDIF.


  IF p_io EQ abap_true.

    LOOP AT pt_i_aufk ASSIGNING <l_fs_aufk>.

        PERFORM f_build_orgunit_intorder   USING    <l_fs_aufk>
                                           CHANGING l_s_orgunit1
                                                    l_s_orgunit2
                                                    l_s_orgunit3
                                                    l_s_orgunit4
                                                    l_s_orgunit5
                                                    l_s_orgunit6.

        IF p_log EQ abap_true.
          PERFORM f_add_log_entry USING l_s_orgunit1
                                        l_s_orgunit2
                                        l_s_orgunit3
                                        l_s_orgunit4
                                        l_s_orgunit5
                                        l_s_orgunit6.
        ENDIF.

         IF l_v_orgunit1_ant NE l_s_orgunit1-code AND NOT
            l_s_orgunit1-code IS INITIAL.

            l_v_orgunit1_ant = l_s_orgunit1-code.
            CLEAR: l_v_orgunit2_ant,
                   l_v_orgunit3_ant,
                   l_v_orgunit4_ant,
                   l_v_orgunit5_ant,
                   l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 1 l_s_orgunit1.
         ENDIF.
         IF l_v_orgunit2_ant NE l_s_orgunit2-code AND NOT
            l_s_orgunit2-code IS INITIAL.

            l_v_orgunit2_ant = l_s_orgunit2-code.
            CLEAR: l_v_orgunit3_ant,
                   l_v_orgunit4_ant,
                   l_v_orgunit5_ant,
                   l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 2 l_s_orgunit2.
         ENDIF.
         IF l_v_orgunit3_ant NE l_s_orgunit3-code AND NOT
            l_s_orgunit3-code IS INITIAL.

            l_v_orgunit3_ant = l_s_orgunit3-code.
            CLEAR: l_v_orgunit4_ant,
                   l_v_orgunit5_ant,
                   l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 3 l_s_orgunit3.
         ENDIF.
         IF l_v_orgunit4_ant NE l_s_orgunit4-code AND NOT
            l_s_orgunit4-code IS INITIAL.

            l_v_orgunit4_ant = l_s_orgunit4-code.
            CLEAR: l_v_orgunit5_ant,
                   l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 4 l_s_orgunit4.
         ENDIF.
         IF l_v_orgunit5_ant NE l_s_orgunit5-code AND NOT
            l_s_orgunit5-code IS INITIAL.

            l_v_orgunit5_ant = l_s_orgunit5-code.
            CLEAR: l_v_orgunit6_ant.
            PERFORM f_add_list_item USING 5 l_s_orgunit5.
         ENDIF.
         IF l_v_orgunit6_ant NE l_s_orgunit6-code AND NOT
            l_s_orgunit6-code IS INITIAL.

            l_v_orgunit6_ant = l_s_orgunit6-code.
            PERFORM f_add_list_item USING 6 l_s_orgunit6.
         ENDIF.
     ENDLOOP.
  ENDIF.


  LOOP AT pt_i_cc_del ASSIGNING <l_fs_cc_del>.

    l_s_del = <l_fs_cc_del>.
    ASSIGN COMPONENT s_orgunit_fields-cost_obj_id OF STRUCTURE l_s_del TO <l_fs_field>.
    IF sy-subrc EQ 0 AND <l_fs_field> IS ASSIGNED.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input         = <l_fs_field>
*       IMPORTING
*          OUTPUT        = <l_fs_field>.
    ENDIF.

     PERFORM f_add_del_list_item USING  l_s_del.
  ENDLOOP.

  LOOP AT pt_i_pj_del ASSIGNING <l_fs_pj_del>.

    l_s_del = <l_fs_pj_del>.
    ASSIGN COMPONENT s_orgunit_fields-cost_obj_id OF STRUCTURE l_s_del TO <l_fs_field>.
    IF sy-subrc EQ 0 AND <l_fs_field> IS ASSIGNED.

*      CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*        EXPORTING
*          input         = <l_fs_field>
*       IMPORTING
*          OUTPUT        = <l_fs_field>.
    ENDIF.

     PERFORM f_add_del_list_item USING  l_s_del.
  ENDLOOP.

  LOOP AT pt_i_io_del ASSIGNING <l_fs_io_del>.

    l_s_del = <l_fs_io_del>.
    ASSIGN COMPONENT s_orgunit_fields-cost_obj_id OF STRUCTURE l_s_del TO <l_fs_field>.
    IF sy-subrc EQ 0 AND <l_fs_field> IS ASSIGNED.

*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input         = <l_fs_field>
*       IMPORTING
*          OUTPUT        = <l_fs_field>.
    ENDIF.

     PERFORM f_add_del_list_item USING  l_s_del.
  ENDLOOP.

ENDFORM.                    " OUTPUT_PREPAIR

*&---------------------------------------------------------------------*
*& Form F_BUILD_ORGUNIT
*&---------------------------------------------------------------------*
FORM F_BUILD_ORGUNIT_COSTCENTER  USING    PU_S_CSKS TYPE ty_csks
                                          PU_S_CSKT TYPE ty_cskt
                                 CHANGING PC_S_ORGUNIT1 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT2 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT3 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT4 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT5 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT6 TYPE ty_orgunit_item.


  DATA: l_s_bukrs    TYPE ty_orgunit_item,
        l_s_ls       TYPE ty_orgunit_item,
        l_s_co_type  TYPE ty_orgunit_item,
        l_s_cost_obj TYPE ty_orgunit_item,
        l_s_contarea TYPE ty_orgunit_item,
        l_s_busarea TYPE ty_orgunit_item,
        l_s_country  TYPE ty_orgunit_item.

   FIELD-SYMBOLS: <l_fs_t001>  TYPE ty_t001,
                  <l_fs_tka01> TYPE ty_tka01,
                  <l_fs_tgsbt>  TYPE ty_tgsbt.

** Controlling area
   READ TABLE i_tka01 WITH KEY kokrs = pu_s_csks-kokrs
                     ASSIGNING <l_fs_tka01> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_contarea-code     = <l_fs_tka01>-kokrs.
      l_s_contarea-name     = <l_fs_tka01>-bezei.
   ENDIF.

** Company code
   READ TABLE i_t001 WITH KEY bukrs = pu_s_csks-bukrs
                     ASSIGNING <l_fs_t001> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_bukrs-code     = <l_fs_t001>-bukrs.
      l_s_bukrs-name     = <l_fs_t001>-butxt.
** Country
      l_s_country-code   = <l_fs_t001>-land1.
      l_s_country-name   = <l_fs_t001>-landx50.
   ENDIF.

** Business Area
   READ TABLE i_tgsbt WITH KEY gsber = pu_s_csks-gsber
                     ASSIGNING <l_fs_tgsbt> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_busarea-code     = <l_fs_tgsbt>-gsber.
      l_s_busarea-name     = <l_fs_tgsbt>-gtext.
   ENDIF.

** Cost Object Type
   l_s_co_type-code        = 'CC'.
   l_s_co_type-name        = text-t01. "Cost Center

** Cost Object ID
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
     EXPORTING
       input         =  pu_s_csks-kostl
    IMPORTING
       OUTPUT        =  l_s_cost_obj-code.

   l_s_cost_obj-name       = pu_s_cskt-ktext.


   PERFORM f_build_orgunit USING l_s_bukrs
                                 l_s_co_type
                                 l_s_cost_obj
                                 l_s_contarea
                                 l_s_busarea
                                 l_s_country
                           CHANGING PC_S_ORGUNIT1
                                    PC_S_ORGUNIT2
                                    PC_S_ORGUNIT3
                                    PC_S_ORGUNIT4
                                    PC_S_ORGUNIT5
                                    PC_S_ORGUNIT6.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_BUILD_PROJECT
*&---------------------------------------------------------------------*
FORM F_BUILD_ORGUNIT_PROJECT  USING    PU_S_PRPS TYPE ty_prps
                              CHANGING PC_S_ORGUNIT1 TYPE ty_orgunit_item
                                       PC_S_ORGUNIT2 TYPE ty_orgunit_item
                                       PC_S_ORGUNIT3 TYPE ty_orgunit_item
                                       PC_S_ORGUNIT4 TYPE ty_orgunit_item
                                       PC_S_ORGUNIT5 TYPE ty_orgunit_item
                                       PC_S_ORGUNIT6 TYPE ty_orgunit_item.


  DATA: l_s_bukrs    TYPE ty_orgunit_item,
        l_s_ls       TYPE ty_orgunit_item,
        l_s_co_type  TYPE ty_orgunit_item,
        l_s_cost_obj TYPE ty_orgunit_item,
        l_s_contarea TYPE ty_orgunit_item,
        l_s_busarea TYPE ty_orgunit_item,
        l_s_country  TYPE ty_orgunit_item.

   FIELD-SYMBOLS: <l_fs_t001>  TYPE ty_t001,
                  <l_fs_tka01> TYPE ty_tka01,
                  <l_fs_tgsbt>  TYPE ty_tgsbt.

** Controlling area
   READ TABLE i_tka01 WITH KEY kokrs = pu_s_prps-pkokr
                     ASSIGNING <l_fs_tka01> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_contarea-code     = <l_fs_tka01>-kokrs.
      l_s_contarea-name     = <l_fs_tka01>-bezei.
   ENDIF.

** Company code
   READ TABLE i_t001 WITH KEY bukrs = pu_s_prps-pbukr
                     ASSIGNING <l_fs_t001> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_bukrs-code     = <l_fs_t001>-bukrs.
      l_s_bukrs-name     = <l_fs_t001>-butxt.
** Country
      l_s_country-code   = <l_fs_t001>-land1.
      l_s_country-name   = <l_fs_t001>-landx50.
   ENDIF.

** Business Area
   READ TABLE i_tgsbt WITH KEY gsber = pu_s_prps-pgsbr
                     ASSIGNING <l_fs_tgsbt> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_busarea-code     = <l_fs_tgsbt>-gsber.
      l_s_busarea-name     = <l_fs_tgsbt>-gtext.
   ENDIF.

** Cost Object Type
   l_s_co_type-code        = 'PJ'.
   l_s_co_type-name        = text-t02. "Project

** Cost Object ID
   l_s_cost_obj-code       = pu_s_prps-pspnr.
   l_s_cost_obj-name       = pu_s_prps-post1.

   CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
     EXPORTING
       input         = l_s_cost_obj-code
    IMPORTING
      OUTPUT        = l_s_cost_obj-code.


   PERFORM f_build_orgunit USING l_s_bukrs
                                 l_s_co_type
                                 l_s_cost_obj
                                 l_s_contarea
                                 l_s_busarea
                                 l_s_country
                           CHANGING PC_S_ORGUNIT1
                                    PC_S_ORGUNIT2
                                    PC_S_ORGUNIT3
                                    PC_S_ORGUNIT4
                                    PC_S_ORGUNIT5
                                    PC_S_ORGUNIT6.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_BUILD_ORGUNIT
*&---------------------------------------------------------------------*
FORM F_BUILD_ORGUNIT_INTORDER    USING    PU_S_AUFK TYPE ty_aufk
                                 CHANGING PC_S_ORGUNIT1 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT2 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT3 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT4 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT5 TYPE ty_orgunit_item
                                          PC_S_ORGUNIT6 TYPE ty_orgunit_item.


  DATA: l_s_bukrs    TYPE ty_orgunit_item,
        l_s_ls       TYPE ty_orgunit_item,
        l_s_co_type  TYPE ty_orgunit_item,
        l_s_cost_obj TYPE ty_orgunit_item,
        l_s_contarea TYPE ty_orgunit_item,
        l_s_busarea TYPE ty_orgunit_item,
        l_s_country  TYPE ty_orgunit_item.

   FIELD-SYMBOLS: <l_fs_t001>  TYPE ty_t001,
                  <l_fs_tka01> TYPE ty_tka01,
                  <l_fs_tgsbt>  TYPE ty_tgsbt.

** Controlling area
   READ TABLE i_tka01 WITH KEY kokrs = pu_s_aufk-kokrs
                     ASSIGNING <l_fs_tka01> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_contarea-code     = <l_fs_tka01>-kokrs.
      l_s_contarea-name     = <l_fs_tka01>-bezei.
   ENDIF.

** Company code
   READ TABLE i_t001 WITH KEY bukrs = pu_s_aufk-bukrs
                     ASSIGNING <l_fs_t001> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_bukrs-code     = <l_fs_t001>-bukrs.
      l_s_bukrs-name     = <l_fs_t001>-butxt.
** Country
      l_s_country-code   = <l_fs_t001>-land1.
      l_s_country-name   = <l_fs_t001>-landx50.
   ENDIF.

** Business Area
   READ TABLE i_tgsbt WITH KEY gsber = pu_s_aufk-gsber
                     ASSIGNING <l_fs_tgsbt> BINARY SEARCH.
   IF sy-subrc EQ 0.
      l_s_busarea-code     = <l_fs_tgsbt>-gsber.
      l_s_busarea-name     = <l_fs_tgsbt>-gtext.
   ENDIF.

** Cost Object Type
   l_s_co_type-code        = 'IO'.
   l_s_co_type-name        = text-t03. "Internal Order

** Cost Object ID
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
     EXPORTING
       input         =  pu_s_aufk-aufnr
    IMPORTING
       OUTPUT        =  l_s_cost_obj-code.

   l_s_cost_obj-name       = pu_s_aufk-ktext.


   PERFORM f_build_orgunit USING l_s_bukrs
                                 l_s_co_type
                                 l_s_cost_obj
                                 l_s_contarea
                                 l_s_busarea
                                 l_s_country
                           CHANGING PC_S_ORGUNIT1
                                    PC_S_ORGUNIT2
                                    PC_S_ORGUNIT3
                                    PC_S_ORGUNIT4
                                    PC_S_ORGUNIT5
                                    PC_S_ORGUNIT6.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_BUILD_ORGUNIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM F_BUILD_ORGUNIT USING pu_s_bukrs       TYPE ty_orgunit_item
                           pu_s_co_type     TYPE ty_orgunit_item
                           pu_s_cost_obj    TYPE ty_orgunit_item
                           pu_s_contarea    TYPE ty_orgunit_item
                           pu_s_busarea     TYPE ty_orgunit_item
                           pu_s_country     TYPE ty_orgunit_item
                     CHANGING PC_S_ORGUNIT1 TYPE ty_orgunit_item
                              PC_S_ORGUNIT2 TYPE ty_orgunit_item
                              PC_S_ORGUNIT3 TYPE ty_orgunit_item
                              PC_S_ORGUNIT4 TYPE ty_orgunit_item
                              PC_S_ORGUNIT5 TYPE ty_orgunit_item
                              PC_S_ORGUNIT6 TYPE ty_orgunit_item.

   CASE 'LOGICAL SYSTEM'.
     WHEN s_zcte_t001-org_unit1.
       pc_s_orgunit1-code = v_logical_system.
       pc_s_orgunit1-name = v_logsys_name.
     WHEN s_zcte_t001-org_unit2.
       pc_s_orgunit2-code = v_logical_system.
       pc_s_orgunit2-name = v_logsys_name.
     WHEN s_zcte_t001-org_unit3.
       pc_s_orgunit3-code = v_logical_system.
       pc_s_orgunit3-name = v_logsys_name.
     WHEN s_zcte_t001-org_unit4.
       pc_s_orgunit4-code = v_logical_system.
       pc_s_orgunit4-name = v_logsys_name.
     WHEN s_zcte_t001-org_unit5.
       pc_s_orgunit5-code = v_logical_system.
       pc_s_orgunit5-name = v_logsys_name.
     WHEN s_zcte_t001-org_unit6.
       pc_s_orgunit6-code = v_logical_system.
       pc_s_orgunit6-name = v_logsys_name.
   ENDCASE.


   CASE 'COMPANY CODE'.
     WHEN pc_s_orgunit1.
       pc_s_orgunit1-code = pu_s_bukrs-code.
       pc_s_orgunit1-name = pu_s_bukrs-name.
     WHEN s_zcte_t001-org_unit2.
       pc_s_orgunit2-code = pu_s_bukrs-code.
       pc_s_orgunit2-name = pu_s_bukrs-name.
     WHEN s_zcte_t001-org_unit3.
       pc_s_orgunit3-code = pu_s_bukrs-code.
       pc_s_orgunit3-name = pu_s_bukrs-name.
     WHEN s_zcte_t001-org_unit4.
       pc_s_orgunit4-code = pu_s_bukrs-code.
       pc_s_orgunit4-name = pu_s_bukrs-name.
     WHEN s_zcte_t001-org_unit5.
       pc_s_orgunit5-code = pu_s_bukrs-code.
       pc_s_orgunit5-name = pu_s_bukrs-name.
     WHEN s_zcte_t001-org_unit6.
       pc_s_orgunit6-code = pu_s_bukrs-code.
       pc_s_orgunit6-name = pu_s_bukrs-name.
   ENDCASE.

   CASE 'CONTROLLING AREA'.
     WHEN pc_s_orgunit1.
       pc_s_orgunit1-code = pu_s_contarea-code.
       pc_s_orgunit1-name = pu_s_contarea-name.
     WHEN s_zcte_t001-org_unit2.
       pc_s_orgunit2-code = pu_s_contarea-code.
       pc_s_orgunit2-name = pu_s_contarea-name.
     WHEN s_zcte_t001-org_unit3.
       pc_s_orgunit3-code = pu_s_contarea-code.
       pc_s_orgunit3-name = pu_s_contarea-name.
     WHEN s_zcte_t001-org_unit4.
       pc_s_orgunit4-code = pu_s_contarea-code.
       pc_s_orgunit4-name = pu_s_contarea-name.
     WHEN s_zcte_t001-org_unit5.
       pc_s_orgunit5-code = pu_s_contarea-code.
       pc_s_orgunit5-name = pu_s_contarea-name.
     WHEN s_zcte_t001-org_unit6.
       pc_s_orgunit6-code = pu_s_contarea-code.
       pc_s_orgunit6-name = pu_s_contarea-name.
   ENDCASE.


   CASE 'BUSINESS AREA'.
     WHEN pc_s_orgunit1.
       pc_s_orgunit1-code = pu_s_busarea-code.
       pc_s_orgunit1-name = pu_s_busarea-name.
     WHEN s_zcte_t001-org_unit2.
       pc_s_orgunit2-code = pu_s_busarea-code.
       pc_s_orgunit2-name = pu_s_busarea-name.
     WHEN s_zcte_t001-org_unit3.
       pc_s_orgunit3-code = pu_s_busarea-code.
       pc_s_orgunit3-name = pu_s_busarea-name.
     WHEN s_zcte_t001-org_unit4.
       pc_s_orgunit4-code = pu_s_busarea-code.
       pc_s_orgunit4-name = pu_s_busarea-name.
     WHEN s_zcte_t001-org_unit5.
       pc_s_orgunit5-code = pu_s_busarea-code.
       pc_s_orgunit5-name = pu_s_busarea-name.
     WHEN s_zcte_t001-org_unit6.
       pc_s_orgunit6-code = pu_s_busarea-code.
       pc_s_orgunit6-name = pu_s_busarea-name.
   ENDCASE.

   CASE 'COUNTRY'.
     WHEN pc_s_orgunit1.
       pc_s_orgunit1-code = pu_s_country-code.
       pc_s_orgunit1-name = pu_s_country-name.
     WHEN s_zcte_t001-org_unit2.
       pc_s_orgunit2-code = pu_s_country-code.
       pc_s_orgunit2-name = pu_s_country-name.
     WHEN s_zcte_t001-org_unit3.
       pc_s_orgunit3-code = pu_s_country-code.
       pc_s_orgunit3-name = pu_s_country-name.
     WHEN s_zcte_t001-org_unit4.
       pc_s_orgunit4-code = pu_s_country-code.
       pc_s_orgunit4-name = pu_s_country-name.
     WHEN s_zcte_t001-org_unit5.
       pc_s_orgunit5-code = pu_s_country-code.
       pc_s_orgunit5-name = pu_s_country-name.
     WHEN s_zcte_t001-org_unit6.
       pc_s_orgunit6-code = pu_s_country-code.
       pc_s_orgunit6-name = pu_s_country-name.
   ENDCASE.


   CASE 'COST OBJECT TYPE'.
     WHEN pc_s_orgunit1.
       pc_s_orgunit1-code = pu_s_co_type-code.
       pc_s_orgunit1-name = pu_s_co_type-name.
     WHEN s_zcte_t001-org_unit2.
       pc_s_orgunit2-code = pu_s_co_type-code.
       pc_s_orgunit2-name = pu_s_co_type-name.
     WHEN s_zcte_t001-org_unit3.
       pc_s_orgunit3-code = pu_s_co_type-code.
       pc_s_orgunit3-name = pu_s_co_type-name.
     WHEN s_zcte_t001-org_unit4.
       pc_s_orgunit4-code = pu_s_co_type-code.
       pc_s_orgunit4-name = pu_s_co_type-name.
     WHEN s_zcte_t001-org_unit5.
       pc_s_orgunit5-code = pu_s_co_type-code.
       pc_s_orgunit5-name = pu_s_co_type-name.
     WHEN s_zcte_t001-org_unit6.
       pc_s_orgunit6-code = pu_s_co_type-code.
       pc_s_orgunit6-name = pu_s_co_type-name.
   ENDCASE.

   CASE 'COST OBJECT'.
     WHEN pc_s_orgunit1.
       pc_s_orgunit1-code = pu_s_cost_obj-code.
       pc_s_orgunit1-name = pu_s_cost_obj-name.
     WHEN s_zcte_t001-org_unit2.
       pc_s_orgunit2-code = pu_s_cost_obj-code.
       pc_s_orgunit2-name = pu_s_cost_obj-name.
     WHEN s_zcte_t001-org_unit3.
       pc_s_orgunit3-code = pu_s_cost_obj-code.
       pc_s_orgunit3-name = pu_s_cost_obj-name.
     WHEN s_zcte_t001-org_unit4.
       pc_s_orgunit4-code = pu_s_cost_obj-code.
       pc_s_orgunit4-name = pu_s_cost_obj-name.
     WHEN s_zcte_t001-org_unit5.
       pc_s_orgunit5-code = pu_s_cost_obj-code.
       pc_s_orgunit5-name = pu_s_cost_obj-name.
     WHEN s_zcte_t001-org_unit6.
       pc_s_orgunit6-code = pu_s_cost_obj-code.
       pc_s_orgunit6-name = pu_s_cost_obj-name.
   ENDCASE.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_LOG_ENTRY
*&---------------------------------------------------------------------*
FORM F_ADD_LOG_ENTRY  USING  pu_s_orgunit1 TYPE ty_orgunit_item
                             pu_s_orgunit2 TYPE ty_orgunit_item
                             pu_s_orgunit3 TYPE ty_orgunit_item
                             pu_s_orgunit4 TYPE ty_orgunit_item
                             pu_s_orgunit5 TYPE ty_orgunit_item
                             pu_s_orgunit6 TYPE ty_orgunit_item.

  FIELD-SYMBOLS: <l_fs_log> TYPE zcte_fit006.

    APPEND INITIAL LINE TO i_log ASSIGNING <l_fs_log>.
    <l_fs_log>-entity_id = p_entity.
    <l_fs_log>-org_unit1 = pu_s_orgunit1-code.
    <l_fs_log>-org_unit2 = pu_s_orgunit2-code.
    <l_fs_log>-org_unit3 = pu_s_orgunit3-code.
    <l_fs_log>-org_unit4 = pu_s_orgunit4-code.
    <l_fs_log>-org_unit5 = pu_s_orgunit5-code.
    <l_fs_log>-org_unit6 = pu_s_orgunit6-code.


ENDFORM.                    " F_ADD_LOG_ENTRY
*&---------------------------------------------------------------------*
*&      Form  FILE_GEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_file_gen .

  DATA: l_v_line      TYPE char1024,
        l_v_item      TYPE bapi_msg.

  FIELD-SYMBOLS: <l_fs_field>        TYPE ANY,
                 <l_fs_orgs>         TYPE ty_structure_orgs,
                 <l_fs_line>         TYPE ty_file.

** Build file structure
  LOOP AT i_orgs  ASSIGNING <l_fs_orgs>.
    DO 16 TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE <l_fs_orgs> TO <l_fs_field>.
      CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
        EXPORTING
          intext            = <l_fs_field>
        IMPORTING
          outtext           = <l_fs_field>
        EXCEPTIONS
          invalid_codepage  = 1
          codepage_mismatch = 2
          internal_error    = 3
          cannot_convert    = 4
          fields_not_type_c = 5
          OTHERS            = 6.

      CONCATENATE l_v_line <l_fs_field> INTO l_v_line SEPARATED BY ','.

    ENDDO.
    IF NOT l_v_line IS INITIAL.
      l_v_line = l_v_line+1.
*      WRITE: / l_v_line.
      PERFORM f_log_add_message USING l_v_line.

      APPEND INITIAL LINE TO i_file ASSIGNING <l_fs_line>.
      <l_fs_line> = l_v_line.

      CLEAR l_v_line.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " FILE_GEN

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_CSV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_download_csv CHANGING p_v_erro.


  CONSTANTS: l_c_encoding TYPE abap_encoding VALUE '4110'.  " utf-8

  FIELD-SYMBOLS: <l_fs_line>         TYPE ty_file.

  DATA: l_v_file                TYPE zcte_inbound_dir,
        l_v_len                 TYPE i,
        l_v_mode                TYPE c.


  IF p_v_erro EQ abap_true.
    EXIT.
  ENDIF.

  IF i_file IS INITIAL.
    MESSAGE e025 INTO v_message. " No data found for process
    s_log-log_obj->capture_and_add_message( ).
    p_v_erro = abap_true.
    EXIT.
  ENDIF.

*  CHECK NOT p_folder IS INITIAL.
  IF  p_folder IS INITIAL AND p_online IS INITIAL.
    p_folder = ''.
  ELSEIF p_folder IS INITIAL AND p_online IS NOT INITIAL.
    p_folder = 'C:\CONCUR\'.
    MESSAGE i026 INTO v_message. " File generated into c:\Concur
    s_log-log_obj->capture_and_add_message( ).
  ENDIF.

  l_v_len = strlen( p_folder ).
  SUBTRACT 1 FROM l_v_len.
  IF p_folder+l_v_len NE '\'.
    CONCATENATE p_folder '\' INTO p_folder.
  ENDIF.


  CONCATENATE p_folder
              'list_'
              p_entity
              sy-datum
              sy-uzeit
              '.txt'
         INTO l_v_file.


  IF p_online EQ abap_true.
    l_v_mode = 'O'.
  ELSE.
    l_v_mode = 'B'.
  ENDIF.

  CALL METHOD zcte_cl_file_handle=>table_to_file_download
    EXPORTING
      im_v_mode       = l_v_mode
      im_v_filepath   = l_v_file
    CHANGING
      ch_i_filetable   = i_file
      ch_cl_log_handle = s_log-log_obj
    EXCEPTIONS
      internal_error   = 1
      OTHERS           = 2 .

  IF sy-subrc NE 0.
    p_v_erro = abap_true.
  ENDIF.




*  IF p_online IS INITIAL.
*
*    " Generate file on application server
*    OPEN DATASET l_v_file FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
*    IF sy-subrc EQ 0.
*
*      LOOP AT i_file ASSIGNING <l_fs_line>.
**        CONCATENATE <l_fs_line> cl_abap_char_utilities=>cr_lf INTO <l_fs_line>.
*        TRANSFER <l_fs_line> TO l_v_file .
*      ENDLOOP.
*
*      CLOSE DATASET l_v_file.
*
*      MESSAGE s012(zcte_base) WITH l_v_file INTO v_message. "File generated
*      WRITE: / v_message.
*    ELSE.
*      MESSAGE e022(zcte_base) WITH l_v_file INTO v_message. "File generation error
*      WRITE: / v_message.
*      p_v_erro = abap_true.
*    ENDIF.
*
*
*  ELSE.  "Generate file on presentation server
*
*
*    CALL FUNCTION 'GUI_DOWNLOAD'
*      EXPORTING
*        filename                = l_v_file
*        codepage                = l_c_encoding
*      TABLES
*        data_tab                = i_file
*      EXCEPTIONS
*        file_write_error        = 1
*        no_batch                = 2
*        gui_refuse_filetransfer = 3
*        invalid_type            = 4
*        no_authority            = 5
*        unknown_error           = 6
*        header_not_allowed      = 7
*        separator_not_allowed   = 8
*        filesize_not_allowed    = 9
*        header_too_long         = 10
*        dp_error_create         = 11
*        dp_error_send           = 12
*        dp_error_write          = 13
*        unknown_dp_error        = 14
*        access_denied           = 15
*        dp_out_of_memory        = 16
*        disk_full               = 17
*        dp_timeout              = 18
*        file_not_found          = 19
*        dataprovider_exception  = 20
*        control_flush_error     = 21
*        OTHERS                  = 22.
*
*    IF sy-subrc NE 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      p_v_erro = abap_true.
*    ELSE.
*      MESSAGE s012(zcte_base) WITH l_v_file INTO v_message. "File generated
*      WRITE: / v_message.
*    ENDIF.
*
*  ENDIF.

ENDFORM.                    " DOWNLOAD_CSV

*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_LOG_TABLE
*&---------------------------------------------------------------------*
FORM f_update_log_table CHANGING p_v_erro.


  IF p_v_erro EQ abap_true.
    EXIT.
  ENDIF.

  IF p_log EQ abap_true.

    IF NOT i_log IS INITIAL.
       MODIFY zcte_fit006 FROM TABLE i_log.
       IF sy-subrc NE 0.
         MESSAGE e028 INTO v_message.    "Error updating table zcte_fit006
         s_log-log_obj->capture_and_add_message( ).
         p_v_erro = abap_true.
       ENDIF.
    ELSE.
         MESSAGE e029 INTO v_message.    "Log table empty
         s_log-log_obj->capture_and_add_message( ).
    ENDIF.

  ENDIF.

  IF p_dele EQ abap_true.
    IF NOT i_del IS INITIAL.
       MODIFY zcte_fit006 FROM TABLE i_del.
       IF sy-subrc NE 0.
         MESSAGE e030 INTO v_message. "Error trying to delete from ZCTE_FIT006
         s_log-log_obj->capture_and_add_message( ).
         p_v_erro = abap_true.
       ENDIF.
    ENDIF.
  ENDIF.

  IF p_v_erro EQ abap_false.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.


ENDFORM.                    " F_UPDATE_LOG_TABLE

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
       i_zcte_t001 = go_zcte_utils->get_entity_list( ).
       SORT i_zcte_t001 BY entity_id.
    ENDIF.

  ENDIF.

  LOOP AT i_zcte_t001 ASSIGNING <l_fs_zcte_t001>.
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

  IF p_back EQ abap_true.

      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'P_FOLDER'.
             screen-input = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDLOOP.

    IF NOT p_entity IS INITIAL.
       CLEAR: p_folder.
       READ TABLE i_zcte_t001 ASSIGNING <l_fs_zcte_t001> WITH KEY entity_id = p_entity.
       IF sy-subrc EQ 0.
          IF NOT <l_fs_zcte_t001>-outbound_dir IS INITIAL.
            p_folder =  <l_fs_zcte_t001>-outbound_dir.
          ELSE.
            MESSAGE e018(zcte_base) WITH p_entity. " No outbound directory set
          ENDIF.
       ENDIF.
     ENDIF.

  ELSE.

    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'P_FOLDER'.
           screen-input = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.


ENDFORM.                    " F_SET_DIRECTORY
*&---------------------------------------------------------------------*
*&      Form  F_ADD_LIST_ITEM
*&---------------------------------------------------------------------*
FORM F_ADD_LIST_ITEM  USING pu_v_item
                            pu_s_orgunit TYPE ty_orgunit_item.


    DATA: l_v_pos               TYPE i.

    FIELD-SYMBOLS: <l_fs_field> TYPE any.

    s_orgs-listname      = p_list.
    s_orgs-listcategoria = p_list.

    l_v_pos = pu_v_item + 2.

    ASSIGN COMPONENT l_v_pos OF STRUCTURE s_orgs TO <l_fs_field>.
    IF sy-subrc EQ 0 AND <l_fs_field> IS ASSIGNED.
      <l_fs_field> = pu_s_orgunit-code.
      s_orgs-value = pu_s_orgunit-name.
    ENDIF.

    DO.
      ADD 1 TO l_v_pos.
      IF l_v_pos GT 12.
        EXIT.
      ENDIF.

      ASSIGN COMPONENT l_v_pos OF STRUCTURE s_orgs TO <l_fs_field>.
      IF sy-subrc EQ 0 AND <l_fs_field> IS ASSIGNED.
        CLEAR <l_fs_field>.
      ENDIF.
    ENDDO.

    APPEND s_orgs TO i_orgs.



ENDFORM.                    " F_ADD_LIST_ITEM
*&---------------------------------------------------------------------*
*&      Form  F_ADD_DEL_LIST_ITEM
*&---------------------------------------------------------------------*
FORM F_ADD_DEL_LIST_ITEM USING pu_s_del STRUCTURE zcte_fit006.

    CONSTANTS: l_c_dely         TYPE c VALUE 'Y'.

    s_orgs-listname      = p_list.
    s_orgs-listcategoria = p_list.
    s_orgs-level01       = pu_s_del-org_unit1.
    s_orgs-level01       = pu_s_del-org_unit1.
    s_orgs-level01       = pu_s_del-org_unit1.
    s_orgs-level01       = pu_s_del-org_unit1.
    s_orgs-level01       = pu_s_del-org_unit1.
    s_orgs-level01       = pu_s_del-org_unit1.
    s_orgs-del           = l_c_dely.

    APPEND s_orgs TO i_orgs.



ENDFORM.                    " F_ADD_LIST_ITEM
*&---------------------------------------------------------------------*
*&      Form  F_LOG_ADD_MESSAGE
*&---------------------------------------------------------------------*
FORM F_LOG_ADD_MESSAGE USING im_v_message TYPE char1024.

  DATA: l_v_message TYPE bapi_msg.

  l_v_message = im_v_message.

  CALL METHOD s_log-log_obj->add_free_text
    EXPORTING
      im_msgtyp  = 'S'
      im_message = l_v_message.



ENDFORM.                    " F_LOG_ADD_MESSAGE
