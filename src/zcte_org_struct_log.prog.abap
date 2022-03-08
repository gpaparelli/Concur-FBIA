*&---------------------------------------------------------------------*
*& Report  ZCTE_ORG_STRUCT_LOG
*&---------------------------------------------------------------------*
*&                  SAP BRAZIL
*&   DEVELOPER:  Gilliatti Paparelli  2020/06
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZCTE_ORG_STRUCT_LOG LINE-SIZE 100 MESSAGE-ID zcte_fi.

*-----------------------------------------------------------------------
* Includes
*-----------------------------------------------------------------------

TYPE-POOLS: SLIS.

*-----------------------------------------------------------------------
* Tables
*-----------------------------------------------------------------------

TABLES: zcte_fit006.
*-----------------------------------------------------------------------
* Internal Tables
*-----------------------------------------------------------------------
DATA: i_zcte_t001   TYPE STANDARD TABLE OF zcte_t001,
      i_zcte_t002   TYPE STANDARD TABLE OF zcte_t002.
*-----------------------------------------------------------------------
* Class
*-----------------------------------------------------------------------
DATA: go_zcte_utils TYPE REF TO zcte_cl_utils.
*-----------------------------------------------------------------------
* Selscreen
*-----------------------------------------------------------------------

  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

      SELECT-OPTIONS: so_org1    FOR zcte_fit006-org_unit1,
                      so_org2    FOR zcte_fit006-org_unit2,
                      so_org3    FOR zcte_fit006-org_unit3,
                      so_org4    FOR zcte_fit006-org_unit4,
                      so_org5    FOR zcte_fit006-org_unit5,
                      so_org6    FOR zcte_fit006-org_unit6.

  SELECTION-SCREEN END OF BLOCK B1.

  SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
      PARAMETERS: p_entity TYPE zcte_entity_id AS LISTBOX VISIBLE LENGTH 12 OBLIGATORY USER-COMMAND entity.
  SELECTION-SCREEN END OF BLOCK B2.

  INITIALIZATION.
    PERFORM f_get_entity_list.


  START-OF-SELECTION.

     PERFORM f_process_report.

*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_REPORT
*&---------------------------------------------------------------------*
FORM f_process_report .

  DATA: l_i_zcte_fit006 TYPE STANDARD TABLE OF zcte_fit006.

    PERFORM: f_select_data TABLES l_i_zcte_fit006,
             f_display_alv TABLES l_i_zcte_fit006.

ENDFORM.                    " F_PROCESS_REPORT

*&---------------------------------------------------------------------*
*&      Form  F_SELECT_DATA
*&---------------------------------------------------------------------*
FORM f_select_data  TABLES pt_zcte_fit006 STRUCTURE zcte_fit006.

    SELECT *
      FROM zcte_t002
      INTO TABLE i_zcte_t002.
    IF sy-subrc EQ 0.
      SORT i_zcte_t002 BY org_unit.
    ENDIF.


    SELECT *
      FROM zcte_fit006
      INTO TABLE pt_zcte_fit006
      WHERE entity_id EQ p_entity
        AND org_unit1 IN so_org1
        AND org_unit2 IN so_org2
        AND org_unit3 IN so_org3
        AND org_unit4 IN so_org4
        AND org_unit5 IN so_org5
        AND org_unit6 IN so_org6.


ENDFORM.                    " F_SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM f_display_alv TABLES pt_zcte_fit006 STRUCTURE zcte_fit006.

  DATA: lt_fieldcat TYPE SLIS_T_FIELDCAT_ALV.

  FIELD-SYMBOLS: <l_fs_fieldcat>  TYPE SLIS_FIELDCAT_ALV,
                 <l_fs_zcte_t001> TYPE zcte_t001,
                 <l_fs_zcte_t002> TYPE zcte_t002,
                 <l_fs_field>     TYPE any.

   CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME               = sy-repid
      I_STRUCTURE_NAME             = 'ZCTE_FIT006'
      I_CLIENT_NEVER_DISPLAY       = 'X'
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
*     IF <l_fs_fieldcat>-fieldname EQ 'MANDT'.
*         <l_fs_fieldcat>-no_out = abap_true.
*      ENDIF.

      READ TABLE i_zcte_t001 WITH KEY entity_id = p_entity ASSIGNING <l_fs_zcte_t001>.
      IF sy-subrc EQ 0.
         ASSIGN COMPONENT <l_fs_fieldcat>-fieldname OF STRUCTURE <l_fs_zcte_t001> TO <l_fs_field>.
         IF sy-subrc EQ 0.
            IF NOT <l_fs_field> IS INITIAL.
              READ TABLE i_zcte_t002 WITH KEY org_unit = <l_fs_field> ASSIGNING <l_fs_zcte_t002> BINARY SEARCH.
              IF sy-subrc EQ 0.
                <l_fs_fieldcat>-seltext_s    = <l_fs_zcte_t002>-display_name.
                <l_fs_fieldcat>-seltext_m    = <l_fs_zcte_t002>-display_name.
                <l_fs_fieldcat>-seltext_l    = <l_fs_zcte_t002>-display_name.
                <l_fs_fieldcat>-reptext_ddic = <l_fs_zcte_t002>-display_name.
                <l_fs_fieldcat>-outputlen = '25'.
              ENDIF.
           ELSE.
             <l_fs_fieldcat>-no_out = abap_true.
           ENDIF.
        ENDIF.
      ENDIF.


      CASE <l_fs_fieldcat>-fieldname.
        WHEN 'DEL'.
           <l_fs_fieldcat>-seltext_s = 'Del. Ind.'.
           <l_fs_fieldcat>-seltext_m = 'Del. Ind.'.
           <l_fs_fieldcat>-seltext_l = 'Deletion Indicator'.
           <l_fs_fieldcat>-outputlen = '10'.
           <l_fs_fieldcat>-checkbox  = abap_true.
      ENDCASE.
   ENDLOOP.


   CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      IT_FIELDCAT                       = lt_fieldcat
     TABLES
       t_outtab                         = pt_zcte_fit006
    EXCEPTIONS
      PROGRAM_ERROR                     = 1
      OTHERS                            = 2.

   IF sy-subrc <> 0.
       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
       EXIT.
   ENDIF.



ENDFORM.                    " F_DISPLAY_ALV

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
