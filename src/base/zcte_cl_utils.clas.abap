class ZCTE_CL_UTILS definition
  public
  final
  create public .

public section.
*"* public components of class ZCTE_CL_UTILS
*"* do not include other source files here!!!

  data GS_ZCTE_T001 type ZCTE_T001 .
  data V_AD_LOGSYS type CHAR6 .
  data V_AD_BUSAREA type CHAR6 .
  data V_AD_CONTAREA type CHAR6 .
  data V_AD_COUNTRY type CHAR6 .
  data V_AD_COST_OBJ_TP type CHAR6 .
  data V_AD_COST_OBJ type CHAR6 .
  data V_AD_COMPCODE type CHAR6 .
  data V_ER_LOGSYS type CHAR6 .
  data V_ER_BUSAREA type CHAR6 .
  data V_ER_CONTAREA type CHAR6 .
  data V_ER_COUNTRY type CHAR6 .
  data V_ER_COST_OBJ_TP type CHAR6 .
  data V_ER_COST_OBJ type CHAR6 .
  data V_ER_COMPCODE type CHAR6 .
  data V_IV_LOGSYS type CHAR6 .
  data V_IV_BUSAREA type CHAR6 .
  data V_IV_CONTAREA type CHAR6 .
  data V_IV_COUNTRY type CHAR6 .
  data V_IV_COST_OBJ_TP type CHAR6 .
  data V_IV_COST_OBJ type CHAR6 .
  data V_IV_COMPCODE type CHAR6 .
  data V_IV_TAX_AUTHORITY type CHAR6 .
  data V_IV_TAX_CODE type CHAR6 .
  data V_IV_TAX_JURISDICTION type CHAR6 .
  data V_IV_TAX_CURRENCY type CHAR6 .
  class-data C_LOGSYS type ZCTE_ORGUNIT value 'LOGICAL SYSTEM'. "#EC NOTEXT .
  class-data C_COUNTRY type ZCTE_ORGUNIT value 'COUNTRY'. "#EC NOTEXT .
  class-data C_CONTAREA type ZCTE_ORGUNIT value 'CONTROLLING AREA'. "#EC NOTEXT .
  class-data C_BUSAREA type ZCTE_ORGUNIT value 'BUSINESS AREA'. "#EC NOTEXT .
  class-data C_COMPCODE type ZCTE_ORGUNIT value 'COMPANY CODE'. "#EC NOTEXT .
  class-data C_COST_OBJ_TP type ZCTE_ORGUNIT value 'COST OBJECT TYPE'. "#EC NOTEXT .
  class-data C_COST_OBJ type ZCTE_ORGUNIT value 'COST OBJECT'. "#EC NOTEXT .
  class-data C_TAX_AUTHORITY type ZCTE_TAXFIELD value 01. "#EC NOTEXT .
  class-data C_TAX_CODE type ZCTE_TAXFIELD value 02. "#EC NOTEXT .
  class-data C_TAX_JURISDICTION type ZCTE_TAXFIELD value 03. "#EC NOTEXT .
  class-data C_TAX_CURRENCY type ZCTE_TAXFIELD value 04. "#EC NOTEXT .

  methods GET_ENTITY_LIST
    returning
      value(RE_I_ENTITY_LIST) type ZCTE_T001_T .
  methods CONSTRUCTOR .
  methods GET_ENTITY_SETTINGS
    returning
      value(RE_S_ZCTE_T001) type ZCTE_T001
    exceptions
      NOT_FOUND .
  methods SET_ENTITY
    importing
      !IM_V_ENTITY type ZCTE_ENTITY_ID
    exceptions
      NOT_FOUND .
  methods GET_FIELD_COL .
protected section.
*"* protected components of class ZCTE_CL_UTILS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCTE_CL_UTILS
*"* do not include other source files here!!!

  data GT_ZCTE_T001 type ZCTE_T001_T .
  data:
    GT_ZCTE_T003 type TABLE OF ZCTE_T003 .
  data:
    GT_ZCTE_T004 TYPE TABLE OF ZCTE_T004 .

  methods SELECT_ENTITY_LIST
    exceptions
      NOT_FOUND .
ENDCLASS.



CLASS ZCTE_CL_UTILS IMPLEMENTATION.


method CONSTRUCTOR.

    me->select_entity_list(
     EXCEPTIONS
       NOT_FOUND = 1
       OTHERS    = 2 ).

endmethod.


method GET_ENTITY_LIST.

   re_i_entity_list = gt_zcte_t001.

endmethod.


method GET_ENTITY_SETTINGS.

   re_s_zcte_t001 = gs_zcte_t001.

endmethod.


method GET_FIELD_COL.

*  IF NOT gs_zcte_t001-entity_id IS INITIAL.
*
*    READ TABLE gt_zcte_t003 WITH TABLE KEY entity_id = gs_zcte_t001-entity_id
*                                           org_unit  = im_v_orgunit.
*
*  ENDIF.
*
endmethod.


method SELECT_ENTITY_LIST.


   SELECT *
     FROM zcte_t001
     INTO TABLE gt_zcte_t001.

   IF sy-subrc EQ 0.
      SORT gt_zcte_t001 BY entity_id.
   ELSE.
      RAISE not_found.
   ENDIF.


   SELECT *
     FROM zcte_t003
     INTO TABLE gt_zcte_t003.

   IF sy-subrc EQ 0.
      SORT gt_zcte_t003 BY entity_id.
   ENDIF.

   SELECT *
     FROM zcte_t004
     INTO TABLE gt_zcte_t004.

   IF sy-subrc EQ 0.
      SORT gt_zcte_t004 BY entity_id.
   ENDIF.

endmethod.


method SET_ENTITY.

  CONSTANTS: lc_col TYPE char3 VALUE 'COL'.

  FIELD-SYMBOLS: <l_fs_zcte_t003> TYPE zcte_t003,
                 <l_fs_zcte_t004> TYPE zcte_t004.

  IF gs_zcte_t001-entity_id NE im_v_entity.

     READ TABLE gt_zcte_t001 INTO gs_zcte_t001 WITH KEY entity_id = im_v_entity BINARY SEARCH.
     IF sy-subrc NE 0.
        RAISE not_found.
     ENDIF.

     LOOP AT gt_zcte_t003 ASSIGNING <l_fs_zcte_t003> WHERE entity_id EQ im_v_entity.

        CASE <l_fs_zcte_t003>-org_unit.
          WHEN C_LOGSYS.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_exp     INTO v_er_logsys.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_cashadv INTO v_ad_logsys.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_inv     INTO v_iv_logsys.
          WHEN C_COUNTRY.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_exp     INTO v_er_country.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_cashadv INTO v_ad_country.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_inv     INTO v_iv_country.
          WHEN C_CONTAREA.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_exp     INTO v_er_contarea.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_cashadv INTO v_ad_contarea.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_inv     INTO v_iv_contarea.
          WHEN C_BUSAREA.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_exp     INTO v_er_busarea.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_cashadv INTO v_ad_busarea.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_inv     INTO v_iv_busarea.
          WHEN C_COMPCODE.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_exp     INTO v_er_compcode.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_cashadv INTO v_ad_compcode.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_inv     INTO v_iv_compcode.
          WHEN C_COST_OBJ_TP.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_exp     INTO v_er_cost_obj_tp.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_cashadv INTO v_ad_cost_obj_tp.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_inv     INTO v_iv_cost_obj_tp.
          WHEN C_COST_OBJ.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_exp     INTO v_er_cost_obj.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_cashadv INTO v_ad_cost_obj.
             CONCATENATE lc_col <l_fs_zcte_t003>-col_inv     INTO v_iv_cost_obj.
          WHEN OTHERS.
            "Implement logic for custom orgunit fields
        ENDCASE.
     ENDLOOP.

     LOOP AT gt_zcte_t004 ASSIGNING <l_fs_zcte_t004> WHERE entity_id EQ im_v_entity.

        CASE <l_fs_zcte_t004>-tax_field.
          WHEN C_TAX_AUTHORITY.
             CONCATENATE lc_col <l_fs_zcte_t004>-col_inv     INTO v_iv_tax_authority.
          WHEN C_TAX_CODE.
             CONCATENATE lc_col <l_fs_zcte_t004>-col_inv     INTO v_iv_tax_code.
          WHEN C_TAX_JURISDICTION.
             CONCATENATE lc_col <l_fs_zcte_t004>-col_inv     INTO v_iv_tax_jurisdiction.
          WHEN C_TAX_CURRENCY.
             CONCATENATE lc_col <l_fs_zcte_t004>-col_inv     INTO v_iv_tax_currency.
          WHEN OTHERS.
*            N/a
        ENDCASE.
      ENDLOOP.


   ENDIF.



endmethod.
ENDCLASS.
