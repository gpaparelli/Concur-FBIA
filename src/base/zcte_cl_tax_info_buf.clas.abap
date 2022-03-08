class ZCTE_CL_TAX_INFO_BUF definition
  public
  final
  create public .

public section.
*"* public components of class ZCTE_CL_TAX_INFO_BUF
*"* do not include other source files here!!!

  class-data GO_TAX_INFO_BUF type ref to ZCTE_CL_TAX_INFO_BUF .

  class-methods GET_INSTANCE
    returning
      value(RE_TAX_INFO_BUF) type ref to ZCTE_CL_TAX_INFO_BUF .
  type-pools ABAP .
  methods CHECK_TAX_CODE_EXIST
    importing
      !IM_V_BUKRS type BUKRS
      !IM_V_MWSKZ type MWSKZ
      !IM_V_TXJCD type TXJCD optional
      !IM_V_WAERS type WAERS
      !IM_V_PRSDT type PRSDT optional
    returning
      value(RE_FOUND) type ABAP_BOOL .
  methods GET_TAX_DATA
    returning
      value(RE_I_TAX_INFO) type ZCTE_S010_T .
protected section.
*"* protected components of class ZCTE_CL_TAX_INFO_BUF
*"* do not include other source files here!!!
private section.
*"* private components of class ZCTE_CL_TAX_INFO_BUF
*"* do not include other source files here!!!

  data GT_TAX_INFO type ZCTE_S010_T .

  methods GET_DATA_FROM_TAX_CODE
    importing
      !IM_V_BUKRS type BUKRS
      !IM_V_MWSKZ type MWSKZ
      !IM_V_TXJCD type TXJCD optional
      !IM_V_WAERS type WAERS
      !IM_V_PRSDT type PRSDT optional
    returning
      value(RE_FOUND) type ABAP_BOOL .
ENDCLASS.



CLASS ZCTE_CL_TAX_INFO_BUF IMPLEMENTATION.


method CHECK_TAX_CODE_EXIST.

  READ TABLE gt_tax_info WITH KEY bukrs = im_v_bukrs
                                  mwskz = im_v_mwskz
                                  waers = im_v_waers
                                  txjcd = im_v_txjcd
                                  prsdt = im_v_prsdt
                         TRANSPORTING NO FIELDS.

  IF sy-subrc EQ 0.
    re_found = abap_true.
  ELSE.
*   Get information and if found save to buffer
    IF im_v_txjcd IS NOT INITIAL.
      re_found = me->get_data_from_tax_code( im_v_bukrs = im_v_bukrs
                                             im_v_mwskz = im_v_mwskz
                                             im_v_waers = im_v_waers
                                             im_v_txjcd = im_v_txjcd
                                             im_v_prsdt = im_v_prsdt ).
    ELSE.
      re_found = me->get_data_from_tax_code( im_v_bukrs = im_v_bukrs
                                             im_v_mwskz = im_v_mwskz
                                             im_v_waers = im_v_waers
                                             im_v_prsdt = im_v_prsdt ).
    ENDIF.

  ENDIF.


endmethod.


method GET_DATA_FROM_TAX_CODE.

  DATA ls_tax_info TYPE zcte_s010.
  DATA lt_mwdat	   TYPE zcte_s011_t.

  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      i_bukrs = im_v_bukrs
      i_mwskz = im_v_mwskz
      i_txjcd = im_v_txjcd
      i_waers = im_v_waers
      i_prsdt = im_v_prsdt
      i_wrbtr = '1.00'
    TABLES
      t_mwdat = lt_mwdat
    EXCEPTIONS
      OTHERS  = 1.      "#EC *

* Add data to buffer table
  IF lt_mwdat IS NOT INITIAL.
    ls_tax_info-bukrs = im_v_bukrs.
    ls_tax_info-mwskz = im_v_mwskz.
    ls_tax_info-waers = im_v_waers.
    ls_tax_info-txjcd = im_v_txjcd.
    ls_tax_info-prsdt = im_v_prsdt.
    ls_tax_info-mwdat = lt_mwdat.
    INSERT ls_tax_info INTO TABLE gt_tax_info.
    re_found = abap_true.
  ELSE.
    re_found = abap_false.
  ENDIF.



endmethod.


method GET_INSTANCE.

    IF NOT go_tax_info_buf IS BOUND.
      CREATE OBJECT re_tax_info_buf.
      go_tax_info_buf = re_tax_info_buf.
    ELSE.
      re_tax_info_buf = go_tax_info_buf.
    ENDIF.


endmethod.


method GET_TAX_DATA.

   re_i_tax_info = gt_tax_info.

endmethod.
ENDCLASS.
