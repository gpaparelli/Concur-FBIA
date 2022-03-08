*&---------------------------------------------------------------------*
*& Report  ZCTE_PMNT_CONFIRM
*&---------------------------------------------------------------------*
*&                  SAP BRAZIL
*&   DEVELOPER:  Gilliatti Paparelli  2020/04
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

REPORT ZCTE_PMNT_CONFIRM MESSAGE-ID zcte_fi.


TYPE-POOLS: vrm, abap.

TABLES: sscrfields.


TYPES: BEGIN OF ty_bsak,
        bukrs   TYPE bsak-bukrs,
        belnr   TYPE bsak-belnr,
        gjahr   TYPE bsak-gjahr,
        augbl   TYPE bsak-augbl,
        augdt   TYPE bsak-augdt,
        wrbtr   TYPE bsak-wrbtr,
        waers   TYPE bsak-waers,
       END OF ty_bsak,

       BEGIN OF ty_bkpf,
         bukrs        TYPE  bkpf-bukrs,
         belnr        TYPE  bkpf-belnr,
         gjahr        TYPE  bkpf-gjahr,
         xreversal    TYPE  bkpf-xreversal,
       END OF ty_bkpf,

       BEGIN OF ty_bseg,
          bukrs       TYPE  bseg-bukrs,
          belnr       TYPE  bseg-belnr,
          gjahr       TYPE  bseg-gjahr,
          buzei       TYPE  bseg-buzei,
          koart       TYPE  bseg-koart,
          bschl       TYPE  bseg-bschl,
          hkont       TYPE  bseg-hkont,
       END OF ty_bseg,

       BEGIN OF ty_skb1,
         bukrs        TYPE skb1-bukrs,
         saknr        TYPE skb1-saknr,
         hbkid        TYPE skb1-hbkid,
       END OF ty_skb1.

TYPES: BEGIN OF ty_file,
         line TYPE char1024,
       END OF ty_file.

TYPES: ty_t_zcte_fit003 TYPE STANDARD TABLE OF zcte_fit003,
       ty_t_bsak        TYPE STANDARD TABLE OF ty_bsak,
       ty_t_bkpf        TYPE STANDARD TABLE OF ty_bkpf,
       ty_t_bseg        TYPE STANDARD TABLE OF ty_bseg,
       ty_t_skb1        TYPE STANDARD TABLE OF ty_skb1,
       ty_t_file        TYPE STANDARD TABLE OF ty_file.

CONSTANTS: c_status_novo       TYPE zcte_fit003-pmnt_status VALUE 'N',
           c_status_estornado  TYPE zcte_fit003-pmnt_status VALUE 'T',
           c_status_compensado TYPE zcte_fit003-pmnt_status VALUE 'C',
           c_status_erro       TYPE zcte_fit003-pmnt_status VALUE 'E',
           c_status_nrelevante TYPE zcte_fit003-pmnt_status VALUE 'O'.


DATA: gt_zcte_t001 TYPE STANDARD TABLE OF zcte_t001.

DATA:  v_name           TYPE VRM_ID,
       v_message        TYPE BAPI_MSG,
       v_folder         TYPE string,
       v_log_handle     TYPE balloghndl,
       v_hkont          TYPE bsak-hkont,
       i_list           TYPE VRM_VALUES,
       wa_value         TYPE VRM_VALUE,
       o_logs           TYPE REF TO zcte_cl_log_handle,
       o_zcte_utils     TYPE REF TO zcte_cl_utils.

RANGES: r_bacc FOR v_hkont.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

  PARAMETERS: p_folder TYPE rlgrap-filename,
              p_online RADIOBUTTON GROUP g1 USER-COMMAND cmd DEFAULT 'X',
              p_back   RADIOBUTTON GROUP g1,
              p_log    AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME.
PARAMETERS: p_entity TYPE zcte_entity_id AS LISTBOX VISIBLE LENGTH 12 OBLIGATORY USER-COMMAND entity.
SELECTION-SCREEN END OF BLOCK B2.


INITIALIZATION.
  PERFORM f_get_entity_list.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_set_directory.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.
  IF p_online EQ 'X'.
    p_folder = zcte_cl_file_handle=>online_folder_selection( ).
  ELSE.
    p_folder = zcte_cl_file_handle=>background_folder_selection( ).
  ENDIF.

START-OF-SELECTION.

  PERFORM: f_bal_log_create.
  PERFORM: f_select_and_build_file.

END-OF-SELECTION.

  o_logs->save_log( ).
  IF p_log EQ abap_true.
     o_logs->display_log( ).
  ENDIF.

*&---------------------------------------------------------------------*
**&      Form  F_LOG_CREATE
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
FORM f_bal_log_create .

  DATA: l_v_extnumber TYPE  bal_s_log-extnumber.

  CONCATENATE 'PMTN' sy-datum sy-uzeit INTO l_v_extnumber.
*
  CREATE OBJECT o_logs
    EXPORTING
      im_object    = 'ZCTE'
      im_subobject = 'ZCTE02'
      im_extnumber = l_v_extnumber
    EXCEPTIONS
      not_created  = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "f_bal_log_create
*&---------------------------------------------------------------------*
*&      Form  F_SELECT_AND_BUILD_FILE
*&---------------------------------------------------------------------*
FORM f_select_and_build_file .

  DATA: l_i_zcte_fit003 TYPE ty_t_zcte_fit003,
        l_i_bsak        TYPE ty_t_bsak,
        l_i_bkpf        TYPE ty_t_bkpf,
        l_i_bseg        TYPE ty_t_bseg,
        l_i_skb1        TYPE ty_t_skb1.

  DATA: l_i_file        TYPE  ty_t_file.

  DATA: l_v_bukrs       TYPE bkpf-bukrs,
        l_v_belnr       TYPE bkpf-belnr,
        l_v_gjahr       TYPE bkpf-gjahr,
        l_v_erro        TYPE flag.

  FIELD-SYMBOLS: <l_fs_zcte_fit003> TYPE zcte_fit003.

  PERFORM f_get_acc_range CHANGING l_v_erro.

  IF l_v_erro IS INITIAL.

      PERFORM  f_select_documents TABLES l_i_zcte_fit003
                                         l_i_bsak
                                         l_i_bkpf
                                         l_i_bseg
                                         l_i_skb1
                                  USING  l_v_bukrs
                                         l_v_belnr
                                         l_v_gjahr.

      LOOP AT l_i_zcte_fit003 ASSIGNING <l_fs_zcte_fit003>.

         PERFORM: f_read_for_payment TABLES l_i_bsak
                                            l_i_bkpf
                                            l_i_bseg
                                            l_i_skb1
                                            l_i_file
                                     CHANGING  <l_fs_zcte_fit003>.

      ENDLOOP.

      IF NOT l_i_file IS INITIAL.
        PERFORM f_download_csv TABLES l_i_file CHANGING l_v_erro.
      ENDIF.

      IF l_v_erro IS INITIAL.
        PERFORM f_update_dtab TABLES l_i_zcte_fit003.
      ENDIF.

   ENDIF.

ENDFORM.                    " F_SELECT_AND_BUILD_FILE

*&---------------------------------------------------------------------*
*&      Form  F_SELECT_DOCUMENTS
*&---------------------------------------------------------------------*
FORM f_select_documents TABLES pt_zcte_fit003 TYPE ty_t_zcte_fit003
                               pt_bsak        TYPE ty_t_bsak
                               pt_bkpf        TYPE ty_t_bkpf
                               pt_bseg        TYPE ty_t_bseg
                               pt_skb1        TYPE ty_t_skb1
                        USING  pu_bukrs       TYPE bkpf-bukrs
                               pu_belnr       TYPE bkpf-belnr
                               pu_gjahr       TYPE bkpf-gjahr.


    DATA: l_i_bsak_sel TYPE ty_t_bsak,
          l_i_bkpf_sel TYPE ty_t_bkpf,
          l_i_skb1_sel TYPE ty_t_skb1.

    FIELD-SYMBOLS: <l_fs_bsak_sel>    TYPE ty_bsak,
                   <l_fs_bkpf_sel>    TYPE ty_bkpf,
                   <l_fs_skb1_sel>    TYPE ty_skb1,
                   <l_fs_bsak>        TYPE ty_bsak,
                   <l_fs_bseg>        TYPE ty_bseg,
                   <l_fs_zcte_fit003> TYPE zcte_fit003.


    IF pu_bukrs IS INITIAL AND
       pu_belnr IS INITIAL AND
       pu_gjahr IS INITIAL.

      SELECT *
        FROM zcte_fit003
        INTO TABLE pt_zcte_fit003
        WHERE   acc_belnr   NE space
          AND ( pmnt_status EQ 'N'
            OR pmnt_status EQ space ).

      IF sy-subrc EQ 0.

        LOOP AT pt_zcte_fit003 ASSIGNING <l_fs_zcte_fit003>.

           APPEND INITIAL LINE TO l_i_bsak_sel ASSIGNING <l_fs_bsak_sel>.
           <l_fs_bsak_sel>-bukrs = <l_fs_zcte_fit003>-acc_bukrs.
           <l_fs_bsak_sel>-belnr = <l_fs_zcte_fit003>-acc_belnr.
           <l_fs_bsak_sel>-gjahr = <l_fs_zcte_fit003>-acc_gjahr.

        ENDLOOP.
      ENDIF.

    ELSE.
      APPEND INITIAL LINE TO l_i_bsak_sel ASSIGNING <l_fs_bsak_sel>.
      <l_fs_bsak_sel>-bukrs = pu_bukrs.
      <l_fs_bsak_sel>-belnr = pu_belnr.
      <l_fs_bsak_sel>-gjahr = pu_gjahr.
    ENDIF.


    IF NOT l_i_bsak_sel[] IS INITIAL.
      SELECT bukrs belnr gjahr augbl augdt
             wrbtr waers
        FROM bsak
        INTO TABLE pt_bsak
        FOR ALL ENTRIES IN l_i_bsak_sel
        WHERE bukrs EQ l_i_bsak_sel-bukrs
          AND belnr EQ l_i_bsak_sel-belnr
          AND gjahr EQ l_i_bsak_sel-gjahr.

      IF sy-subrc EQ 0.

        SORT pt_bsak BY bukrs belnr gjahr.

         LOOP AT pt_bsak ASSIGNING <l_fs_bsak>.
            APPEND INITIAL LINE TO l_i_bkpf_sel ASSIGNING <l_fs_bkpf_sel>.
            <l_fs_bkpf_sel>-bukrs  = <l_fs_bsak>-bukrs.
            <l_fs_bkpf_sel>-belnr  = <l_fs_bsak>-augbl.
            <l_fs_bkpf_sel>-gjahr  = <l_fs_bsak>-augdt(4).
         ENDLOOP.

         SELECT  bukrs
                 belnr
                 gjahr
                 xreversal
           FROM bkpf
           INTO TABLE pt_bkpf
           FOR ALL ENTRIES IN l_i_bkpf_sel
           WHERE bukrs = l_i_bkpf_sel-bukrs
             AND belnr = l_i_bkpf_sel-belnr
             AND gjahr = l_i_bkpf_sel-gjahr.

         IF sy-subrc EQ 0.

           SORT pt_bkpf BY bukrs belnr gjahr.

           SELECT bukrs
                  belnr
                  gjahr
                  buzei
                  koart
                  bschl
                  hkont
             FROM bseg
             INTO TABLE pt_bseg
             FOR ALL ENTRIES IN pt_bkpf
             WHERE bukrs EQ pt_bkpf-bukrs
               AND belnr EQ pt_bkpf-belnr
               AND gjahr EQ pt_bkpf-gjahr.

           IF sy-subrc EQ 0.

              SORT pt_bseg BY bukrs belnr gjahr.

              LOOP AT pt_bseg ASSIGNING <l_fs_bseg>.
                  IF <l_fs_bseg>-koart NE 'S'.
                    CONTINUE.
                  ENDIF.

                  APPEND INITIAL LINE TO l_i_skb1_sel ASSIGNING <l_fs_skb1_sel>.
                  <l_fs_skb1_sel>-bukrs  = <l_fs_bseg>-bukrs.
                  <l_fs_skb1_sel>-saknr  = <l_fs_bseg>-hkont.
              ENDLOOP.

              IF NOT l_i_skb1_sel IS INITIAL.
                SORT l_i_skb1_sel.
                DELETE ADJACENT DUPLICATES FROM l_i_skb1_sel.

                SELECT bukrs saknr hbkid
                  FROM skb1
                  INTO TABLE pt_skb1
                  FOR ALL ENTRIES IN l_i_skb1_sel
                  WHERE bukrs = l_i_skb1_sel-bukrs
                    AND saknr = l_i_skb1_sel-saknr.
                IF sy-subrc EQ 0.

                   SORT pt_skb1 BY bukrs saknr.

                ENDIF.
              ENDIF.
           ENDIF.
         ENDIF.
      ENDIF.
   ENDIF.


ENDFORM.                    " F_SELECT_OPEN_REPORTS

*&---------------------------------------------------------------------*
*&      Form  F_READ_FOR_PAYMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_read_for_payment TABLES pt_bsak        TYPE ty_t_bsak
                               pt_bkpf        TYPE ty_t_bkpf
                               pt_bseg        TYPE ty_t_bseg
                               pt_skb1        TYPE ty_t_skb1
                               pt_file        TYPE ty_t_file
                        CHANGING pc_zcte_fit003 TYPE zcte_fit003.


  DATA:
        l_i_bsak        TYPE ty_t_bsak,
        l_i_bkpf        TYPE ty_t_bkpf,
        l_i_bseg        TYPE ty_t_bseg,
        l_i_skb1        TYPE ty_t_skb1.

  DATA: l_v_cleared     TYPE flag.

  FIELD-SYMBOLS: <l_fs_bsak>   TYPE ty_bsak,
                 <l_fs_bkpf>   TYPE ty_bkpf,
                 <l_fs_bseg>   TYPE ty_bseg,
                 <l_fs_skb1>   TYPE ty_skb1.

  READ TABLE pt_bsak ASSIGNING <l_fs_bsak> WITH KEY bukrs = pc_zcte_fit003-acc_bukrs
                                                    belnr = pc_zcte_fit003-acc_belnr
                                                    gjahr = pc_zcte_fit003-acc_gjahr
                                           BINARY SEARCH.
  IF sy-subrc NE 0.

    MESSAGE i021 WITH pc_zcte_fit003-acc_bukrs
                      pc_zcte_fit003-acc_belnr
                      pc_zcte_fit003-acc_gjahr
                 INTO v_message. " Log - Documento não está compensado
    o_logs->capture_and_add_message( ).

    pc_zcte_fit003-pmnt_status    = c_status_novo.

  ELSE.

     READ TABLE pt_bkpf ASSIGNING <l_fs_bkpf> WITH KEY bukrs = <l_fs_bsak>-bukrs
                                                       belnr = <l_fs_bsak>-augbl
                                                       gjahr = <l_fs_bsak>-augdt(4)
                                               BINARY SEARCH.
     IF sy-subrc EQ 0.

        IF NOT <l_fs_bkpf>-xreversal IS INITIAL.

           MESSAGE e022 WITH pc_zcte_fit003-acc_bukrs
                             pc_zcte_fit003-acc_belnr
                             pc_zcte_fit003-acc_gjahr
                        INTO v_message. " Log - Documento foi estornado
           o_logs->capture_and_add_message( ).

           pc_zcte_fit003-pmnt_status = c_status_estornado.            " atualizar status para Estorno
           EXIT.
        ENDIF.

        CLEAR l_v_cleared.
        LOOP AT pt_bseg ASSIGNING <l_fs_bseg> WHERE  bukrs = <l_fs_bkpf>-bukrs
                                                AND  belnr = <l_fs_bkpf>-belnr
                                                AND  gjahr = <l_fs_bkpf>-gjahr
                                                AND  koart = 'S'.

               IF <l_fs_bseg>-hkont IN r_bacc. "Found a bank account

                  MESSAGE s023 WITH pc_zcte_fit003-acc_bukrs
                                    pc_zcte_fit003-acc_belnr
                                    pc_zcte_fit003-acc_gjahr
                               INTO v_message. " Log - Documento foi compensado
                  o_logs->capture_and_add_message( ).

                  pc_zcte_fit003-pmnt_date    = <l_fs_bsak>-augdt.
                  pc_zcte_fit003-pmnt_amount  = <l_fs_bsak>-wrbtr.
                  pc_zcte_fit003-pmnt_curr    = <l_fs_bsak>-waers.
                  pc_zcte_fit003-pmnt_augbl   = <l_fs_bsak>-augbl.
                  pc_zcte_fit003-pmnt_status  = c_status_compensado.

                  IF pc_zcte_fit003-report_type EQ 'P'.  " Generate pmnt file only if not cash advance
                    PERFORM: f_generate_pmnt_confirm_line TABLES pt_file
                                                           USING pc_zcte_fit003.
                  ENDIF.
                    l_v_cleared = abap_true.
                  EXIT.
              ENDIF.
*           ENDIF.
        ENDLOOP.
        IF l_v_cleared EQ abap_false.

            MESSAGE e024 WITH pc_zcte_fit003-acc_bukrs
                              pc_zcte_fit003-acc_belnr
                              pc_zcte_fit003-acc_gjahr
                         INTO v_message. " Log - Não foi encontrada baixa contra banco
            o_logs->capture_and_add_message( ).

                      " atualizar status para Erro
           pc_zcte_fit003-pmnt_status = c_status_erro.

        ENDIF.
     ENDIF.
  ENDIF.


ENDFORM.                    " F_READ_FOR_PAYMENT

*&---------------------------------------------------------------------*
*&      Form  F_GENERATE_PMNT_CONFIRM_LINE
*&---------------------------------------------------------------------*
FORM f_generate_pmnt_confirm_line  TABLES pt_file        TYPE  ty_t_file
                                   USING  pu_zcte_fit003 TYPE  zcte_fit003.


  TYPES: BEGIN OF l_ty_pmnt_line,
           segment      TYPE char3,
           amount       TYPE char13,
           pay_date     TYPE char8,
           payee        TYPE char25,
           pay_tp       TYPE char1,
           pay_trans    TYPE char32,
           report_id    TYPE char32,
           pay_curr     TYPE char3,
           reserved1    TYPE char1,
           reserved2    TYPE char1,
           reserved3    TYPE char1,
           reserved4    TYPE char1,
           reserved5    TYPE char1,
         END OF l_ty_pmnt_line.

  DATA: l_s_pmnt_line TYPE l_ty_pmnt_line.
  DATA: l_s_file_line TYPE ty_file.

  FIELD-SYMBOLS: <l_fs_field> TYPE ANY.


** Insert header if this is the first line
  IF pt_file[] IS INITIAL.
    l_s_file_line = '100,LF,ID'.
    APPEND l_s_file_line TO pt_file.
    CLEAR l_s_file_line.
  ENDIF.

  l_s_pmnt_line-segment      = '600'.
  l_s_pmnt_line-amount       = pu_zcte_fit003-pmnt_amount.
  TRANSLATE l_s_pmnt_line-amount USING '. , '.
  CONDENSE l_s_pmnt_line-amount NO-GAPS.
  l_s_pmnt_line-pay_date     = pu_zcte_fit003-pmnt_date.
*  ls_pmnt_line-payee        = pu_bsak-
  l_s_pmnt_line-pay_tp       = 'E'. "Electronic fund transfer
  l_s_pmnt_line-pay_trans    = pu_zcte_fit003-pmnt_augbl.
  l_s_pmnt_line-report_id    = pu_zcte_fit003-report_id.
  l_s_pmnt_line-pay_curr     = pu_zcte_fit003-pmnt_curr.


  DO 13 TIMES.
     ASSIGN COMPONENT sy-index OF STRUCTURE l_s_pmnt_line TO <l_fs_field>.
     IF sy-subrc EQ 0.
        CONCATENATE l_s_file_line
                    <l_fs_field>
               INTO l_s_file_line
               SEPARATED BY ','.
     ENDIF.
  ENDDO.

  l_s_file_line = l_s_file_line+1.
  APPEND l_s_file_line TO pt_file.


ENDFORM.                    " F_GENERATE_PMNT_CONFIRM_LINE

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_CSV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_download_csv TABLES   pt_file
                    CHANGING p_v_erro.


  CONSTANTS: l_c_encoding TYPE abap_encoding VALUE '4110'.  " utf-8

  FIELD-SYMBOLS: <l_fs_line>         TYPE ty_file.

  DATA: l_v_file                TYPE zcte_backup_dir,
        l_v_len                 TYPE i,
        l_v_mode                TYPE c.

  IF p_v_erro EQ abap_true.
    EXIT.
  ENDIF.

  IF pt_file[] IS INITIAL.

    MESSAGE e025 INTO v_message.
    o_logs->capture_and_add_message( ).
    p_v_erro = abap_true.
    EXIT.
  ENDIF.

  IF  p_folder IS INITIAL AND p_online IS INITIAL.
    p_folder = ''.
  ELSEIF p_folder IS INITIAL AND p_online IS NOT INITIAL.
     p_folder = 'C:\CONCUR\'.

    MESSAGE s026 INTO v_message. "'Arquivo gerado em pasta local generica: (C:\CONCUR\)'.
    o_logs->capture_and_add_message( ).

  ENDIF.

  l_v_len = strlen( p_folder ).
  SUBTRACT 1 FROM l_v_len.
  IF p_folder+l_v_len NE '\'.
    CONCATENATE p_folder '\' INTO p_folder.
  ENDIF.


  CONCATENATE p_folder
              'exp_pay_confirm_'
              p_entity
              '_'
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
      ch_i_filetable   = pt_file
      ch_cl_log_handle = o_logs
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
*      LOOP AT pt_file ASSIGNING <l_fs_line>.
*        TRANSFER <l_fs_line> TO l_v_file .
*      ENDLOOP.
*
*      CLOSE DATASET l_v_file.
*
*      MESSAGE s012(zcte_base) WITH l_v_file INTO v_message. "File generated
*      o_logs->capture_and_add_message( ).
*
*    ELSE.
*
*      MESSAGE e022(zcte_base) WITH l_v_file INTO v_message. "File generation error
*      o_logs->capture_and_add_message( ).
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
*        data_tab                = pt_file
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
*       MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO v_message.
*
*       o_logs->capture_and_add_message( ).
*       p_v_erro = abap_true.
*    ELSE.
*       MESSAGE s012(zcte_base) WITH l_v_file INTO v_message. " file generated successfully
*       o_logs->capture_and_add_message( ).
*    ENDIF.

*  ENDIF.

ENDFORM.                    " DOWNLOAD_CSV
*&---------------------------------------------------------------------*
*&      Form  F_UPDATE_DTAB
*&---------------------------------------------------------------------*
FORM f_update_dtab TABLES pt_zcte_fit003 TYPE ty_t_zcte_fit003.


  DELETE pt_zcte_fit003 WHERE pmnt_status EQ 'N'.

  IF NOT pt_zcte_fit003[] IS INITIAL.
     MODIFY zcte_fit003 FROM TABLE pt_zcte_fit003.
     COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    " F_UPDATE_DTAB

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

  IF NOT o_zcte_utils IS BOUND.
    CREATE OBJECT o_zcte_utils.
    IF sy-subrc EQ 0.
       gt_zcte_t001 = o_zcte_utils->get_entity_list( ).
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
       READ TABLE gt_zcte_t001 ASSIGNING <l_fs_zcte_t001> WITH KEY entity_id = p_entity.
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
*&      Form  F_GET_ACC_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GET_ACC_RANGE CHANGING pc_v_erro.

  CONSTANTS: l_c_i  TYPE c     VALUE 'I',
             l_c_eq TYPE char2 VALUE 'EQ'.

  DATA: lt_zcte_fit010 TYPE STANDARD TABLE OF zcte_fit010.

  FIELD-SYMBOLS: <l_fs_zcte_fit010> TYPE zcte_fit010.

  CLEAR pc_v_erro.

  SELECT *
    FROM zcte_fit010
    INTO TABLE lt_zcte_fit010
    WHERE entity_id EQ p_entity.
  IF sy-subrc NE 0.
     MESSAGE e027 INTO v_message.
     o_logs->capture_and_add_message( ).
     pc_v_erro = abap_true.

  ELSE.

    r_bacc-sign   = l_c_i.
    r_bacc-option = l_c_eq.

    LOOP AT lt_zcte_fit010 ASSIGNING <l_fs_zcte_fit010>.
       r_bacc-low = <l_fs_zcte_fit010>-account_id.
       APPEND r_bacc.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " F_GET_ACC_RANGE
