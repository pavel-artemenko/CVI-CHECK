FUNCTION /qchk/cvi_duplicate_check.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(IS_SOURCE_CUST) TYPE  CMDS_EI_EXTERN OPTIONAL
*"     VALUE(IS_SOURCE_VEND) TYPE  VMDS_EI_EXTERN OPTIONAL
*"     VALUE(IT_WORK_TAB_CUST) TYPE  CMDS_EI_EXTERN_T OPTIONAL
*"     VALUE(IT_WORK_TAB_VEND) TYPE  VMDS_EI_EXTERN_T OPTIONAL
*"     VALUE(IV_THREAD_ID) TYPE  CHAR18
*"     VALUE(IT_CHECK_RESULT_DUPL) TYPE  /QCHK/TT_DUPLICATE_RES
*"       OPTIONAL
*"     VALUE(IV_DUPPADR) TYPE  XFELD
*"     VALUE(IV_DUPPNAM) TYPE  XFELD
*"     VALUE(IV_DUPPUSTID) TYPE  XFELD
*"     VALUE(IV_DUPG) TYPE  I
*"     VALUE(IV_JOBNAME) TYPE  BTCJOB
*"  EXPORTING
*"     VALUE(ET_CHECK_RESULT_DUPL) TYPE  /QCHK/TT_DUPLICATE_RES_MTHRD
*"----------------------------------------------------------------------
  " Types
  TYPES: BEGIN OF gty_check_strings,
           descr           TYPE char20,
           string          TYPE string,
           original_string TYPE string,
         END OF gty_check_strings.

  DATA: lv_search_str1             TYPE string,
        lv_search_str2             TYPE string,
        lv_lev_probability         TYPE /qchk/cvi_probability,
        lv_le_max                  TYPE boolean,
        lv_append_score            TYPE boolean,
        lv_yes                     TYPE boolean,
        lv_is_duplicate            TYPE boolean,
        lv_group_length            TYPE i,
        lv_last_group              TYPE i,

        ls_result_to_return        TYPE /qchk/st_duplicate_res_mthrd,

        lt_check_strings_main      TYPE TABLE OF gty_check_strings,
        lt_check_strings_opp       TYPE TABLE OF gty_check_strings,
        lt_results                 TYPE /qchk/tt_duplicate_res,

        gt_customers_compl_work    TYPE cmds_ei_extern_t,
        gt_vendors_compl_work      TYPE vmds_ei_extern_t,
        gt_dupl_check_result_mthrd TYPE /qchk/tt_duplicate_res_mthrd,

        gt_worked                  TYPE /qchk/tt_cvi_obj_source_id,

        lr_service_dupl            TYPE REF TO lcl_cvi_service_dupl.

  FIELD-SYMBOLS: <ls_cust_data_base>    TYPE cmds_ei_extern,
                 <ls_check_string_main> LIKE LINE OF lt_check_strings_main,
                 <ls_check_string_opp>  LIKE LINE OF lt_check_strings_opp,
                 <ls_result>            TYPE /qchk/st_duplicate_res,
                 <ls_result_return>     TYPE /qchk/st_duplicate_res_mthrd.

  " Create service instance
  lr_service_dupl = NEW #( ).

  IMPORT tab_worked = gt_worked[]
         FROM DATABASE /qchk/indx_table(zc) ID iv_thread_id.

  " Object type definition and Daten-Cluster
  IF is_source_cust IS NOT INITIAL.
    IMPORT tab_work = gt_customers_compl_work[] FROM DATABASE /qchk/indx_table(zc) ID iv_jobname.
  ELSE.
    IMPORT tab_work = gt_vendors_compl_work[] FROM DATABASE /qchk/indx_table(zc) ID iv_jobname.
  ENDIF.

  DELETE FROM DATABASE /qchk/indx_table(zc) ID iv_thread_id.

  IF lines( gt_customers_compl_work ) = 0
    AND lines( gt_vendors_compl_work ) = 0.
    RETURN.
    "
  ENDIF.

  SORT gt_worked ASCENDING.

  "" Сustomers duplicate check
  IF gt_customers_compl_work IS NOT INITIAL.

    lr_service_dupl->dupl_check_customers( iv_duppnam              = iv_duppnam
                                           iv_duppadr              = iv_duppadr
                                           iv_duppustid            = iv_duppustid
                                           iv_dupg                 = iv_dupg
                                           is_source_cust          = is_source_cust
                                           it_customers_compl_work = gt_customers_compl_work
                                           it_worked               = gt_worked
                                           ir_service_dupl         = lr_service_dupl  ).
  ENDIF.

  "" Vendors duplicate check
  IF gt_vendors_compl_work IS NOT INITIAL.

    lr_service_dupl->dupl_check_vendors( iv_duppnam              = iv_duppnam
                                         iv_duppadr              = iv_duppadr
                                         iv_duppustid            = iv_duppustid
                                         iv_dupg                 = iv_dupg
                                         is_source_vend          = is_source_vend
                                         it_vendors_compl_work   = gt_vendors_compl_work
                                         it_worked               = gt_worked
                                         ir_service_dupl         = lr_service_dupl ).
  ENDIF.

* Get results
  lt_results = lr_service_dupl->get_results( ).

  IF lines( lt_results ) > 0.

    SORT lt_results BY group_id.

    CLEAR lv_last_group.

    " Determine length of groups
    LOOP AT lt_results ASSIGNING <ls_result>.
      IF lv_last_group IS INITIAL.
        lv_last_group = <ls_result>-group_id.
      ENDIF.

      IF lv_last_group <> <ls_result>-group_id.
        LOOP AT et_check_result_dupl ASSIGNING <ls_result_return> WHERE group_id = lv_last_group.
          <ls_result_return>-group_length = lv_group_length.
        ENDLOOP.
        lv_last_group = <ls_result>-group_id.
        CLEAR lv_group_length.
      ENDIF.

      ls_result_to_return = <ls_result>.
      ls_result_to_return-thread_id = iv_thread_id.
      APPEND ls_result_to_return TO et_check_result_dupl.
      lv_group_length = lv_group_length + 1.

      AT LAST.
        LOOP AT et_check_result_dupl ASSIGNING <ls_result_return> WHERE group_id = lv_last_group.
          <ls_result_return>-group_length = lv_group_length.
        ENDLOOP.
        lv_last_group = <ls_result>-group_id.
      ENDAT.

    ENDLOOP.

  ENDIF.


ENDFUNCTION.
