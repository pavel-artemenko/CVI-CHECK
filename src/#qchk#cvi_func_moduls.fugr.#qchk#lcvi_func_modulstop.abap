FUNCTION-POOL /qchk/cvi_func_moduls.        "MESSAGE-ID ..

CLASS lcl_cvi_service_dupl DEFINITION.

  PUBLIC SECTION.

    DATA: gc_descr_string_address TYPE string VALUE 'ADDRESS',
          gc_descr_string_name    TYPE string VALUE 'NAME',
          gc_descr_string_ustid   TYPE string VALUE 'USTID'.

    METHODS:

      dupl_check_customers  IMPORTING iv_duppadr              TYPE xfeld
                                      iv_duppnam              TYPE xfeld
                                      iv_duppustid            TYPE xfeld
                                      iv_dupg                 TYPE i
                                      is_source_cust          TYPE cmds_ei_extern
                                      it_customers_compl_work TYPE cmds_ei_extern_t
                                      it_worked               TYPE /qchk/tt_cvi_obj_source_id
                                      ir_service_dupl         TYPE REF TO lcl_cvi_service_dupl,

      dupl_check_vendors IMPORTING iv_duppadr            TYPE xfeld
                                   iv_duppnam            TYPE xfeld
                                   iv_duppustid          TYPE xfeld
                                   iv_dupg               TYPE i
                                   is_source_vend        TYPE vmds_ei_extern
                                   it_vendors_compl_work TYPE vmds_ei_extern_t
                                   it_worked             TYPE /qchk/tt_cvi_obj_source_id
                                   ir_service_dupl         TYPE REF TO lcl_cvi_service_dupl,

      calculate_similarity_score   IMPORTING iv_search_str1     TYPE string
                                             iv_search_str2     TYPE string
                                             iv_dupg            TYPE i
                                   EXPORTING ev_lev_probability TYPE /qchk/cvi_probability
                                             ev_le_max          TYPE boolean ,

      save_score IMPORTING iv_src         TYPE char10
                           iv_trg         TYPE char10
                           iv_string_src  TYPE string
                           iv_string_trg  TYPE string
                           iv_probability TYPE /qchk/cvi_probability
                           iv_field_param TYPE char20 ,

      append_score IMPORTING is_dupl_check_result TYPE /qchk/tt_duplicate_res_mthrd OPTIONAL
                   EXPORTING ev_result            TYPE boolean ,

      build_check_string IMPORTING is_data          TYPE any
                                   iv_duppadr       TYPE xfeld
                                   iv_duppnam       TYPE xfeld
                                   iv_duppustid     TYPE xfeld
                         EXPORTING it_check_strings TYPE ANY TABLE,

      clear_score,

      get_results
        RETURNING
          VALUE(et_results) TYPE /qchk/tt_duplicate_res.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF gty_check_strings,
        descr           TYPE char20,
        string          TYPE string,
        original_string TYPE string,
      END OF gty_check_strings .

    TYPES:
      BEGIN OF gty_processing_group,
        group_id TYPE i,
        source   TYPE c LENGTH 10,
        target   TYPE c LENGTH 10,
      END OF gty_processing_group .

    DATA: gv_group_id                TYPE i,

          gs_dupl_check_result_sourc TYPE  /qchk/st_duplicate_res,
          gs_dupl_check_result_targ  TYPE  /qchk/st_duplicate_res,
          gs_current_group           TYPE  gty_processing_group,

          gt_dupl_plausi             TYPE TABLE OF /qchk/dupl_plaus,
          gt_dupl_check_result       TYPE  /qchk/tt_duplicate_res.

    METHODS:

      plausi_check IMPORTING is_check_data  TYPE /qchk/st_duplicate_res
                   EXPORTING ev_show_result TYPE boole_d ,

      create_group_id IMPORTING is_source          TYPE /qchk/st_duplicate_res
                                is_target          TYPE /qchk/st_duplicate_res
                      RETURNING VALUE(rv_group_id) TYPE i ,

      check_group_id_changed IMPORTING is_source        TYPE /qchk/st_duplicate_res
                                       is_target        TYPE /qchk/st_duplicate_res
                             RETURNING VALUE(rv_result) TYPE boolean.

ENDCLASS.
