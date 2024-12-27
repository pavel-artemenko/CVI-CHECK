*&---------------------------------------------------------------------*
*&  Include  /qchk/cvi_check_proc_lcl_def
*&---------------------------------------------------------------------*

**&---------------------------------------------------------------------*
**&       Class lcl_service_proc
**&---------------------------------------------------------------------*
CLASS lcl_cvi_service_proc DEFINITION.

  PUBLIC SECTION.

    DATA: gc_descr_string_address TYPE string VALUE 'ADDRESS',
          gc_descr_string_name    TYPE string VALUE 'NAME',
          gc_descr_string_ustid   TYPE string VALUE 'USTID'.

    CLASS-METHODS:

      def_sel_options       EXPORTING es_sel_param  TYPE mdst_sync_parameter,

      def_master_data_type  EXPORTING ev_check_type TYPE char32
                                      ev_obj_source TYPE mds_ctrl_obj_source.
*---------------------------------------------------------------------------
*    METHODS:
*
*      build_check_string IMPORTING is_data          TYPE any
*                                   iv_duppadr       TYPE xfeld
*                                   iv_duppnam       TYPE xfeld
*                                   iv_duppustid     TYPE xfeld
*                         EXPORTING it_check_strings TYPE ANY TABLE,
*
*
*      calculate_similarity_score   IMPORTING iv_search_str1     TYPE string
*                                             iv_search_str2     TYPE string
*                                             iv_dupg            TYPE i
*                                   EXPORTING ev_lev_probability TYPE /qchk/cvi_probability
*                                             ev_le_max          TYPE boolean ,
*
*      save_score IMPORTING iv_src         TYPE char10
*                           iv_trg         TYPE char10
*                           iv_string_src  TYPE string
*                           iv_string_trg  TYPE string
*                           iv_probability TYPE /qchk/cvi_probability
*                           iv_field_param TYPE char20 ,
*
*      append_score IMPORTING is_dupl_check_result TYPE /qchk/tt_duplicate_res_mthrd OPTIONAL
*                   EXPORTING ev_result            TYPE boolean ,
*
*      clear_score ,
*
*      get_results
*        RETURNING
*          VALUE(et_results) TYPE /qchk/tt_duplicate_res .

  PROTECTED SECTION.

  PRIVATE SECTION.
*
*    TYPES:
*      BEGIN OF gty_check_strings,
*        descr           TYPE char20,
*        string          TYPE string,
*        original_string TYPE string,
*      END OF gty_check_strings .
*
*    TYPES:
*      BEGIN OF gty_processing_group,
*        group_id TYPE i,
*        source   TYPE c LENGTH 10,
*        target   TYPE c LENGTH 10,
*      END OF gty_processing_group .
*
*    DATA: gv_group_id                TYPE i,
*
*          gs_dupl_check_result_sourc TYPE  /qchk/st_duplicate_res,
*          gs_dupl_check_result_targ  TYPE  /qchk/st_duplicate_res,
*          gs_current_group           TYPE  gty_processing_group,
*
*          gt_dupl_plausi             TYPE TABLE OF /qchk/dupl_plaus,
*          gt_dupl_check_result       TYPE  /qchk/tt_duplicate_res.
*
*    METHODS:
*
*      plausi_check IMPORTING is_check_data  TYPE /qchk/st_duplicate_res
*                   EXPORTING ev_show_result TYPE boole_d ,
*
*      create_group_id IMPORTING is_source          TYPE /qchk/st_duplicate_res
*                                is_target          TYPE /qchk/st_duplicate_res
*                      RETURNING VALUE(rv_group_id) TYPE i ,
*
*      check_group_id_changed IMPORTING is_source        TYPE /qchk/st_duplicate_res
*                                       is_target        TYPE /qchk/st_duplicate_res
*                             RETURNING VALUE(rv_result) TYPE boolean .

ENDCLASS.

**&---------------------------------------------------------------------*
**&       Class lcl_multi_thread
**&---------------------------------------------------------------------*
CLASS lcl_multi_thread DEFINITION.

  PUBLIC SECTION.
    TYPE-POOLS abap.

    CONSTANTS:
      c_default_group TYPE rzlli_apcl VALUE ' ',
      c_task          TYPE char6      VALUE 'PARALL' ##NO_TEXT.

    METHODS:
      all_threads_are_finished
        RETURNING VALUE(rv_empty) TYPE abap_bool,

      clear_thread
        IMPORTING i_task TYPE char8,

      constructor
        IMPORTING i_task_prefix TYPE char6      DEFAULT c_task
                  i_threads     TYPE i
                  i_group       TYPE rzlli_apcl DEFAULT c_default_group,

      handle_resource_failure,

      get_free_thread
        RETURNING VALUE(rv_thread) TYPE char8,

      initialize_dupl_work_tab IMPORTING it_work_tab     TYPE /qchk/tt_duplicate_res_mthrd,

      end_of_thread            IMPORTING p_task          TYPE clike,

      get_temp_dupl_results    EXPORTING et_temp_results TYPE /qchk/tt_duplicate_res_mthrd,

      clear_temp_results.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_thread,
        thread TYPE char8,
        used   TYPE abap_bool,
      END OF ty_thread.

    DATA:
      gv_task_prefix           TYPE char6,
      gt_threads_list          TYPE TABLE OF ty_thread WITH DEFAULT KEY,
      gv_threads               TYPE i,
      gv_count                 TYPE i,
      gv_used_threads          TYPE i,
      gv_group                 TYPE rzlli_apcl,

      gt_dupl_check_result_tmp TYPE /qchk/tt_duplicate_res_mthrd.

    METHODS
      get_free_threads
        RETURNING VALUE(rv_free_threads) TYPE i.

ENDCLASS.                    " lcl_multi_thread DEFINITION

*&---------------------------------------------------------------------*
*&       Class lcl_cvi_checks
*&---------------------------------------------------------------------*
CLASS lcl_cvi_checks DEFINITION ABSTRACT.

  PUBLIC SECTION.

    DATA: gv_obj_source    TYPE mds_ctrl_obj_source,
          gv_dupg          TYPE i,
          gv_objtype       TYPE /qchk/cvi_objtype,
          gv_type          TYPE string,
          gv_duppadr       TYPE xfeld,
          gv_duppnam       TYPE xfeld,
          gv_duppustid     TYPE xfeld,
          gv_excarc        TYPE xfeld,
          gv_excdel        TYPE xfeld,
          gv_uuid          TYPE uuid,
          gv_timestamp     TYPE timestamp,
          gv_jobcount      TYPE btcjobcnt,
          gv_jobname       TYPE btcjob,
          gv_save          TYPE int1,
          gv_runid         TYPE /qchk/cvi_runid,
          gr_ref_extractor TYPE REF TO if_mds_extractor,
          gs_sync_param    TYPE mdst_sync_parameter.

    METHODS:
      constructor IMPORTING iv_dupg        TYPE int1  OPTIONAL
                            iv_duppadr     TYPE xfeld OPTIONAL
                            iv_duppnam     TYPE xfeld OPTIONAL
                            iv_duppustid   TYPE xfeld OPTIONAL
                            iv_obj_source  TYPE mds_ctrl_obj_source OPTIONAL
                            iv_check_type  TYPE char32 OPTIONAL
                            iv_excarc      TYPE xfeld  OPTIONAL
                            iv_excdel      TYPE xfeld  OPTIONAL
                            iv_uuid        TYPE uuid   OPTIONAL
                            iv_type        TYPE string OPTIONAL
                            iv_timestamp   TYPE timestamp OPTIONAL
                            iv_jobcount    TYPE btcjobcnt OPTIONAL
                            iv_jobname     TYPE btcjob    OPTIONAL
                            iv_save        TYPE int1      OPTIONAL
                            iv_runid       TYPE /qchk/cvi_runid OPTIONAL
                            it_sel_options TYPE mdst_sync_sel_options_tab OPTIONAL,

      initialize_load_api   IMPORTING is_object_tuple TYPE mdst_sync_object
                            EXPORTING es_load_api     TYPE mdst_load_api,

      read_by_source_object IMPORTING !is_object_tuple TYPE   mdst_sync_object
                            EXPORTING !es_load_api     TYPE   mdst_load_api
                                      !es_error        TYPE   mds_ctrls_error,
      process.

  PROTECTED SECTION.

    METHODS:
      exec_check                  ABSTRACT IMPORTING is_data      TYPE REF TO data
                                                     it_customers TYPE cmds_ei_extern_t OPTIONAL
                                                     it_vendors   TYPE vmds_ei_extern_t OPTIONAL,

      exec_processing_completion  ABSTRACT,

      exec_initializing           ABSTRACT,

      exec_save                   ABSTRACT IMPORTING
                                             it_customers TYPE cmds_ei_extern_t   OPTIONAL
                                             it_vendors   TYPE vmds_ei_extern_t   OPTIONAL.

ENDCLASS.


*&---------------------------------------------------------------------*
*&       Class lcl_cvi_check_factory
*&---------------------------------------------------------------------*
CLASS lcl_cvi_check_factory DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      exec_create_check IMPORTING iv_dupg                TYPE int1 OPTIONAL
                                  iv_duppadr             TYPE xfeld OPTIONAL
                                  iv_duppnam             TYPE xfeld OPTIONAL
                                  iv_duppustid           TYPE xfeld OPTIONAL
                                  iv_type                TYPE string OPTIONAL
                                  iv_obj_source          TYPE mds_ctrl_obj_source OPTIONAL
                                  iv_check_type          TYPE char32 OPTIONAL
                                  iv_excarc              TYPE xfeld OPTIONAL
                                  iv_excdel              TYPE xfeld OPTIONAL
                                  iv_uuid                TYPE uuid OPTIONAL
                                  iv_runid               TYPE /qchk/cvi_runid OPTIONAL
                                  iv_timestamp           TYPE timestamp OPTIONAL
                                  iv_jobcount            TYPE btcjobcnt OPTIONAL
                                  iv_jobname             TYPE btcjob OPTIONAL
                                  iv_save                TYPE int1 OPTIONAL
                                  it_sel_options         TYPE mdst_sync_sel_options_tab OPTIONAL
                        RETURNING VALUE(er_check_object) TYPE REF TO lcl_cvi_checks.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


**&---------------------------------------------------------------------*
**&       Class lcl_documents_check
**&---------------------------------------------------------------------*
CLASS lcl_documents_check DEFINITION INHERITING FROM lcl_cvi_checks.

  PUBLIC SECTION.


  PROTECTED SECTION.
    METHODS:
      exec_check                 REDEFINITION,
      exec_initializing          REDEFINITION,
      exec_processing_completion REDEFINITION,
      exec_save                  REDEFINITION.

  PRIVATE SECTION.

    DATA: gt_hitlist TYPE /qchk/tt_documents_hitlist.

    METHODS:
      exec_customer_doc_checks IMPORTING
                                 iv_archiv_flag TYPE boolean
                                 iv_objtype     TYPE /qchk/cvi_objtype
                                 is_data        TYPE REF TO data
                               EXPORTING
                                 et_hitlist     TYPE /qchk/tt_documents_hitlist,

      exec_vendor_doc_checks IMPORTING
                               iv_archiv_flag TYPE boolean
                               iv_objtype     TYPE /qchk/cvi_objtype
                               is_data        TYPE REF TO data
                             EXPORTING
                               et_hitlist     TYPE /qchk/tt_documents_hitlist,

      get_all_fi_documents     IMPORTING
                                 iv_objtype   TYPE /qchk/cvi_objtype
                                 iv_cvnum     TYPE /qchk/cvi_cvnum
                               EXPORTING
                                 et_documents TYPE /qchk/tt_vbeln_check.

ENDCLASS.


**&---------------------------------------------------------------------*
**&       Class lcl_s4qual_check
**&---------------------------------------------------------------------*
CLASS lcl_s4qual_check DEFINITION INHERITING FROM lcl_cvi_checks.

  PUBLIC SECTION.

  PROTECTED SECTION.

    METHODS:
      exec_check                 REDEFINITION,
      exec_initializing          REDEFINITION,
      exec_processing_completion REDEFINITION,
      exec_save                  REDEFINITION.

  PRIVATE SECTION.


    CONSTANTS:
      gc_debitor            TYPE nrobj VALUE 'DEBITOR',
      gc_kreditor           TYPE nrobj VALUE 'KREDITOR',

      gc_msg_class_precheck TYPE string VALUE '/QCHK/CL_CVI_MESSAGE'.

    DATA:

      gt_qual_check_result TYPE /qchk/tt_quality_check_hitlist,

      gt_t005              TYPE TABLE OF t005,
      gt_tsad3             TYPE TABLE OF tsad3,
      gt_t077d             TYPE TABLE OF t077d,
      gt_t077k             TYPE TABLE OF t077k,
      gt_tzone             TYPE TABLE OF tzone,
      gt_nriv              TYPE TABLE OF nriv,
      gt_tfktaxnumtype     TYPE TABLE OF tfktaxnumtype,
      gv_istype            TYPE tb038-istype.

    METHODS:

      initialize_customizing_data,

      consistency_check        IMPORTING it_customers TYPE cmds_ei_extern_t
                                         it_vendors   TYPE vmds_ei_extern_t,

      check_address            IMPORTING is_address_data TYPE bapiad1vl
                               EXPORTING et_errors       TYPE bapiret2_t,

      check_email              IMPORTING it_smtp   TYPE cvis_ei_smtp_t
                               EXPORTING et_errors TYPE bapiret2_t,

      check_number_range       IMPORTING iv_nrobj         TYPE nrobj
                                         iv_objnr         TYPE char10
                                         iv_account_group TYPE char4
                               EXPORTING et_errors        TYPE bapiret2_t,

      check_bank_data          IMPORTING iv_nrobj       TYPE nrobj
                                         it_bankdetails TYPE cvis_ei_bankdetail_t
                               EXPORTING et_errors      TYPE bapiret2_t,

      check_tax_type           IMPORTING iv_nrobj                 TYPE nrobj
                                         it_eu_vat_nums           TYPE cvis_ei_vat_t
                                         is_customer_central_data TYPE cmds_ei_vmd_central_data OPTIONAL
                                         is_vendor_central_data   TYPE vmds_ei_vmd_central_data OPTIONAL
                                         is_address_data          TYPE bapiad1vl
                               EXPORTING et_errors                TYPE bapiret2_t,

      check_tax_jur            IMPORTING iv_nrobj        TYPE nrobj
                                         is_address_data TYPE bapiad1vl
                               EXPORTING et_errors       TYPE bapiret2_t,

      check_postcode           IMPORTING is_address_data TYPE bapiad1vl
                               EXPORTING et_errors       TYPE bapiret2_t,

      check_transzone          IMPORTING iv_nrobj        TYPE nrobj
                                         is_address_data TYPE bapiad1vl
                                         iv_ktokd        TYPE ktokd OPTIONAL
                                         iv_ktokk        TYPE ktokk OPTIONAL
                               EXPORTING et_errors       TYPE bapiret2_t,

      check_industry           IMPORTING iv_brsch  TYPE brsch
                               EXPORTING et_errors TYPE bapiret2_t,


      fill_check_result_bp_from_ret2 IMPORTING iv_objnr   TYPE char10
                                               iv_bp      TYPE /qchk/cvi_obj_source_id OPTIONAL
                                               iv_partner TYPE bu_partner
                                               it_errors  TYPE bapiret2_t,

      check_r1_367_map_vendor_to_bp      IMPORTING is_vendor TYPE vmds_ei_extern
                                         EXPORTING et_errors TYPE bapiret2_t
                                                   ev_bp     TYPE /qchk/cvi_obj_source_id,

      check_r1_367_map_custom_to_bp      IMPORTING is_customer TYPE cmds_ei_extern
                                         EXPORTING et_errors   TYPE bapiret2_t
                                                   ev_bp       TYPE /qchk/cvi_obj_source_id,

      get_partner_customer                  IMPORTING iv_kunnr   TYPE kunnr
                                            EXPORTING ev_partner TYPE bu_partner,

      get_partner_vendor        IMPORTING iv_lifnr   TYPE lifnr
                                EXPORTING ev_partner TYPE bu_partner.

ENDCLASS.


**&---------------------------------------------------------------------*
**&       Class lcl_duplicate_check
**&---------------------------------------------------------------------*
CLASS lcl_duplicate_check DEFINITION INHERITING FROM lcl_cvi_checks.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS:
      exec_check                 REDEFINITION,
      exec_initializing          REDEFINITION,
      exec_processing_completion REDEFINITION,
      exec_save                  REDEFINITION.

  PRIVATE SECTION.

    TYPES: BEGIN OF gty_object_worktab,
             object_nr TYPE char10,
             processed TYPE boolean,
           END OF gty_object_worktab.

    TYPES: BEGIN OF gty_processing_group,
             group_id TYPE i,
             source   TYPE c LENGTH 10,
             target   TYPE c LENGTH 10,
           END OF gty_processing_group.

    TYPES: BEGIN OF gty_worked,
             source_id TYPE char10,
           END OF gty_worked.

    DATA: gv_thread_count            TYPE i,
          gv_group_id                TYPE i,

          gs_check_result_source     TYPE REF TO /qchk/st_duplicate_res,
          gs_check_result_target     TYPE REF TO /qchk/st_duplicate_res,
          gs_current_group           TYPE gty_processing_group,

          gt_worked                  TYPE TABLE OF gty_worked,
          gt_dupl_check_result       TYPE /qchk/tt_duplicate_res,
          gt_dupl_check_result_mthrd TYPE /qchk/tt_duplicate_res_mthrd,
          gt_dupl_plaus              TYPE TABLE OF /qchk/dupl_plaus,
          gt_tshirt_sizes            TYPE TABLE OF /qchk/tshirt_siz,
          gt_vendors_compl           TYPE vmds_ei_extern_t,
          gt_vendors_compl_work      TYPE vmds_ei_extern_t,
          gt_vendors_compl_work_cl   TYPE vmds_ei_extern_t,
          gt_customers_compl         TYPE cmds_ei_extern_t,
          gt_customers_compl_work    TYPE cmds_ei_extern_t,
          gt_customers_compl_work_cl TYPE cmds_ei_extern_t,

          gr_multi_thread            TYPE REF TO lcl_multi_thread.

    METHODS:
      duplicate_check           IMPORTING it_customers TYPE cmds_ei_extern_t
                                          it_vendors   TYPE vmds_ei_extern_t,

      build_check_string        IMPORTING is_data          TYPE any
                                EXPORTING it_check_strings TYPE ANY TABLE,

      change_work_status        IMPORTING iv_objnr TYPE char10,

      initialize_id_complete,

      plausi_check              IMPORTING is_check_data  TYPE REF TO /qchk/st_duplicate_res
                                EXPORTING ev_show_result TYPE boole_d,
      append_score,


      calculate_similarity_score IMPORTING iv_search_str1     TYPE string
                                           iv_search_str2     TYPE string
                                 EXPORTING ev_lev_probability TYPE /qchk/cvi_probability
                                           ev_le_max          TYPE boolean,
      save_score                IMPORTING iv_src         TYPE char10
                                          iv_trg         TYPE char10
                                          iv_string_src  TYPE string
                                          iv_string_trg  TYPE string
                                          iv_probability TYPE /qchk/cvi_probability
                                          iv_field_param TYPE char20,
      clear_score,

      create_group_id           IMPORTING is_source          TYPE REF TO /qchk/st_duplicate_res
                                          is_target          TYPE REF TO /qchk/st_duplicate_res
                                RETURNING VALUE(rv_group_id) TYPE i,

      check_group_id_changed    IMPORTING is_source        TYPE REF TO /qchk/st_duplicate_res
                                          is_target        TYPE REF TO /qchk/st_duplicate_res
                                RETURNING VALUE(rv_result) TYPE boolean,

      collect_duplicates       .



ENDCLASS.                    "lcl_duplicate_check DEFINITION
