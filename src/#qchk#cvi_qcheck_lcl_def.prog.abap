*&---------------------------------------------------------------------*
*&  Include  /qchk/cvi_check_lcl_def
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Class LCL_CVI_SERVICE
*&---------------------------------------------------------------------*
*        Service Class for CVI Checks
*----------------------------------------------------------------------*
CLASS lcl_cvi_service DEFINITION.

  PUBLIC SECTION.

    TYPES: tt_vari_contents TYPE STANDARD TABLE OF rsparams.

    CLASS-METHODS:

      generete_run_id            RETURNING VALUE(ev_runid)   TYPE /qchk/cvi_runid,

      get_job_date_time          IMPORTING iv_pdate TYPE dats OPTIONAL
                                           iv_ptime TYPE tims OPTIONAL
                                 EXPORTING ev_time  TYPE tbtcjob-sdlstrttm
                                           ev_date  TYPE tbtcjob-sdlstrtdt,

      create_submit_variant     IMPORTING iv_check_type  TYPE /qchk/cvi_check_type
                                          iv_variantname TYPE raldb_vari
                                          is_run_stat    TYPE /qchk/st_cvi_run,

      create_variant_content    IMPORTING iv_check_type    TYPE /qchk/cvi_check_type
                                          is_run_stat      TYPE /qchk/st_cvi_run
                                CHANGING  et_vari_contents TYPE  tt_vari_contents,

      get_job_name              IMPORTING iv_check_type TYPE /qchk/cvi_check_type OPTIONAL
                                EXPORTING ev_job_name   TYPE tbtcjob-jobname,

      create_job                IMPORTING iv_check_type TYPE /qchk/cvi_check_type,

      get_run_list              EXPORTING et_check_list TYPE  /qchk/tt_cvi_check_list,

      "-----------------------------------------------------------------------*
      get_job_status             CHANGING cs_log_results_hitlist TYPE /qchk/st_log_results_hitlist,

      get_check_descr            IMPORTING iv_jobname TYPE btcjob
                                 RETURNING VALUE(rv_run_desc) TYPE /qchk/cvi_run_desc,

      convert_date_time_external IMPORTING is_timestamp   TYPE timestamp
                                 EXPORTING ev_dateandtime TYPE val_text.

ENDCLASS.

*&---------------------------------------------------------------------*
*&       Class LCL_DISPLAY
*&---------------------------------------------------------------------*
*        Display Class for CVI Checks
*----------------------------------------------------------------------*
CLASS lcl_cvi_display DEFINITION.

  PUBLIC SECTION.

    METHODS constructor IMPORTING is_log_result TYPE /qchk/st_log_results_hitlist
                                  iv_title      TYPE string.

  PRIVATE SECTION.

    METHODS:

      fill_log_list         IMPORTING it_log_results         TYPE /qchk/tt_check_run
                            EXPORTING et_log_results_hitlist TYPE /qchk/tt_log_results_hitlist,

      get_log_data          RETURNING VALUE(et_log_res_usage) TYPE /qchk/tt_check_run,

      get_docs_check_result IMPORTING iv_runid             TYPE /qchk/cvi_runid
                            EXPORTING et_documents_hitlist TYPE /qchk/tt_documents_hitlist,

      get_docs_sel_param    IMPORTING is_log_result TYPE /qchk/st_log_results_hitlist
                            EXPORTING ev_arch       TYPE string
                                      ev_loevm      TYPE string,

      get_qual_check_result IMPORTING iv_runid             TYPE /qchk/cvi_runid
                            EXPORTING et_quality_check_res TYPE /qchk/tt_quality_check_hitlist,

      get_dupl_check_result IMPORTING iv_runid          TYPE /qchk/cvi_runid
                            EXPORTING et_dupl_check_res TYPE /qchk/tt_duplicate_res,

      get_dupl_sel_param    IMPORTING is_log_result TYPE /qchk/st_log_results_hitlist
                            EXPORTING ev_name       TYPE string
                                      ev_adr        TYPE string
                                      ev_ustid      TYPE string,

      modify_log_columns        IMPORTING ir_table TYPE REF TO cl_salv_table,

      modify_docs_result_table  IMPORTING iv_title      TYPE string
                                          iv_count      TYPE i
                                          ir_table      TYPE REF TO cl_salv_table
                                          is_log_result TYPE /qchk/st_log_results_hitlist,

      modify_dupl_result_table  IMPORTING iv_title      TYPE string
                                          iv_count      TYPE i
                                          ir_table      TYPE REF TO cl_salv_table
                                          is_log_result TYPE /qchk/st_log_results_hitlist,

      modify_qual_result_table  IMPORTING iv_title      TYPE string
                                          iv_count      TYPE i
                                          iv_count_err  TYPE i
                                          ir_table      TYPE REF TO cl_salv_table
                                          is_log_result TYPE /qchk/st_log_results_hitlist,

      set_log_event_handler     IMPORTING ir_log_table TYPE REF TO cl_salv_table,

      set_docs_event_handler    IMPORTING it_documents_hitlist TYPE /qchk/tt_documents_hitlist
                                          is_log_result        TYPE /qchk/st_log_results_hitlist
                                          ir_docs_res          TYPE REF TO cl_salv_table,

      set_dupl_event_handler    IMPORTING ir_qual_res      TYPE REF TO cl_salv_table
                                          is_log_result    TYPE /qchk/st_log_results_hitlist
                                          it_duplicate_res TYPE /qchk/tt_duplicate_res,

      set_qual_event_handler    IMPORTING ir_qual_res          TYPE REF TO cl_salv_table
                                          is_log_result        TYPE /qchk/st_log_results_hitlist
                                          it_quality_check_res TYPE /qchk/tt_quality_check_hitlist,

      show_docs_check_result   IMPORTING iv_title      TYPE string
                                         is_log_result TYPE /qchk/st_log_results_hitlist,

      show_qual_check_result   IMPORTING iv_title      TYPE string
                                         is_log_result TYPE /qchk/st_log_results_hitlist,

      show_dupl_check_result   IMPORTING iv_title      TYPE string
                                         is_log_result TYPE /qchk/st_log_results_hitlist,

      show_log_results.


ENDCLASS.

*&---------------------------------------------------------------------*
*&       Class LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*        Event handler Class for CVI Checks
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    "  CLASS-METHODS:
    "   on_delete_press.

    "  DATA: lo_salv_table                  TYPE STANDARD TABLE OF REF TO cl_salv_table.


    METHODS:

* for the next version
********************************************************************************************************************************************
      "on_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid IMPORTING sender,                            " event for editing a column.
      "on_toolbar       FOR EVENT toolbar      OF cl_gui_alv_grid IMPORTING e_object e_interactive sender,
      "on_alv_function FOR EVENT added_function OF cl_salv_events IMPORTING sender e_salv_function,
********************************************************************************************************************************************

      constructor             IMPORTING it_table TYPE ANY TABLE OPTIONAL,

      set_obj_type            IMPORTING iv_obj_type TYPE /qchk/cvi_objtype,
      get_obj_type            RETURNING VALUE(rv_obj_type) TYPE /qchk/cvi_objtype,

      auto_refresh            FOR EVENT finished OF cl_gui_timer,
      on_user_command         FOR EVENT added_function OF cl_salv_events_table IMPORTING e_salv_function,
      on_log_double_click     FOR EVENT double_click OF cl_salv_events_table IMPORTING row,
      on_job_link_click       FOR EVENT link_click OF cl_salv_events_table IMPORTING row,

      on_obj_docs_linkl_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_obj_qual_linkl_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_obj_dubl_linkl_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column.

    "on_link_click         FOR EVENT link_click OF cl_salv_events_table IMPORTING row ,
    "on_user_command       FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.

  PRIVATE SECTION.

    DATA: lv_obj_type TYPE /qchk/cvi_objtype,
          gr_data     TYPE REF TO data.

    CONSTANTS: gc_connected_bp TYPE salv_de_column VALUE 'CONNECTED_BP',
               gc_cv_link      TYPE salv_de_column VALUE 'CV_LINK'.

ENDCLASS.                    "lcl_event_handler DEFINITION
