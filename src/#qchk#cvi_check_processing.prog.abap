REPORT /qchk/cvi_check_processing.

INCLUDE /qchk/cvi_check_proc_lcl_def. " LCL Definition
INCLUDE /qchk/cvi_check_proc_top.     " Global Data
INCLUDE /qchk/cvi_check_proc_sel.     " Selection Screen
INCLUDE /qchk/cvi_check_proc_lcl_imp. " LCL Implementation


START-OF-SELECTION.

*  DATA lv_while.
*  WHILE lv_while IS INITIAL.
*  ENDWHILE.

  " Create check object
  lcl_cvi_service_proc=>def_master_data_type( IMPORTING ev_check_type = gv_check_type
                                                        ev_obj_source = gv_obj_source ).
  " Get select parameters
  lcl_cvi_service_proc=>def_sel_options( IMPORTING es_sel_param = gs_sel_param  ).

  " Create check object
  lcl_cvi_check_factory=>exec_create_check(
    EXPORTING
      it_sel_options = gs_sel_param-sel_criteria-sel_options
      iv_obj_source  = gv_obj_source
      iv_dupg        = gv_dupg
      iv_duppadr     = gv_duppadr
      iv_duppnam     = gv_duppnam
      iv_duppustid   = gv_duppustid
      iv_check_type  = gv_check_type
      iv_excarc      = gv_excarc
      iv_uuid        = gv_uuid
      iv_runid       = gv_runid
      iv_timestamp   = gv_timestamp
      iv_jobcount    = gv_jobcount
      iv_jobname     = gv_jobname
    RECEIVING
      er_check_object = gr_check ).

  "Start processing
  gr_check->process( ).
