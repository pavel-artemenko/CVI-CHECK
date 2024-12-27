*&---------------------------------------------------------------------*
*&  Include  /qchk/zcvi_checks_lcl_imp
*&---------------------------------------------------------------------*
**&---------------------------------------------------------------------*
**&       Class lcl_cvi_service
**&---------------------------------------------------------------------*
CLASS lcl_cvi_service IMPLEMENTATION.

  " -----------------------------------------------------------------------
  " Method get_job_name
  " -----------------------------------------------------------------------
  METHOD get_job_name.
    CASE iv_check_type.

      WHEN gc_check_docs.
        ev_job_name = gc_job_name_docs.
      WHEN gc_check_dupl.
        ev_job_name = gc_job_name_dupl.
      WHEN gc_check_qual.
        ev_job_name = gc_job_name_qual.
      WHEN OTHERS.
        ev_job_name = 'CHECK'.
    ENDCASE.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_job_date_time
  " -----------------------------------------------------------------------
  METHOD get_job_date_time.

    IF iv_pdate IS INITIAL AND iv_ptime IS INITIAL.
      ev_date = sy-datum.
      ev_time = sy-timlo.
    ELSE.
      " set Time and Date from parameters
    ENDIF.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method generete_run_id
  " -----------------------------------------------------------------------
  METHOD generete_run_id.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = '/QCHK/CVI'
      IMPORTING
        number                  = ev_runid
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc <> 0.
      MESSAGE i049(/qchk/cl_cvi_message) DISPLAY LIKE 'E'. " Fehler bei Lauf-ID-Erzeugung
      RETURN.
    ENDIF.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method create_variant_content
  " -----------------------------------------------------------------------
  METHOD create_variant_content.

    DATA: ls_vari_contents TYPE rsparams,
          ls_lifnr         LIKE LINE OF so_lifnr,
          ls_kunnr         LIKE LINE OF so_kunnr.

    " Select options Vendor
    IF p_kunst IS INITIAL.
      IF lines( so_lifnr[] ) IS INITIAL.
        CLEAR ls_vari_contents.
        ls_vari_contents-selname = 'SO_LIFNR'.
        ls_vari_contents-kind    = 'S'.
        APPEND ls_vari_contents TO et_vari_contents.
      ELSE.
        LOOP AT so_lifnr INTO ls_lifnr.
          CLEAR ls_vari_contents.
          ls_vari_contents-selname = 'SO_LIFNR'.
          ls_vari_contents-kind    = 'S'.
          ls_vari_contents-sign    = ls_lifnr-sign.
          ls_vari_contents-option  = ls_lifnr-option.
          ls_vari_contents-low     = ls_lifnr-low.
          ls_vari_contents-high    = ls_lifnr-high.
          APPEND ls_vari_contents TO et_vari_contents.
        ENDLOOP.
      ENDIF.
    ELSE.
      " Select options Customer
      IF lines( so_kunnr[] ) IS INITIAL.
        CLEAR ls_vari_contents.
        ls_vari_contents-selname = 'SO_KUNNR'.
        ls_vari_contents-kind    = 'S'.
        APPEND ls_vari_contents TO et_vari_contents.
      ELSE.
        LOOP AT so_kunnr INTO ls_kunnr.
          CLEAR ls_vari_contents.
          ls_vari_contents-selname = 'SO_KUNNR'.
          ls_vari_contents-kind    = 'S'.
          ls_vari_contents-sign    = ls_kunnr-sign.
          ls_vari_contents-option  = ls_kunnr-option.
          ls_vari_contents-low     = ls_kunnr-low.
          ls_vari_contents-high    = ls_kunnr-high.
          APPEND ls_vari_contents TO et_vari_contents.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " Select options run parameters
    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'P_LIFNST'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = p_lifnst.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'P_KUNST'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = p_kunst.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'C_LOEVM'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = c_loevm.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'C_BELEG'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = c_beleg.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'C_USE'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = c_use.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'P_CHTYPE'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = iv_check_type.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'P_UUID'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = is_run_stat-job_guid.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'P_TIMEST'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = is_run_stat-timestamp.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'P_JOBCOU'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = is_run_stat-jobcount.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'P_RUN_ID'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = is_run_stat-run_id.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'P_JOBNAM'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = is_run_stat-jobname.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'C_DUPST'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = c_dupst.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'C_DUPADR'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = c_dupadr.
    APPEND ls_vari_contents TO et_vari_contents.

    CLEAR ls_vari_contents.
    ls_vari_contents-selname = 'C_DUPNAM'.
    ls_vari_contents-kind    = 'P'.
    ls_vari_contents-sign    = 'I'.
    ls_vari_contents-option  = 'EQ'.
    ls_vari_contents-low     = c_dupnam.
    APPEND ls_vari_contents TO et_vari_contents.


  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method create_submit_variant
  " -----------------------------------------------------------------------
  METHOD create_submit_variant.
    DATA: ls_vari_desc     TYPE varid,
          ls_vari_text     TYPE varit,

          lt_vari_text     TYPE STANDARD TABLE OF varit,
          lt_vari_contents TYPE STANDARD TABLE OF rsparams.

    create_variant_content( EXPORTING iv_check_type    = iv_check_type
                                      is_run_stat      = is_run_stat
                            CHANGING  et_vari_contents = lt_vari_contents  ).

    ls_vari_text-report  = gc_report_name.
    ls_vari_desc-report  = gc_report_name.
    ls_vari_text-vtext   = iv_variantname.
    ls_vari_text-variant = iv_variantname.
    ls_vari_desc-variant = iv_variantname.
    APPEND ls_vari_text TO lt_vari_text.

    CALL FUNCTION 'RS_CREATE_VARIANT'
      EXPORTING
        curr_report               = gc_report_name
        curr_variant              = iv_variantname
        vari_desc                 = ls_vari_desc
      TABLES
        vari_contents             = lt_vari_contents
        vari_text                 = lt_vari_text
      EXCEPTIONS
        illegal_report_or_variant = 1
        illegal_variantname       = 2
        not_authorized            = 3
        not_executed              = 4
        report_not_existent       = 5
        report_not_supplied       = 6
        variant_exists            = 7
        variant_locked            = 8
        OTHERS                    = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.
  " -------------------------------------------------------------------------

  " -----------------------------------------------------------------------
  " Method create_job
  " -----------------------------------------------------------------------
  METHOD create_job.
    " ----------------------------------------------------------------------
    " Method contains COMMIT WORK!!
    " ----------------------------------------------------------------------

    DATA: lv_time        TYPE tbtcjob-sdlstrttm,
          lv_date        TYPE tbtcjob-sdlstrtdt,
          lv_jobcount    TYPE tbtcjob-jobcount,
          lv_jobname     TYPE tbtcjob-jobname,
          lv_timestamp   TYPE timestamp,
          lv_tstmp_str   TYPE string,
          lv_check_desc  TYPE /qchk/cvi_run_desc,
          lv_process     TYPE boolean,
          lv_cvi_objtype TYPE /qchk/cvi_objtype,
          lv_uuid        TYPE sysuuid_x16,
          lv_variantname TYPE raldb_vari,
          lv_exit        TYPE char1,
          lv_ucomm       TYPE sy-ucomm,
          lv_checkd      TYPE boolean,
          lv_checku      TYPE boolean,
          lv_checkq      TYPE boolean,
          lv_down        TYPE boolean,

          ls_run_stat    TYPE /qchk/st_cvi_run,

          lr_classdescr  TYPE REF TO cl_abap_classdescr,
          lr_descr_ref   TYPE REF TO cl_abap_typedescr.

    " CALL FUNCTION 'DEQUEUE_ALL'.

    " define object type
    IF p_kunst IS NOT INITIAL.
      lv_cvi_objtype = gc_objtype_c.
    ELSE.
      lv_cvi_objtype = gc_objtype_v.
    ENDIF.

    " Get Run ID
    ls_run_stat-run_id     = generete_run_id( ).

    "
    get_job_date_time( IMPORTING ev_time = lv_time
                                 ev_date = lv_date ).
    "
    CONVERT DATE lv_date TIME lv_time INTO TIME STAMP ls_run_stat-timestamp TIME ZONE sy-zonlo.

    "
    get_job_name( EXPORTING iv_check_type = iv_check_type
                  IMPORTING ev_job_name   = lv_jobname ).

    " job description
    lv_check_desc = get_check_descr( lv_jobname ).

    " add job type and timestamp
    "lv_jobname = lv_jobname && |_| && lv_cvi_objtype && |_| && ls_run_stat-timestamp.

    "
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = lv_jobname
*       sdlstrtdt        = lv_date
*       sdlstrttm        = lv_time
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      " Notification for user Raise exception
    ENDIF.

    " Get UUID
    TRY.
        lv_uuid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error INTO DATA(e_text).
        MESSAGE e_text->get_text( ) TYPE 'I'.
    ENDTRY.

    IF lv_uuid IS INITIAL.
      MESSAGE s999(zcvi_check) WITH 'UUID Generierungsfehler'(059) '!' '!' '!' DISPLAY LIKE 'E'.
      CLEAR gv_ok_code.
      RETURN.
    ENDIF.

    "
    CLEAR: lv_variantname.

    " Set variant name
    CONCATENATE lv_jobcount
                lv_jobname
                INTO     lv_variantname.

    " Set run details
    ls_run_stat-mandt       = sy-mandt.
    ls_run_stat-job_guid    = lv_uuid.
    ls_run_stat-object_type = lv_cvi_objtype.
    ls_run_stat-jobcount    = lv_jobcount.
    ls_run_stat-jobname     = lv_jobname. "iv_check_type.
    ls_run_stat-created_by  = sy-uname.

    " Create submit variant
    create_submit_variant( iv_check_type  = iv_check_type
                           iv_variantname = lv_variantname
                           is_run_stat    = ls_run_stat  ).

    " Submit
    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
        authcknam               = sy-uname
        jobcount                = lv_jobcount
        jobname                 = lv_jobname
        report                  = gc_report_name
        variant                 = lv_variantname
      EXCEPTIONS
        bad_priparams           = 1
        bad_xpgflags            = 2
        invalid_jobdata         = 3
        jobname_missing         = 4
        job_notex               = 5
        job_submit_failed       = 6
        lock_failed             = 7
        program_missing         = 8
        prog_abap_and_extpg_set = 9
        OTHERS                  = 10.
    IF sy-subrc <> 0.
*          EXIT.
    ELSE.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount          = lv_jobcount
          jobname           = lv_jobname
          strtimmed         = abap_true " IMMEDIATE
*         sdlstrtdt         = lv_date
*         sdlstrttm         = lv_time
*         direct_start      = abap_true
        EXCEPTIONS
          invalid_startdate = 1
          jobname_missing   = 2
          job_close_failed  = 3
          job_nosteps       = 4
          job_notex         = 5
          lock_failed       = 6
          OTHERS            = 7.
      IF sy-subrc > 0.
        " notification for user
      ELSE.
        MESSAGE s888(sabapdocu) WITH 'Job' lv_check_desc 'erfolgreich eingeplant'(014).
      ENDIF.
    ENDIF.

    WAIT UP TO 1 SECONDS.
    " Save run id
    INSERT /qchk/check_run FROM ls_run_stat.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_job_status
  " -----------------------------------------------------------------------
  METHOD get_job_status.

    DATA: lv_status TYPE tbtcjob-status,
          lv_icon   TYPE iconname.


    CALL FUNCTION 'BP_JOB_STATUS_GET'
      EXPORTING
        jobcount                   = cs_log_results_hitlist-jobcount
        jobname                    = cs_log_results_hitlist-jobname
        read_only_status           = 'X'
      IMPORTING
        status                     = lv_status
      EXCEPTIONS
        job_doesnt_exist           = 1
        unknown_error              = 2
        parent_child_inconsistency = 3
        OTHERS                     = 4.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.

      CASE lv_status.
        WHEN 'P'. " preliminary
          CONCATENATE '@EB\Q' 'Geplant'(053) '@' INTO lv_icon. " for tooltip
          cs_log_results_hitlist-status_icon = lv_icon. " ICON_LIGHT_OUT
          " cs_log_results_hitlist-status_icon = '@EB\QGeplant@'."ICON_LIGHT_OUT
        WHEN 'S'. " scheduled
          CONCATENATE '@EB\Q' 'Freigegeben'(054) '@' INTO lv_icon. " for tooltip
          cs_log_results_hitlist-status_icon = lv_icon. " ICON_LIGHT_OUT
*         cs_res_out_temp-status = '@EB\QFreigegeben@'."ICON_LIGHT_OUT
        WHEN 'Y'. " ready
          CONCATENATE '@EB\Q' 'Bereit'(055) '@' INTO lv_icon. " for tooltip
          cs_log_results_hitlist-status_icon = lv_icon. " ICON_LIGHT_OUT
*         cs_res_out_temp-status = '@EB\QBereit@'."ICON_LIGHT_OUT
        WHEN 'R'. " running
          CONCATENATE '@09\Q' 'in Bearbeitung'(056) '@' INTO lv_icon. " for tooltip
          cs_log_results_hitlist-status_icon = lv_icon. " ICON_LIGHT_YELLOW
*         cs_res_out_temp-status = '@09\Qin Bearbeitung@'."ICON_LIGHT_YELLOW
        WHEN 'F'. " finished
          CONCATENATE '@08\Q' 'Erfolgreich beendet'(057)'@' INTO lv_icon. " for tooltip
          cs_log_results_hitlist-status_icon = lv_icon. " ICON_GREEN_LIGHT
*         cs_res_out_temp-status = '@08\QErfolgreich beendet@'."ICON_GREEN_LIGHT
        WHEN 'A'. " aborted
          CONCATENATE '@0A\Q' 'mit Fehlern beendet'(058) '@' INTO lv_icon. " for tooltip
          cs_log_results_hitlist-status_icon = lv_icon. " ICON_LIGHT_RED
*         cs_res_out_temp-status = '@0A\Qmit Fehlern beendet@'."ICON_LIGHT_RED
        WHEN 'Z'. " suspended
          CONCATENATE '@EB\Q' 'Pausiert'(059) '@' INTO lv_icon. " for tooltip
          cs_log_results_hitlist-status_icon = lv_icon. " ICON_LIGHT_OUT
*         cs_res_out_temp-status = '@EB\QPausiert@'."ICON_LIGHT_OUT
        WHEN OTHERS.
          CONCATENATE '@PM\Q' 'Unbekannter Zustand'(060) '@' INTO lv_icon. " for tooltip
          cs_log_results_hitlist-status_icon = lv_icon. " ICON_PAW_ITEM
*         cs_res_out_temp-status = '@PM\QUnbekannter Zustand@'."ICON_PAW_ITEM
      ENDCASE.

    ENDIF.

  ENDMETHOD.


  " -----------------------------------------------------------------------
  " Method convert_date_time_external
  " -----------------------------------------------------------------------
  METHOD convert_date_time_external.

    DATA: lv_date     TYPE sy-datum,
          lv_time     TYPE sy-uzeit,
          lv_date_out TYPE string,
          lv_time_out TYPE string.

    CONVERT TIME STAMP is_timestamp TIME ZONE sy-zonlo INTO DATE lv_date TIME lv_time.

    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal            = lv_date
      IMPORTING
        date_external            = lv_date_out
      EXCEPTIONS
        date_internal_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

    TRY.
        cl_abap_timefm=>conv_time_int_to_ext( EXPORTING time_int = lv_time    " Type T für internes Zeitformat
*                                                        without_seconds = ABAP_FALSE    " Flag für Sekunden
*                                                        format_according_to = ENVIRONMENT    " Format gemäß ENVIRONMENT oder ISO
                                              IMPORTING time_ext = lv_time_out ).  " Externe Zeitdarstellung

      CATCH cx_parameter_invalid_range ##NO_HANDLER. " Parameter mit ungültigem Wertebereich
    ENDTRY.

    CONCATENATE lv_date_out lv_time_out INTO ev_dateandtime SEPARATED BY space.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_check_descr
  " -----------------------------------------------------------------------
  METHOD get_check_descr.

*    DATA lv_jobtype_short  TYPE char4.
*    iv_jobname_short = iv_jobname.

    IF iv_jobname = gc_job_name_docs.
      rv_run_desc = 'Verwendungsnachweis'(009).
    ELSEIF iv_jobname = gc_job_name_dupl.
      rv_run_desc = 'Dubletten-Prüfung'(008).
    ELSEIF iv_jobname = gc_job_name_qual.
      rv_run_desc = 'S/4HANA Pre Checks'(007).
    ENDIF.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_run_list
  " -----------------------------------------------------------------------
  METHOD get_run_list.

    DATA: ls_check_list TYPE /qchk/st_cvi_check_list.

    CLEAR et_check_list.

    IF c_use IS NOT INITIAL.
      CLEAR ls_check_list.
      ls_check_list-check_type = gc_check_docs.
      ls_check_list-check_selected = abap_true.
      APPEND ls_check_list TO et_check_list.
    ENDIF.

    IF c_s4 IS NOT INITIAL.
      CLEAR ls_check_list.
      ls_check_list-check_type = gc_check_qual.
      ls_check_list-check_selected = abap_true.
      APPEND ls_check_list TO et_check_list.
    ENDIF.

    IF c_dubl IS NOT INITIAL.
      CLEAR ls_check_list.
      ls_check_list-check_type = gc_check_dupl.
      ls_check_list-check_selected = abap_true.
      APPEND ls_check_list TO et_check_list.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


**&---------------------------------------------------------------------*
**&       CLASS lcl_display Implementation
**&---------------------------------------------------------------------*
CLASS lcl_cvi_display IMPLEMENTATION.

  " -----------------------------------------------------------------------
  " Method show_log_results
  " -----------------------------------------------------------------------
  METHOD constructor.
    IF is_log_result-run_id IS NOT INITIAL.

      DATA lv_jobname_short  TYPE char4.

      lv_jobname_short = is_log_result-jobname.

      IF lv_jobname_short = gc_job_name_docs.

        show_docs_check_result( iv_title      = iv_title
                                is_log_result = is_log_result ).

      ELSEIF lv_jobname_short = gc_job_name_dupl.

        show_dupl_check_result( iv_title      = iv_title
                                is_log_result = is_log_result ).

      ELSEIF lv_jobname_short = gc_job_name_qual.

        show_qual_check_result( iv_title      = iv_title
                                is_log_result = is_log_result ).

      ENDIF.

    ELSE.

      show_log_results( ).

    ENDIF.
  ENDMETHOD.                    " constructor

  " -----------------------------------------------------------------------
  " Method show_log_results
  " -----------------------------------------------------------------------
  METHOD show_log_results.

    DATA: lt_log_results   TYPE /qchk/tt_check_run,
          lr_alv_functions TYPE REF TO cl_salv_functions_list,
          lr_selections    TYPE REF TO cl_salv_selections,
          lv_icon          TYPE string.

    " Clear old data
    REFRESH gt_log_res.

    " Build new results list
    lt_log_results = get_log_data( ).

    fill_log_list( EXPORTING it_log_results         = lt_log_results
                   IMPORTING et_log_results_hitlist = gt_log_res  ).

    SORT gt_log_res BY run_id DESCENDING.

    IF gr_cust_container IS INITIAL.
      CREATE OBJECT gr_cust_container
        EXPORTING
          container_name              = 'CONTAINER_LOG'   " Name of the dynpro CustCtrl name to link this container to
          repid                       = sy-repid   " Dynpro to which this container is linked to
          dynnr                       = '0100'    " Report to which this container is linked to
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
    ENDIF.

    IF gr_log_table IS INITIAL.
      TRY.
          cl_salv_table=>factory( EXPORTING r_container    = gr_cust_container
                                            container_name = 'CONTAINER_LOG'
                                  IMPORTING r_salv_table   = gr_log_table
                                  CHANGING  t_table        = gt_log_res ).
        CATCH cx_salv_msg ##NO_HANDLER.
      ENDTRY.

      modify_log_columns( gr_log_table ).

*     set new functions for the log table
      lr_alv_functions = gr_log_table->get_functions( ).

      TRY.

          lv_icon = icon_refresh.
          lr_alv_functions->add_function( name     = 'REFRESH'                 " ALV Funktion
                                          icon     = lv_icon
                                          text     = 'Refresh'
                                          tooltip  = 'Reload Logs'
                                          position = if_salv_c_function_position=>right_of_salv_functions ). " Funktion Positionierung

          lv_icon = icon_delete.
          lr_alv_functions->add_function( name     = 'DELETE'                 " ALV Funktion
                                          icon     = lv_icon
                                          text     = 'Delete Log Entry'
                                          tooltip  = 'Reload Logs'
                                          position = if_salv_c_function_position=>right_of_salv_functions ). " Funktion Positionierung

        CATCH cx_salv_existing.   " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
        CATCH cx_salv_wrong_call. " ALV: Allg. Fehlerklasse (wird bei Syntaxprüfung geprüft)
      ENDTRY.

*     log table events
      set_log_event_handler( gr_log_table ).

      " Selection Mode
      lr_selections = gr_log_table->get_selections( ).
      lr_selections->set_selection_mode( value = if_salv_c_selection_mode=>row_column ).

      gr_log_table->display( ).

    ELSE.
      gr_log_table->refresh( ).
    ENDIF.

  ENDMETHOD.                    " show_log_results

  " -----------------------------------------------------------------------
  " Method get_log_data
  " -----------------------------------------------------------------------
  METHOD get_log_data.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE et_log_res_usage
    FROM /qchk/check_run
    ORDER BY PRIMARY KEY.

  ENDMETHOD.                    " get_log_data

  " -----------------------------------------------------------------------
  " Method fill_log_list
  " -----------------------------------------------------------------------
  METHOD fill_log_list.

    DATA: ls_log_hitlist TYPE /qchk/st_log_results_hitlist,
          lt_dom_values  TYPE TABLE OF dd07v.

    " Get the Domain values
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = '/QCHK/CVI_OBJTYPE'
        text           = abap_true
      TABLES
        dd07v_tab      = lt_dom_values
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      " No exception handling required
    ENDIF.

    "
    LOOP AT it_log_results ASSIGNING FIELD-SYMBOL(<fs_log_results>).

      MOVE-CORRESPONDING <fs_log_results> TO ls_log_hitlist.

      " Get job status
      IF     <fs_log_results>-jobcount IS ASSIGNED
         AND <fs_log_results>-jobname  IS ASSIGNED.
        lcl_cvi_service=>get_job_status( CHANGING cs_log_results_hitlist = ls_log_hitlist ).
      ENDIF.

      " Get description for object type
      READ TABLE lt_dom_values ASSIGNING FIELD-SYMBOL(<fs_dom_values>) WITH KEY domvalue_l = <fs_log_results>-object_type.
      IF sy-subrc = 0.
        ls_log_hitlist-object_desc = <fs_dom_values>-ddtext.
      ENDIF.

      " Get description for check type
      ls_log_hitlist-run_desc = lcl_cvi_service=>get_check_descr( ls_log_hitlist-jobname ).

      IF <fs_log_results>-timestamp IS ASSIGNED.
        " Convert timestamp to external date and time
        lcl_cvi_service=>convert_date_time_external( EXPORTING is_timestamp   = <fs_log_results>-timestamp
                                                     IMPORTING ev_dateandtime = ls_log_hitlist-dateandtime ).
      ENDIF.

      INSERT ls_log_hitlist INTO TABLE et_log_results_hitlist.

    ENDLOOP.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  "        Method set_log_event_handler
  " -----------------------------------------------------------------------
  METHOD set_log_event_handler.

    DATA lr_events TYPE REF TO cl_salv_events_table.

    lr_events = ir_log_table->get_event( ).

    " CREATE OBJECT gr_event_handler.
    gr_event_handler = NEW #( ).

    SET HANDLER gr_event_handler->on_log_double_click FOR lr_events.
    SET HANDLER gr_event_handler->on_user_command FOR lr_events.
    SET HANDLER gr_event_handler->on_job_link_click FOR lr_events.

  ENDMETHOD.    " set_log_event_handler

  " -----------------------------------------------------------------------
  " Method modify_log_columns
  " -----------------------------------------------------------------------
  METHOD modify_log_columns.

    DATA: lv_text       TYPE scrtext_l,
          lv_text_m     TYPE scrtext_m,
          lv_text_s     TYPE scrtext_s,

          lr_display    TYPE REF TO cl_salv_display_settings,
          lr_columns    TYPE REF TO cl_salv_columns,
          lr_column     TYPE REF TO cl_salv_column_table,
          lr_selections TYPE REF TO cl_salv_selections.

    TRY.
        lr_selections = ir_table->get_selections( ).
        lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        lr_display = ir_table->get_display_settings( ).
        lr_display->set_striped_pattern( cl_salv_display_settings=>true ).

        " Get all columns from structure
        lr_columns = ir_table->get_columns( ).

        " set optimal column width for all columns
        " lr_columns->set_optimize( abap_true ).

        lr_column ?= lr_columns->get_column( 'STATUS_ICON' ).
        lr_columns->set_column_position( columnname = 'STATUS_ICON' position = 1 ).
        lv_text = 'Status'(016). " Status
        " lr_column->set_medium_text( ' ' ).
        " lr_column->set_short_text( ' ' ).
        lr_column->set_long_text( lv_text ).
        lr_column->set_output_length( 5 ).

        " lr_column->set_tooltip( '' ).
        CLEAR: lv_text,
               lv_text_m,
               lv_text_s.

        "
        lr_column ?= lr_columns->get_column( 'STATUS' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
        "
        lr_column ?= lr_columns->get_column( 'RUN_ID' ).
        lr_column->set_technical( if_salv_c_bool_sap=>false ).
        lv_text = 'Laufnummer'(017).
        lr_column->set_medium_text( ' ' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_long_text( lv_text ).
        lr_column->set_output_length( 8 ).
        CLEAR: lv_text,
               lv_text_m,
               lv_text_s.

        lr_column ?= lr_columns->get_column( 'RUN_DESC' ).
        lr_column->set_technical( if_salv_c_bool_sap=>false ).
        lv_text = 'Prüfung'(020).
        lr_column->set_medium_text( ' ' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_long_text( lv_text ).
        lr_column->set_output_length( 18 ).
        CLEAR: lv_text,
               lv_text_m,
               lv_text_s.

*       lr_column ?= lr_columns->get_column( 'RUN_DESC' ).
*       lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'OBJECT_TYPE' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'OBJECT_DESC' ).
        lr_column->set_technical( if_salv_c_bool_sap=>false ).
        lv_text = 'Objektart'(018).
        lr_column->set_medium_text( ' ' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_long_text( lv_text ).
        lr_column->set_output_length( 10 ).
        CLEAR: lv_text,
               lv_text_m,
               lv_text_s.

        lr_column ?= lr_columns->get_column( 'JOBNAME' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'JOBCOUNT' ).
        lr_column->set_technical( if_salv_c_bool_sap=>false ).
        lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        "lr_columns->set_optimize( if_salv_c_bool_sap=>true ).
        lv_text = 'Jobnummer'(019).
        lr_column->set_medium_text( ' ' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_long_text( lv_text ).
*       lv_text = TEXT-003. " Klick auf Jobnummer für Joblog
*       lr_column->set_tooltip( lv_text ).
        CLEAR: lv_text,
               lv_text_m,
               lv_text_s.

        lr_column ?= lr_columns->get_column( 'TIMESTAMP' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

        lr_column ?= lr_columns->get_column( 'DATEANDTIME' ).
        lr_column->set_technical( if_salv_c_bool_sap=>false ).
        " lr_columns->set_optimize( if_salv_c_bool_sap=>true ).
        lv_text = 'Start des Laufs'(022).
        lr_column->set_medium_text( ' ' ).
        lr_column->set_short_text( ' ' ).
        lr_column->set_long_text( lv_text ).
*       lv_text = TEXT-003. " Klick auf Jobnummer für Joblog
*       lr_column->set_tooltip( lv_text ).
        lr_column->set_output_length( 16 ).
        CLEAR: lv_text,
               lv_text_m,
               lv_text_s.

        lr_column ?= lr_columns->get_column( 'STATUS_COMPLETE' ).
        lr_column->set_technical( if_salv_c_bool_sap=>true ).

      CATCH cx_salv_not_found ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.                    " modify_log_columns

  " -----------------------------------------------------------------------
  " Method get_docs_check_result
  " -----------------------------------------------------------------------
  METHOD get_docs_check_result.

    CLEAR et_documents_hitlist.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE et_documents_hitlist
    FROM /qchk/docs_use
    WHERE run_id = iv_runid.
    IF sy-subrc <> 0.
      MESSAGE i042(/qchk/cl_cvi_message) WITH iv_runid DISPLAY LIKE 'E'.
      " Für die angegebene RUN ID wurden keine Verwendungsnachweise gefunden.
    ENDIF.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_qual_check_result
  " -----------------------------------------------------------------------
  METHOD get_qual_check_result.

    CLEAR et_quality_check_res.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE et_quality_check_res
    FROM /qchk/s4_quality
    WHERE run_id = iv_runid
    ORDER BY /qchk/s4_quality~timestamp DESCENDING.
    IF sy-subrc <> 0.
      MESSAGE i045(/qchk/cl_cvi_message) WITH iv_runid DISPLAY LIKE 'E'.
      " Zu Laufnummer &1 wurden keine Ergebnisse gefunden
    ENDIF.

  ENDMETHOD.


  " -----------------------------------------------------------------------
  " Method get_dupl_check_result
  " -----------------------------------------------------------------------
  METHOD get_dupl_check_result.

    CLEAR et_dupl_check_res.

    SELECT *
    INTO CORRESPONDING FIELDS OF TABLE et_dupl_check_res
    FROM /qchk/duplicate
    WHERE run_id = iv_runid.
    IF sy-subrc <> 0.
      MESSAGE i045(/qchk/cl_cvi_message) WITH iv_runid DISPLAY LIKE 'E'.
      " Zu Laufnummer &1 wurden keine Ergebnisse gefunden
    ENDIF.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method modify_docs_result_tablet
  " -----------------------------------------------------------------------
  METHOD modify_docs_result_table.

    DATA: lv_title       TYPE lvc_title,
          lv_text_l      TYPE scrtext_l,
          lv_loevm       TYPE    string,
          lv_archiv      TYPE string,
          lv_object_desc TYPE string,

          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns,
          lo_column      TYPE REF TO cl_salv_column_table,
          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
          lo_grid_label  TYPE REF TO cl_salv_form_label,
          lo_grid_txt    TYPE REF TO cl_salv_form_text.

    lv_title = iv_title.

    IF is_log_result-object_type = 'C'.
      lv_object_desc = 'Kunden'(045).
    ELSE.
      lv_object_desc = 'Liferanten'(046).
    ENDIF.

    get_docs_sel_param( EXPORTING is_log_result = is_log_result
                        IMPORTING ev_arch       = lv_archiv
                                  ev_loevm      = lv_loevm ).

    " Grid-Header
    lo_grid = NEW #( ).

    " Heading
*    lo_grid->create_header_information( row    = 1
*                                        column = 2
*                                        text   = 'Art der Prüfung'(036) ).
    lo_grid_label = lo_grid->create_label( row    = 1
                                           column = 1
                                           text   = 'Art der Prüfung'(036) ).
    lo_grid_txt = lo_grid->create_text( row   = 1
                                      column  = 2
                                      text    = is_log_result-run_desc
                                      tooltip = is_log_result-run_desc ).

    " Laufnummer
    lo_grid_label = lo_grid->create_label( row     = 3
                                           column  = 1
                                           text    = 'Laufnummer:'
                                           tooltip = 'Laufnummer' ).
    lo_grid_txt = lo_grid->create_text( row     = 3
                                        column  = 2
                                        text    = is_log_result-run_id
                                        tooltip = is_log_result-run_id  ).
    " lo_grid_label->set_label_for( lo_grid_txt ).

    " Objektart
    lo_grid_label = lo_grid->create_label( row     = 4
                                           column  = 1
                                           text    = 'Objektart:'
                                           tooltip = 'Objektart' ).
    lo_grid_txt = lo_grid->create_text( row     = 4
                                        column  = 2
                                        text    = is_log_result-object_desc
                                        tooltip = is_log_result-object_desc  ).

    " Objektart
    lo_grid_label = lo_grid->create_label( row     = 5
                                           column  = 1
                                           text    = `Gesamt` && ` ` && lv_object_desc && ` Verarbeitet:`
                                           tooltip = 'Anzahl der verarbeiteten Datensätze' ).
    lo_grid_txt = lo_grid->create_text( row     = 5
                                        column  = 2
                                        text    = iv_count
                                        tooltip = iv_count  ).

    " Löschvermerk
    lo_grid_label = lo_grid->create_label( row     = 6
                                           column  = 1
                                           text    = `Löschvermerk ausschließen:`
                                           tooltip = 'Löschvermerk ausschließen' ).
    lo_grid_txt = lo_grid->create_text( row     = 6
                                        column  = 2
                                        text    = lv_loevm
                                        tooltip = lv_loevm  ).

    "Archivbelege ausschließen
    lo_grid_label = lo_grid->create_label( row     = 7
                                           column  = 1
                                           text    = `Archivbelege ausschließen`
                                           tooltip = 'Archivbelege ausschließen' ).
    lo_grid_txt = lo_grid->create_text( row     = 7
                                        column  = 2
                                        text    = lv_archiv
                                        tooltip = lv_archiv  ).

    ir_table->set_top_of_list( lo_grid ).

    " Modify Columns
    TRY.
        ir_table->set_screen_status( pfstatus      = 'STANDARD_FULLSCREEN'
                                     report        = 'SAPLSLVC_FULLSCREEN'
                                     set_functions = ir_table->c_functions_all ).

        lo_display = ir_table->get_display_settings( ).
        lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
        lo_display->set_list_header( lv_title ).
        lo_display->set_max_linesize( 1 ).

        lo_columns = ir_table->get_columns( ).
        "lo_columns->set_optimize( abap_true ).

        lo_column ?= lo_columns->get_column( 'CVI_CVNUM' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lv_text_l = 'Objektnummer'(025).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 11 ).

        lo_column ?= lo_columns->get_column( 'VBELN' ).
        lo_columns->set_column_position( columnname = 'VBELN' position = 2 ).
        lo_column->set_output_length( value = 10 ).

        lo_column ?= lo_columns->get_column( 'LAST_USE_DESCR' ).
        lv_text_l = 'Letzte Verwendung'(026).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 26 ).

        lo_column ?= lo_columns->get_column( 'LAST_USE_DATE' ).
        lv_text_l = 'Datum'(027).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 10 ).


        lo_column ?= lo_columns->get_column( 'CV_LINK' ).
        lv_text_l = 'Verknüpfung Kunde-Lieferant'(028).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 22 ).

        lo_column ?= lo_columns->get_column( 'ACCOUNT_GROUP' ).
        lv_text_l = 'Kontengruppe'(029).
        lo_column->set_short_text( '' ).
        lo_column->set_medium_text( '' ).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 10 ).

        lo_column ?= lo_columns->get_column( 'ACCOUNT_DESCR' ).
        lv_text_l = 'Kontengruppe Beschreibung'(030).
        lo_column->set_short_text( '' ).
        lo_column->set_medium_text( '' ).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 20 ).

        lo_column ?= lo_columns->get_column( 'LOEVM_CENTRAL' ).
        lv_text_l = 'Löschvermerk Zentral'(031).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 16 ).

        lo_column ?= lo_columns->get_column( 'LOEVM_BUKRS' ).
        lv_text_l = 'Löschvermerk Buchungskreis'(032).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 18 ).

        lo_column ?= lo_columns->get_column( 'LOEVM_SALES' ).
        lv_text_l = 'Löschvermerk Verk.Org.'(033).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 18 ).

        lo_column ?= lo_columns->get_column( 'NUMKR' ).
        lv_text_l = 'Nummernkr.'(034).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 10 ).

        " lo_column->set_technical( if_salv_c_bool_sap=>true ).

      CATCH cx_salv_not_found ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method modify_qual_result_table
  " -----------------------------------------------------------------------
  METHOD modify_qual_result_table.

    DATA: lv_text_l      TYPE scrtext_l,
          lv_text_m      TYPE scrtext_m,
          lv_text_s      TYPE scrtext_s,
          lv_title       TYPE lvc_title,
          lv_object_desc TYPE string,
          lv_icon        TYPE string,
          lv_grbew       TYPE lvc_fname,

          ls_grbew_ref   TYPE salv_s_ddic_reference,

          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns,
          lo_column      TYPE REF TO cl_salv_column_table,
          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
          lo_grid_label  TYPE REF TO cl_salv_form_label,
          lo_grid_txt    TYPE REF TO cl_salv_form_text.


    lv_title = iv_title.

    IF is_log_result-object_type = 'C'.
      lv_object_desc = 'Kunden'(045).
    ELSE.
      lv_object_desc = 'Liferanten'(046).
    ENDIF.

    " Grid-Header
    lo_grid = NEW #( ).

    " Heading
    lo_grid_label = lo_grid->create_label( row    = 1
                             column = 1
                             text   = 'Art der Prüfung'(036) ).
    lo_grid_txt = lo_grid->create_text( row     = 1
                                      column  = 2
                                      text    = is_log_result-run_desc
                                      tooltip = is_log_result-run_desc ).
    " Laufnummer
    lo_grid_label = lo_grid->create_label( row     = 3
                                           column  = 1
                                           text    = 'Laufnummer:'
                                           tooltip = 'Laufnummer' ).
    lo_grid_txt = lo_grid->create_text( row     = 3
                                        column  = 2
                                        text    = is_log_result-run_id
                                        tooltip = is_log_result-run_id  ).
    " lo_grid_label->set_label_for( lo_grid_txt ).

    " Objektart
    lo_grid_label = lo_grid->create_label( row     = 4
                                           column  = 1
                                           text    = 'Objektart:'
                                           tooltip = 'Objektart' ).
    lo_grid_txt = lo_grid->create_text( row     = 4
                                        column  = 2
                                        text    = is_log_result-object_desc
                                        tooltip = is_log_result-object_desc  ).

    " Objekt Anzahl
    lo_grid_label = lo_grid->create_label( row     = 5
                                           column  = 1
                                           text    = `Gesamt ` && lv_object_desc && ` Verarbeitet:`
                                           tooltip = 'Anzahl der verarbeiteten Datensätze' ).
    lo_grid_txt = lo_grid->create_text( row     = 5
                                        column  = 2
                                        text    = iv_count
                                        tooltip = iv_count  ).
    " Objekt Anzahl
    lo_grid_label = lo_grid->create_label( row     = 6
                                           column  = 1
                                           text    = `Gesamtzahl der Meldungen:`
                                           tooltip = 'Gesamtzahl der Meldungen' ).
    lo_grid_txt = lo_grid->create_text( row     = 6
                                        column  = 2
                                        text    = iv_count_err
                                        tooltip = iv_count_err  ).

*   Set Grid header
    ir_table->set_top_of_list( lo_grid ).

    " Modify Columns
    TRY.
        ir_table->set_screen_status( pfstatus      = 'STANDARD_FULLSCREEN' " 'SALV_STANDARD'
                                     report        = 'SAPLSLVC_FULLSCREEN' " 'SALV_DEMO_TABLE_EVENTS'
                                     set_functions = ir_table->c_functions_all ).

        lo_display = ir_table->get_display_settings( ).
        lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
        lo_display->set_list_header( lv_title ).
        lo_display->set_max_linesize( 1 ).

        lo_columns = ir_table->get_columns( ).
        "lo_columns->set_optimize( abap_true ).

        lo_column ?= lo_columns->get_column( 'SYNC_OBJ_SOUR_ID' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lv_text_l = 'Objektnummer'(025).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 10 ).
*
        lo_column ?= lo_columns->get_column( 'TYPE' ).
        "lv_text_l =
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 6 ).
*
        lo_column ?= lo_columns->get_column( 'MSGID' ).
        "lv_text_l =
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 10 ).
*
        lo_column ?= lo_columns->get_column( 'MSGNO' ).
        "lv_text_l = .
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 10 ).
*
        lo_column ?= lo_columns->get_column( 'ERROR_CATEGORY' ).
        "lv_text_l =
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 8 ).
*
        lo_column ?= lo_columns->get_column( 'TECH_FIELDNAME' ).
        "lv_text_l =
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 14 ).
*
        lo_column ?= lo_columns->get_column( 'ERROR_FIELD_VALUE' ).
        "lv_text_l =
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 10 ).
*
        lo_column ?= lo_columns->get_column( 'MESSAGE' ).
        "lv_text_l =
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 40 ).
*
        lo_column ?= lo_columns->get_column( 'JOBCOUNT' ).
        "lv_text_l =
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 8 ).
        lo_column->set_technical( if_salv_c_bool_sap=>true ).
*
        lo_column ?= lo_columns->get_column( 'JOBNAME' ).
        "lv_text_l =
        "lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( value = 8 ).
        lo_column->set_technical( if_salv_c_bool_sap=>true ).

*
*        lo_column ?= lo_columns->get_column( 'CONNECTED_BP' ).
*        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*        lv_text_s = 'Verb. GP'(038).
*        lo_column->set_short_text( lv_text_s ).
*        lv_text_m = 'Verb. Geschäftspartner'(039).
*        lo_column->set_medium_text( lv_text_m ).
*        lv_text_l = 'Verbundener Geschäftspartner'(037).
*        lo_column->set_long_text( lv_text_l ).
*        lo_column->set_output_length( value = 12 ).
*
**
*        lo_column ?= lo_columns->get_column( 'PARTNER' ).
*        "lv_text_l =
*        "lo_column->set_long_text( lv_text_l ).
*        lo_column->set_output_length( value = 8 ).

*      " lo_column->set_technical( if_salv_c_bool_sap=>true ).

      CATCH cx_salv_not_found ##NO_HANDLER.





    ENDTRY.

  ENDMETHOD.        " modify_qual_result_table

  " -----------------------------------------------------------------------
  " Method modify_dupl_result_table
  " -----------------------------------------------------------------------
  METHOD modify_dupl_result_table.

    DATA: lv_text_l      TYPE scrtext_l,
          lv_text_m      TYPE scrtext_m,
          lv_text_s      TYPE scrtext_s,
          lv_title       TYPE lvc_title,
          lv_object_desc TYPE string,
          lv_name        TYPE string,
          lv_adr         TYPE string,
          lv_ustid       TYPE string,


          lo_display     TYPE REF TO cl_salv_display_settings,
          lo_columns     TYPE REF TO cl_salv_columns,
          lo_column      TYPE REF TO cl_salv_column_table,
          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
          lo_grid_label  TYPE REF TO cl_salv_form_label,
          lo_grid_txt    TYPE REF TO cl_salv_form_text.

**
*    IF is_log_result-object_type = gc_objtype_c.
*      lv_object_desc = 'Kunden'(045).
*    ELSE.
*      lv_object_desc = 'Liferanten'(046).
*    ENDIF.

    " get selection parameters.
    get_dupl_sel_param( EXPORTING is_log_result = is_log_result
                        IMPORTING ev_name       = lv_name
                                  ev_adr        = lv_adr
                                  ev_ustid      = lv_ustid ).

    " Grid-Header
    lo_grid = NEW #( ).

    " Heading
*    lo_grid->create_header_information( row    = 1
*                                        column = 2
*                                        text   = 'Art der Prüfung'(036) ).
    lo_grid_label = lo_grid->create_label( row    = 1
                                           column = 1
                                           text   = 'Art der Prüfung'(036) ).
    lo_grid_txt = lo_grid->create_text( row   = 1
                                      column  = 2
                                      text    = is_log_result-run_desc
                                      tooltip = is_log_result-run_desc ).

    " Laufnummer
    lo_grid_label = lo_grid->create_label( row     = 3
                                           column  = 1
                                           text    = 'Laufnummer:'
                                           tooltip = 'Laufnummer' ).
    lo_grid_txt = lo_grid->create_text( row     = 3
                                        column  = 2
                                        text    = is_log_result-run_id
                                        tooltip = is_log_result-run_id  ).
    " lo_grid_label->set_label_for( lo_grid_txt ).

    " Objektart
    lo_grid_label = lo_grid->create_label( row     = 4
                                           column  = 1
                                           text    = 'Objektart:'
                                           tooltip = 'Objektart' ).
    lo_grid_txt = lo_grid->create_text( row     = 4
                                        column  = 2
                                        text    = is_log_result-object_desc
                                        tooltip = is_log_result-object_desc  ).
    " Objekt Anzahl
    lo_grid_label = lo_grid->create_label( row     = 5
                                           column  = 1
                                           text    = `Gesamtzahl der bearbeiteten Einträge:`  "`Gesamt  ` && lv_object_desc && ` Verarbeitet:`
                                           tooltip = 'Gesamtzahl der bearbeiteten Einträge' ).
    lo_grid_txt = lo_grid->create_text( row     = 5
                                        column  = 2
                                        text    = iv_count
                                        tooltip = iv_count  ).

    " Namensvergleich
    lo_grid_label = lo_grid->create_label( row     = 6
                                           column  = 1
                                           text    = 'Namensvergleich :'
                                           tooltip = 'Namensvergleich ' ).
    lo_grid_txt = lo_grid->create_text( row     = 6
                                        column  = 2
                                        text    = lv_name
                                        tooltip = is_log_result-object_desc  ).

    " Adressenvergleich
    lo_grid_label = lo_grid->create_label( row     = 7
                                           column  = 1
                                           text    = 'Adressenvergleich:'
                                           tooltip = 'Adressenvergleich' ).
    lo_grid_txt = lo_grid->create_text( row     = 7
                                        column  = 2
                                        text    = lv_adr
                                        tooltip = is_log_result-object_desc  ).

    " Steuer-ID Vergleich
    lo_grid_label = lo_grid->create_label( row     = 8
                                           column  = 1
                                           text    = 'Steuer-ID Vergleich:'
                                           tooltip = 'Steuer-ID Vergleich' ).
    lo_grid_txt = lo_grid->create_text( row     = 8
                                        column  = 2
                                        text    = lv_ustid
                                        tooltip = is_log_result-object_desc  ).

    " Set Grid-Header
    ir_table->set_top_of_list( lo_grid ).

    " Modify Columns
    TRY.
        ir_table->set_screen_status( pfstatus      = 'STANDARD_FULLSCREEN'
                                     report        = 'SAPLSLVC_FULLSCREEN'
                                     set_functions = ir_table->c_functions_all ).

        lo_display = ir_table->get_display_settings( ).
        lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
        lo_display->set_list_header( lv_title ).
        lo_display->set_max_linesize( 1 ).

        lo_columns = ir_table->get_columns( ).
        "lo_columns->set_optimize( abap_true ).

        lo_column ?= lo_columns->get_column( 'SYNC_OBJ_SOUR_ID' ).
        lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lv_text_l = 'Objektnummer'(025).
        lo_column->set_long_text( lv_text_l ).
        lo_column->set_output_length( 10 ).

        lo_column ?= lo_columns->get_column( 'JOBNAME' ).
        lo_column->set_technical( if_salv_c_bool_sap=>true ).

        lo_column ?= lo_columns->get_column( 'JOBCOUNT' ).
        lo_column->set_technical( if_salv_c_bool_sap=>true ).

*        lr_column ?= lr_columns->get_column( 'DUPLICATE_ID' ).
*
*        lr_column ?= lr_columns->get_column( 'PROBABILITY' ).
*        lv_text_m = text-046."Dubletten
*        lr_column->set_medium_text( lv_text_m ).

        lo_column ?= lo_columns->get_column( 'NAME_SRC' ).
        lo_column->set_optimized( abap_true ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_long_text( ' ' ).
        lv_text_m = 'Name'(040).
        lo_column->set_medium_text( lv_text_m ).
        lo_column->set_output_length( 35 ).

        lo_column ?= lo_columns->get_column( 'ADRESS_SRC' ).
        lo_column->set_optimized( abap_true ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_long_text( ' ' ).
        lv_text_m = 'Adresse'(041).
        lo_column->set_medium_text( lv_text_m ).
        lo_column->set_output_length( 30 ).

        lo_column ?= lo_columns->get_column( 'USTID_SRC' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_long_text( ' ' ).
        lv_text_m = 'Umsatzsteuer-ID'(044).
        lo_column->set_medium_text( lv_text_m ).
        lo_column->set_output_length( 20 ).

        lo_column ?= lo_columns->get_column( 'DUPLICATE_TYPE' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_medium_text( ' ' ).
        lo_column->set_output_length( 10 ).

        lo_column ?= lo_columns->get_column( 'DUPLICATE_PROBABILITY' ).
        lo_column->set_short_text( ' ' ).
        lo_column->set_long_text( ' ' ).
        lo_column->set_output_length( 12 ).

        lo_column ?= lo_columns->get_column( 'GROUP_ID' ).
        lv_text_s = 'Gruppen-ID'(043).
        lv_text_m = 'Gruppen-ID'(043).
        lv_text_l = 'Gruppen-ID'(043).
        lo_column->set_short_text( lv_text_s ).
        lo_column->set_medium_text( lv_text_m ).
        lo_column->set_long_text( lv_text_l ).

      CATCH  cx_salv_not_found.                         "#EC NO_HANDLER


    ENDTRY.



  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method set_docs_event_handler
  " -----------------------------------------------------------------------
  METHOD set_docs_event_handler.

    DATA lo_events TYPE REF TO cl_salv_events_table.

    lo_events = ir_docs_res->get_event( ).

    gr_event_handler = NEW lcl_event_handler( it_documents_hitlist ).
    gr_event_handler->set_obj_type( iv_obj_type = is_log_result-object_type ).

    "SET HANDLER gr_event_handler->on_objtype_linkl_click FOR lo_events.
    SET HANDLER gr_event_handler->on_obj_docs_linkl_click FOR lo_events.

  ENDMETHOD. " set_docs_event_handler


  " -----------------------------------------------------------------------
  " Method set_dupl_event_handler
  " -----------------------------------------------------------------------
  METHOD set_dupl_event_handler.

    DATA lo_events TYPE REF TO cl_salv_events_table.

    lo_events = ir_qual_res->get_event( ).

    gr_event_handler = NEW lcl_event_handler( it_duplicate_res ).
    gr_event_handler->set_obj_type( iv_obj_type = is_log_result-object_type ).

    SET HANDLER gr_event_handler->on_obj_dubl_linkl_click FOR lo_events.


  ENDMETHOD. " set_dupl_event_handler

  " -----------------------------------------------------------------------
  " Method set_qual_event_handler
  " -----------------------------------------------------------------------
  METHOD set_qual_event_handler.

    DATA lo_events TYPE REF TO cl_salv_events_table.

    lo_events = ir_qual_res->get_event( ).

    gr_event_handler = NEW lcl_event_handler( it_quality_check_res ).
    gr_event_handler->set_obj_type( iv_obj_type = is_log_result-object_type ).

    SET HANDLER gr_event_handler->on_obj_qual_linkl_click FOR lo_events.

    "SET HANDLER gr_event_handler->on_after_refresh FOR ALL INSTANCES ACTIVATION 'X'.               " for the next version
    "SET HANDLER gr_event_handler->on_alv_function FOR lo_events ACTIVATION abap_true.


  ENDMETHOD. " set_qual_event_handler

  " -----------------------------------------------------------------------
  " Method get_docs_check_result
  " -----------------------------------------------------------------------
  METHOD show_docs_check_result.

    DATA lv_count TYPE i.

    " Clear old data
    REFRESH gt_documents_res.

    " Build new results list

    get_docs_check_result( EXPORTING iv_runid             = is_log_result-run_id
                           IMPORTING et_documents_hitlist = gt_documents_res ).

    IF gt_documents_res IS INITIAL.
      RETURN.
    ENDIF.

    SORT gt_documents_res BY cvi_cvnum ASCENDING.

    lv_count = lines( gt_documents_res ).

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = gr_docs_result
                                CHANGING  t_table      = gt_documents_res ).
      CATCH cx_salv_msg ##NO_HANDLER.
    ENDTRY.

    modify_docs_result_table( iv_title      = iv_title
                              iv_count      = lv_count
                              is_log_result = is_log_result
                              ir_table      = gr_docs_result ).

    set_docs_event_handler( ir_docs_res          = gr_docs_result
                            is_log_result        =  is_log_result
                            it_documents_hitlist = gt_documents_res ).

    gr_docs_result->display( ).

  ENDMETHOD.    " get_docs_check_result

  " -----------------------------------------------------------------------
  " Method show_qual_check_result
  " -----------------------------------------------------------------------
  METHOD show_qual_check_result.

    DATA: lv_count       TYPE i,
          lv_count_err   TYPE i,
          lt_quality_res TYPE /qchk/tt_quality_check_hitlist,
          lv_no_result   TYPE boolean.

    " Clear old data
    REFRESH gt_quality_res .

    " Build new results list

    get_qual_check_result( EXPORTING iv_runid = is_log_result-run_id
                           IMPORTING et_quality_check_res = gt_quality_res ).

    IF gt_quality_res IS INITIAL.
      RETURN.
    ENDIF.

    SORT gt_quality_res BY sync_obj_sour_id ASCENDING.

*   get objects count
    lt_quality_res = gt_quality_res.
    DELETE ADJACENT DUPLICATES FROM lt_quality_res COMPARING sync_obj_sour_id.
    DESCRIBE TABLE lt_quality_res LINES lv_count.
*   get errors count
    lv_count_err = lines( gt_quality_res ).


    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = gr_quality_result
                                CHANGING  t_table      = gt_quality_res ).
      CATCH cx_salv_msg ##NO_HANDLER.
    ENDTRY.

    modify_qual_result_table( iv_title      = iv_title
                              iv_count      = lv_count
                              iv_count_err  = lv_count_err
                              is_log_result = is_log_result
                              ir_table      = gr_quality_result ).



    set_qual_event_handler( ir_qual_res          = gr_quality_result
                            is_log_result        = is_log_result
                            it_quality_check_res = gt_quality_res ).

    gr_quality_result->display( ).

  ENDMETHOD.    " show_qual_check_result

  " -----------------------------------------------------------------------
  " Method show_dupl_check_result
  " -----------------------------------------------------------------------
  METHOD show_dupl_check_result.

    DATA:

      lt_duplicate_res TYPE /qchk/tt_duplicate_res,
      lv_title         TYPE lvc_title,
      lv_count         TYPE i,
      lr_display       TYPE REF TO cl_salv_display_settings,
      lr_columns       TYPE REF TO cl_salv_columns,
      lr_column        TYPE REF TO cl_salv_column_table,
      lv_text_l        TYPE scrtext_l,
      lv_text_m        TYPE scrtext_m,
      lv_text_s        TYPE scrtext_s,
      lr_events        TYPE REF TO cl_salv_events_table,
      lv_no_result     TYPE boolean.


    " Clear old data
    REFRESH gt_dupl_res  .

    " Build new results list

    get_dupl_check_result( EXPORTING iv_runid = is_log_result-run_id
                           IMPORTING et_dupl_check_res = gt_dupl_res ).

    IF gt_dupl_res IS INITIAL.
      EXIT.
    ENDIF.

    lv_title = iv_title.

*   get table count
    lv_count = lines( gt_dupl_res ).

    SORT gt_dupl_res BY group_id sync_obj_sour_id duplicate_type ASCENDING.


    TRY.
        cl_salv_table=>factory(
        IMPORTING
          r_salv_table   = gr_dupl_result
        CHANGING
          t_table        = gt_dupl_res ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.


    modify_dupl_result_table( iv_title      = iv_title
                              iv_count      = lv_count
                              is_log_result = is_log_result
                              ir_table      = gr_dupl_result ).

    set_dupl_event_handler( ir_qual_res     = gr_dupl_result
                            is_log_result   = is_log_result
                            it_duplicate_res = gt_dupl_res ).


    gr_dupl_result->display( ).

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_dupl_sel_param
  " -----------------------------------------------------------------------
  METHOD get_dupl_sel_param.

    DATA:
      lv_variant TYPE  rsvar-variant,
      lt_valutab TYPE TABLE OF rsparams.

    lv_variant = is_log_result-jobcount && is_log_result-jobname.

    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = gc_report_name
        variant              = lv_variant
      TABLES
        valutab              = gt_valutab
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc EQ 0.
      lt_valutab = gt_valutab.
    ENDIF.

    LOOP AT lt_valutab ASSIGNING FIELD-SYMBOL(<fs_valutab>).

      CASE <fs_valutab>-selname.

        WHEN 'C_DUPNAM'.
          IF <fs_valutab>-low = 'X'.
            ev_name =  'Ja'.
          ELSE.
            ev_name =  'Nein'.
          ENDIF.
        WHEN 'C_DUPADR'.
          IF <fs_valutab>-low = 'X'.
            ev_adr =  'Ja'.
          ELSE.
            ev_adr =  'Nein'.
          ENDIF.
        WHEN 'C_DUPST'.
          IF <fs_valutab>-low = 'X'.
            ev_ustid =  'Ja'.
          ELSE.
            ev_ustid =  'Nein'.
          ENDIF.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_docs_sel_param
  " -----------------------------------------------------------------------
  METHOD get_docs_sel_param.

    DATA: lv_variant TYPE  rsvar-variant,
          lt_valutab TYPE TABLE OF rsparams.

    lv_variant = is_log_result-jobcount && is_log_result-jobname.

    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = gc_report_name
        variant              = lv_variant
      TABLES
        valutab              = gt_valutab
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc EQ 0.
      lt_valutab = gt_valutab.
    ENDIF.

    LOOP AT lt_valutab ASSIGNING FIELD-SYMBOL(<fs_valutab>).

      CASE <fs_valutab>-selname.

        WHEN 'C_LOEVM'.
          IF <fs_valutab>-low = 'X'.
            ev_loevm =  'Ja'.
          ELSE.
            ev_loevm =  'Nein'.
          ENDIF.

        WHEN 'C_BELEG'.
          IF <fs_valutab>-low = 'X'.
            ev_arch =  'Ja'.
          ELSE.
            ev_arch =  'Nein'.
          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD. "get_docs_sel_param


ENDCLASS.                    " lcl_display IMPLEMENTATION



**&---------------------------------------------------------------------*
**&       CLASS lcl_event_handler Implementation
**&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

*----------------------------------------------------------------------*
*       Method constructor
*----------------------------------------------------------------------*
  METHOD constructor.

    FIELD-SYMBOLS <lt_data> TYPE ANY TABLE.

    CHECK it_table IS NOT INITIAL.

    CREATE DATA gr_data LIKE it_table.
    ASSIGN gr_data->* TO <lt_data>.

    <lt_data> = it_table.

  ENDMETHOD.

*----------------------------------------------------------------------*
*       Method auto_refresh
*----------------------------------------------------------------------*
  METHOD auto_refresh.

    cl_gui_cfw=>set_new_ok_code( new_code = 'AUTO_REFRESH' ).

  ENDMETHOD.                    "auto_refresh

*----------------------------------------------------------------------*
*         Method on_double_click
*  ---------------------------------------------------------------------*
  METHOD on_log_double_click.

    DATA: lv_title   TYPE string,

          ls_run     TYPE /qchk/st_cvi_run,

          lo_display TYPE REF TO lcl_cvi_display.

    FIELD-SYMBOLS <fs_log_results_hitlist> TYPE /qchk/st_log_results_hitlist.


    READ TABLE gt_log_res ASSIGNING <fs_log_results_hitlist> INDEX row.
    IF sy-subrc = 0.
      CONCATENATE 'Prüfergebnisse'(021) <fs_log_results_hitlist>-run_desc <fs_log_results_hitlist>-dateandtime INTO lv_title SEPARATED BY space.

      CREATE OBJECT lo_display
        EXPORTING
          is_log_result = <fs_log_results_hitlist>
          iv_title      = lv_title.
    ENDIF.

    auto_refresh( ).

  ENDMETHOD.                    "on_double_click


*----------------------------------------------------------------------*
*         Method on_job_link_click
*  ---------------------------------------------------------------------*
  METHOD on_job_link_click.

    DATA: ls_log_res      TYPE /qchk/st_log_results_hitlist,
          ls_jobhead      TYPE tbtcjob,
          lt_job_steplist TYPE TABLE OF tbtcstep.

    READ TABLE gt_log_res INTO ls_log_res INDEX row.

    " Read job data from DB
    CALL FUNCTION 'BP_JOB_READ'
      EXPORTING
        job_read_jobname  = ls_log_res-jobname
        job_read_jobcount = ls_log_res-jobcount
        job_read_opcode   = '20' " btc_read_all_jobdata
      IMPORTING
        job_read_jobhead  = ls_jobhead
      TABLES
        job_read_steplist = lt_job_steplist
      EXCEPTIONS
        job_doesnt_exist  = 1
        OTHERS            = 99.

    CASE sy-subrc.
      WHEN 0.
        " Reading has worked
      WHEN 1.
        MESSAGE s127(bt) DISPLAY LIKE 'E' WITH ls_log_res-jobname.
        EXIT.
      WHEN OTHERS.
        MESSAGE s155(bt) DISPLAY LIKE 'E' WITH ls_log_res-jobname.
        EXIT.
    ENDCASE.

    IF ls_jobhead-newflag = 'O'.
      MESSAGE s182(bt) DISPLAY LIKE 'E' WITH ls_log_res-jobname.
      EXIT.
    ENDIF.

    " Show job data
    CALL FUNCTION 'BP_JOB_EDITOR'
      EXPORTING
        job_editor_dialog = 'Y' " btc_yes
        job_editor_opcode = '12' " btc_show_job
        job_head_input    = ls_jobhead
      TABLES
        job_steplist      = lt_job_steplist
      EXCEPTIONS
        OTHERS            = 99.
    IF sy-subrc <> 0.
      MESSAGE s999(/qchk/cl_cvi_message) WITH 'Konnte' 'Job-Editor' 'nicht' 'öffnen' DISPLAY LIKE 'E'.
    ENDIF.

  ENDMETHOD.


*----------------------------------------------------------------------*
*         Method on_user_command
*  ---------------------------------------------------------------------*
  METHOD on_user_command.

    DATA: lv_answer        TYPE c LENGTH 1,

          ls_selected_row  TYPE /qchk/st_log_results_hitlist,
          ls_layout        TYPE lvc_s_layo,

          lt_selected_rows TYPE salv_t_row,

          lr_selections    TYPE REF TO cl_salv_selections,

          lo_full_adap     TYPE REF TO cl_salv_fullscreen_adapter,
          lo_grid          TYPE REF TO cl_gui_alv_grid.

    CASE e_salv_function.

      " WHEN 'MYFUNCTION'.

      WHEN 'REFRESH'.
        auto_refresh( ).

      WHEN 'DELETE'.
        lr_selections = gr_log_table->get_selections( ).
        lt_selected_rows = lr_selections->get_selected_rows( ).

        IF lt_selected_rows IS INITIAL.
          MESSAGE i044(/qchk/cl_cvi_message) DISPLAY LIKE 'I'.  " Bitte Zutreffende Laufnummer ankreuzen
          RETURN.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Löschen?'(048) "'Löschen?'
            text_question         = 'Ausgewählte Daten löschen?'(047) "'Ausgewählte Daten löschen? ?'
            display_cancel_button = abap_false
          IMPORTING
            answer                = lv_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        IF lv_answer = '1'.

          LOOP AT lt_selected_rows ASSIGNING FIELD-SYMBOL(<ls_row>).

            READ TABLE gt_log_res INTO ls_selected_row INDEX <ls_row>.
            IF sy-subrc = 0.
              DELETE gt_log_res INDEX <ls_row>.
              DELETE FROM /qchk/check_run WHERE run_id = ls_selected_row-run_id.

              " delete results from document use db
              SELECT COUNT( * ) FROM /qchk/docs_use UP TO 1 ROWS WHERE run_id = ls_selected_row-run_id.
              IF sy-subrc = 0.
                DELETE FROM /qchk/docs_use WHERE run_id = ls_selected_row-run_id.
              ENDIF.

              " delete results from S4 checks  db
              SELECT COUNT( * ) FROM /qchk/s4_quality UP TO 1 ROWS WHERE run_id = ls_selected_row-run_id.
              IF sy-subrc = 0.
                DELETE FROM /qchk/s4_quality WHERE run_id = ls_selected_row-run_id.
              ENDIF.

              " delete results from duplicate  db
              SELECT COUNT( * ) FROM /qchk/duplicate UP TO 1 ROWS WHERE run_id = ls_selected_row-run_id.
              IF sy-subrc = 0.
                DELETE FROM /qchk/duplicate WHERE run_id = ls_selected_row-run_id.
              ENDIF.

            ENDIF.

          ENDLOOP.

        ENDIF.

        auto_refresh( ).
    ENDCASE.
  ENDMETHOD.

*----------------------------------------------------------------------*
*         Method on_objtype_linkl_click
*  ---------------------------------------------------------------------*
*  METHOD on_objtype_linkl_click.
*
*    DATA: lv_obj_type TYPE /qchk/cvi_objtype.
*
*    FIELD-SYMBOLS <fs_documents_hitlist> TYPE /qchk/st_documents_hitlist.
*
*    lv_obj_type = gr_event_handler->get_obj_type( ).
*
*    "
*    READ TABLE gt_documents_res ASSIGNING <fs_documents_hitlist> INDEX row.
*    IF sy-subrc = 0.
*      IF sy-subrc = 0 AND <fs_documents_hitlist>-cvi_cvnum IS ASSIGNED.
*        IF lv_obj_type = gc_objtype_v.   " vendor/supplier display
*          SET PARAMETER ID 'LIF' FIELD <fs_documents_hitlist>-cvi_cvnum.
*          CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN. "#EC
*        ELSE.                   " customer/debitor display
*          SET PARAMETER ID 'KUN' FIELD <fs_documents_hitlist>-cvi_cvnum.
*          CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN. "#
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*  ENDMETHOD.

*----------------------------------------------------------------------*
*         Method on_obj_dubl_linkl_click
*  ---------------------------------------------------------------------*
  METHOD on_obj_dubl_linkl_click.

    DATA: lv_obj_type TYPE /qchk/cvi_objtype.

    FIELD-SYMBOLS <fs_duplicate_res> TYPE /qchk/st_duplicate_res.

    lv_obj_type = gr_event_handler->get_obj_type( ).

    "
    READ TABLE gt_dupl_res ASSIGNING <fs_duplicate_res> INDEX row.
    IF sy-subrc = 0.
      IF sy-subrc = 0 AND <fs_duplicate_res>-sync_obj_sour_id IS ASSIGNED.
        IF lv_obj_type = gc_objtype_v.   " vendor/supplier display
          SET PARAMETER ID 'LIF' FIELD <fs_duplicate_res>-sync_obj_sour_id.
          CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN. "#EC
        ELSE.                   " customer/debitor display
          SET PARAMETER ID 'KUN' FIELD <fs_duplicate_res>-sync_obj_sour_id.
          CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN. "#EC
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.   " on_obj_dubl_linkl_click


**----------------------------------------------------------------------*
**       Method on_obj_docs_linkl_click
**----------------------------------------------------------------------*
  METHOD on_obj_docs_linkl_click.

    DATA: lv_obj_type    TYPE /qchk/cvi_objtype,
          lv_object_desc TYPE string,
          lv_cvnum_txt   TYPE string.

    FIELD-SYMBOLS <fs_documents_hitlist> TYPE /qchk/st_documents_hitlist.

    lv_obj_type = gr_event_handler->get_obj_type( ).

    " If the connected partner is to be displayed, then
    " the selected partner type (vendor/customer) must be reversed.
    IF column = gc_cv_link.
      IF lv_obj_type = gc_objtype_v.
        lv_obj_type = gc_objtype_c.
      ELSE.
        lv_obj_type = gc_objtype_v.
      ENDIF.
      " Display connected partner
      READ TABLE gt_documents_res ASSIGNING <fs_documents_hitlist> INDEX row.
      IF sy-subrc = 0 AND <fs_documents_hitlist>-cv_link IS NOT INITIAL.
        IF lv_obj_type = gc_objtype_v.   " vendor/creditor display
          SET PARAMETER ID 'LIF' FIELD <fs_documents_hitlist>-cv_link.
          CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN. "#EC
        ELSE.                   " customer/debitor display
          SET PARAMETER ID 'KUN' FIELD <fs_documents_hitlist>-cv_link.
          CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN. "#EC
        ENDIF.
      ELSE.
        " set description
        IF lv_obj_type = gc_objtype_v.
          lv_object_desc = 'Kunden'(052).
          lv_cvnum_txt = 'Lieferantennummer'(050).
        ELSE.
          lv_object_desc = 'Liferant'(051).
          lv_cvnum_txt = 'Kundennummer'(049).
        ENDIF.
        MESSAGE i048(/qchk/cl_cvi_message) WITH lv_object_desc
                                               |{ <fs_documents_hitlist>-cvi_cvnum ALPHA = OUT }|
                                                lv_cvnum_txt DISPLAY LIKE 'I'.
      ENDIF.
    ELSE.
      " Display vendor/customer
      READ TABLE gt_documents_res ASSIGNING <fs_documents_hitlist> INDEX row.
      IF sy-subrc = 0 AND <fs_documents_hitlist>-cvi_cvnum IS NOT INITIAL.
        IF lv_obj_type = gc_objtype_v.   " vendor/creditor display
          SET PARAMETER ID 'LIF' FIELD <fs_documents_hitlist>-cvi_cvnum.
          CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN. "#EC
        ELSE.                   " customer/debitor display
          SET PARAMETER ID 'KUN' FIELD <fs_documents_hitlist>-cvi_cvnum.
          CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN. "#EC
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


*----------------------------------------------------------------------*
*         Method on_obj_qual_linkl_click
*  ---------------------------------------------------------------------*
  METHOD on_obj_qual_linkl_click.

    DATA: lv_obj_type TYPE /qchk/cvi_objtype.

    FIELD-SYMBOLS <fs_quality_hitlist> TYPE /qchk/st_quality_check_hitlist.

    lv_obj_type = gr_event_handler->get_obj_type( ).


    " Display vendor/customer
    READ TABLE gt_quality_res ASSIGNING <fs_quality_hitlist> INDEX row.
    IF sy-subrc = 0.
      IF sy-subrc = 0 AND <fs_quality_hitlist>-sync_obj_sour_id IS ASSIGNED.
        IF lv_obj_type = gc_objtype_v.   " vendor/supplier display
          SET PARAMETER ID 'LIF' FIELD <fs_quality_hitlist>-sync_obj_sour_id.
          CALL TRANSACTION 'XK03' AND SKIP FIRST SCREEN. "#EC
        ELSE.                   " customer/debitor display
          SET PARAMETER ID 'KUN' FIELD <fs_quality_hitlist>-sync_obj_sour_id.
          CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN. "#EC
        ENDIF.
      ENDIF.
    ENDIF.


    "ENDIF.

  ENDMETHOD.

*>---------------------------------------------------------------------*
*         Method set_obj_type
*  ---------------------------------------------------------------------*
  METHOD set_obj_type.

    lv_obj_type = iv_obj_type.

  ENDMETHOD.

*>---------------------------------------------------------------------*
*         Method get_obj_type
*  ---------------------------------------------------------------------*
  METHOD get_obj_type.

    rv_obj_type =  lv_obj_type.

  ENDMETHOD.

  " for the next version
***********************************************************************
*>---------------------------------------------------------------------*
*         Method on_after_refresh
*  ---------------------------------------------------------------------*
*  METHOD on_after_refresh.
*
*    DATA: ls_layout TYPE lvc_s_layo,
*          lt_fcat   TYPE lvc_t_fcat.
*
*    FIELD-SYMBOLS: <ls_fcat> LIKE LINE OF lt_fcat.
*
*    TRY .
*        SET HANDLER on_after_refresh FOR ALL INSTANCES ACTIVATION space.
*
*        sender->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).
*        sender->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
*
*        LOOP AT lt_fcat ASSIGNING <ls_fcat>.
*          IF <ls_fcat>-fieldname EQ 'ERROR_FIELD_VALUE'.
*            <ls_fcat>-edit = abap_true.
*          ENDIF.
*        ENDLOOP.
*        sender->set_frontend_fieldcatalog( lt_fcat ).
**        ls_layout-edit = abap_true. "Edit ALL
*        sender->set_frontend_layout( ls_layout ).
*        sender->set_ready_for_input( 1 ).
*      CATCH cx_salv_error.
*
*    ENDTRY.
*
*  ENDMETHOD.

***********************************************************************
*  METHOD on_toolbar.
*
*    DATA: ls_layout  TYPE lvc_s_layo,
*          lo_grid    TYPE REF TO cl_gui_alv_grid,
*
*          mt_toolbar TYPE ttb_button,
*          ls_toolbar LIKE LINE OF mt_toolbar,
*          lo_salv    TYPE REF TO cl_salv_table.
*
*    TRY .
*        LOOP AT t_salv INTO lo_salv.
*          lo_grid = lcl_salv_model=>get_grid( lo_salv ).
*          IF lo_grid EQ sender.
*            EXIT.
*          ELSE.
*            CLEAR lo_grid.
*          ENDIF.
*        ENDLOOP.
*      CATCH cx_salv_msg.
*        EXIT.
*    ENDTRY.
*
*    CHECK lo_grid IS BOUND.
*    CHECK lo_grid->is_ready_for_input( ) = 1.
*
**… Toolbar Button CHECK
*CLEAR ls_toolbar.
*ls_toolbar–function    = cl_gui_alv_grid=>mc_fc_check.
*ls_toolbar–quickinfo  = text–053.  “Eingaben prfen
*ls_toolbar–icon        = icon_check.
*ls_toolbar–disabled    = space.
*APPEND ls_toolbar TO mt_toolbar.
*
*  ENDMETHOD.
**********************************************************************

*  METHOD on_alv_function.
*
*    CASE e_salv_function.
*      WHEN 'OWN'.
*        MESSAGE i045(/QCHK/CL_CVI_MESSAGE) DISPLAY LIKE 'I'.
*      WHEN OTHERS.
*
*    ENDCASE.
*
*  ENDMETHOD.
***********************************************************************


ENDCLASS.                    "lcl_event_handler IMPLEMENTATION
