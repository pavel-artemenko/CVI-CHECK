*&---------------------------------------------------------------------*
*&  Include  /qchk/cvi_check_proc_lcl_imp
*&---------------------------------------------------------------------*

**&---------------------------------------------------------------------*
**&       Class lcl_cvi_checks
**&---------------------------------------------------------------------*
CLASS lcl_cvi_checks IMPLEMENTATION.

  " -----------------------------------------------------------------------
  " Method constructor
  " -----------------------------------------------------------------------
  METHOD constructor.

    DATA: ls_obj_tuple  TYPE mdst_sync_object,
          ls_load_api   TYPE mdst_load_api,
          ls_error      TYPE mds_ctrls_error,               "#EC NEEDED
          ls_sync_param TYPE mdst_sync_parameter.

*   call super
    super->constructor( ).

* --------------------------------
* Get control parameter
* --------------------------------
    CASE iv_obj_source.
      WHEN 'CUSTOMER'.
        ls_obj_tuple-sync_obj_source = gc_objtype_cust.
        ls_obj_tuple-sync_obj_target = 'BP'.
        ls_sync_param-sel_criteria-id_sel_table = 'KNA1'.
        ls_sync_param-data-sel_kind = 'B'.
      WHEN 'VENDOR'.
        ls_obj_tuple-sync_obj_source = gc_objtype_vend.
        ls_obj_tuple-sync_obj_target = 'BP'.
        ls_sync_param-sel_criteria-id_sel_table = 'LFA1'.
        ls_sync_param-data-sel_kind = 'A'.
    ENDCASE.

    gv_dupg = '79'. "iv_dupg.
    gv_check_type = iv_check_type.
    gv_duppustid = iv_duppustid.
    gs_sync_param-data-test_run = abap_true.
    gs_sync_param-data-sel_kind =  ls_sync_param-data-sel_kind.
    gv_obj_source = iv_obj_source.
    gv_uuid = iv_uuid.
    gv_timestamp = iv_timestamp.
    gv_jobcount = iv_jobcount.
    gv_jobname = iv_jobname.
    gv_runid   = iv_runid.
    gv_duppadr = iv_duppadr.
    gv_duppnam = iv_duppnam.
    "gv_save = iv_save.

    initialize_load_api( EXPORTING is_object_tuple = ls_obj_tuple
                         IMPORTING es_load_api = ls_load_api ).

    read_by_source_object( EXPORTING is_object_tuple = ls_obj_tuple
                           IMPORTING es_load_api     = ls_load_api
                                     es_error        = ls_error ).

    gs_sync_param-data-business_process     = ls_load_api-business_process.
    gs_sync_param-source-sync_obj_source    = ls_load_api-sync_object.
    gs_sync_param-source-extract_class      = ls_load_api-extract_class.
    gs_sync_param-data-sync_obj_target      = ls_load_api-sync_obj_target.
    gs_sync_param-data-block_size           = 50.
    gs_sync_param-sel_criteria-id_sel_table = ls_sync_param-sel_criteria-id_sel_table.
    gs_sync_param-sel_criteria-sel_options = it_sel_options.

  ENDMETHOD.                    "constructor

  " -----------------------------------------------------------------------
  " Method process
  " -----------------------------------------------------------------------
  METHOD process.

    DATA: lv_relative_name   TYPE string,
          lv_flg_last_block  TYPE comt_boolean,

          ls_error_objects   TYPE mds_ctrls_error_objects,
          ls_cust_data       TYPE cmds_ei_main,
          ls_vend_data       TYPE vmds_ei_main,
          ls_data            TYPE REF TO data,

          lt_object_id       TYPE mdst_sync_object_id_tab,
          lt_object_id_part  TYPE mdst_sync_object_id_tab,
          lt_customers       TYPE cmds_ei_extern_t,
          lt_customers_compl TYPE cmds_ei_extern_t,
          lt_vendors         TYPE vmds_ei_extern_t,
          lt_vendors_compl   TYPE vmds_ei_extern_t,

          lr_struct_descr    TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS <fs_data> TYPE any.

    " ------------------------------------------------------------------------------
    " Get instance for extractor class
    " ------------------------------------------------------------------------------
    TRY.
        CALL METHOD (gs_sync_param-source-extract_class)=>if_mds_extractor_inst~get_instance
          RECEIVING
            rr_instance = gr_ref_extractor.
      CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*      MESSAGE x002 WITH 'if_mds_extractor_inst~get_instance' is_control-source-extract_class.
      CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
*      MESSAGE x005 WITH is_control-source-extract_class.
    ENDTRY.

    " ------------------------------------------------------------------------------
    " Initialize object selection
    " ------------------------------------------------------------------------------
    exec_initializing( ).

    TRY.
        gr_ref_extractor->initialize_id( EXPORTING iv_source_object = gs_sync_param-source-sync_obj_source
                                                   it_sel_criteria  = gs_sync_param-sel_criteria
                                         IMPORTING et_messages      = ls_error_objects ).

      CATCH cx_mds_extractor ##NO_HANDLER.
*      MESSAGE x006 WITH 'CX_MDS_EXTRACTOR' 'initialize_id' is_control-source-extract_class.
    ENDTRY.

    " ------------------------------------------------------------------------------
    " Process data synchronization
    " ------------------------------------------------------------------------------
    " Caution here - a maximum of 100000 records are selected in one call -
    " LV_FLG_LAST_BLOCK indicates whether everything has been selected. So here is a while loop
    WHILE lv_flg_last_block IS INITIAL.

      " Extract object ids
      TRY.
          gr_ref_extractor->extract_id( IMPORTING et_object_id  = lt_object_id
                                                  ev_last_block = lv_flg_last_block
                                                  et_messages   = ls_error_objects ).

        CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*        MESSAGE x002 WITH 'extract_id' is_control-source-extract_class.
        CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
*        MESSAGE x005 WITH is_control-source-extract_class.
        CATCH cx_mds_extractor ##NO_HANDLER.
*        MESSAGE x006 WITH 'CX_MDS_EXTRACTOR' 'extract_id' is_control-source-extract_class.
      ENDTRY.

      WHILE lt_object_id[] IS NOT INITIAL.

        " Block Handling for inner loop
        CLEAR lt_object_id_part[].
        APPEND LINES OF lt_object_id FROM 1 TO gs_sync_param-data-block_size
               TO lt_object_id_part.
        DELETE lt_object_id FROM 1 TO gs_sync_param-data-block_size.

        " ------------------------------------------------------------------------------
        " Initialize object extraction
        " ------------------------------------------------------------------------------
        TRY.
            gr_ref_extractor->initialize( EXPORTING iv_source_object = gs_sync_param-source-sync_obj_source
                                                    iv_block_size    = gs_sync_param-data-block_size
                                                    it_object_id     = lt_object_id_part
                                          IMPORTING et_messages      = ls_error_objects ).

          CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*      MESSAGE x002 WITH 'initialize' is_control-source-extract_class.
          CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
*      MESSAGE x005 WITH is_control-source-extract_class.
          CATCH cx_mds_extractor ##NO_HANDLER.
*      MESSAGE x006 WITH 'CX_MDS_EXTRACTOR' 'initialize' is_control-source-extract_class.
        ENDTRY.

        " ------------------------------------------------------------------------------
        " Extract object data from object list
        " ------------------------------------------------------------------------------
        TRY.
            gr_ref_extractor->extract_data( EXPORTING iv_source_object = gs_sync_param-source-sync_obj_source
                                                      iv_sel_kind      = gs_sync_param-data-sel_kind
                                                      it_object_id     = lt_object_id_part " gs_sync_param-object_list
                                            IMPORTING es_data          = ls_data
                                                      et_messages      = ls_error_objects ).

            ASSIGN ls_data->* TO <fs_data>.
            IF sy-subrc <> 0.
              EXIT.
*           MESSAGE x003.
            ENDIF.

          CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*      MESSAGE x002 WITH 'extract_data' is_control-source-extract_class.
          CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
*      MESSAGE x005 WITH is_control-source-extract_class.
          CATCH cx_mds_extractor ##NO_HANDLER.
*      MESSAGE x006 WITH 'CX_MDS_EXTRACTOR' 'extract_data' is_control-source-extract_class.
        ENDTRY.

        " ------------------------------------------------------------------------------
        " Execute check
        " ------------------------------------------------------------------------------
        exec_check( ls_data ).

      ENDWHILE.

    ENDWHILE.

    " ------------------------------------------------------------------------------
    " Finalize object selection
    " ------------------------------------------------------------------------------
    TRY.
        gr_ref_extractor->finalize_id( ).
      CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*      MESSAGE x002 WITH 'finalize_id' is_control-source-extract_class.
      CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
*      MESSAGE x005 WITH is_control-source-extract_class.
    ENDTRY.

    " ------------------------------------------------------------------------------
    " Postprocessing steps on results
    " ------------------------------------------------------------------------------
    exec_processing_completion( ).

    " ------------------------------------------------------------------------------
    " Save result
    " ------------------------------------------------------------------------------
    exec_save(  ).


  ENDMETHOD.

  " ------------------------------------------------------------------------------
  " Method initialize_load_api.
  " ------------------------------------------------------------------------------
  METHOD initialize_load_api.
    es_load_api-sync_object     = is_object_tuple-sync_obj_source.
    es_load_api-sync_obj_target = is_object_tuple-sync_obj_target.

    SELECT SINGLE extract_class block_size FROM mdse_ctrl_obj
      INTO (es_load_api-extract_class, es_load_api-block_size)
      WHERE sync_object = es_load_api-sync_object.


    SELECT SINGLE queue_name FROM mdse_ctrl_opt
      INTO es_load_api-queue_name
      WHERE  sync_obj_source  = es_load_api-sync_object AND
             sync_obj_target  = es_load_api-sync_obj_target.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method read_by_source_object.
  " -----------------------------------------------------------------------
  METHOD read_by_source_object.

    DATA: lv_strategy   TYPE mds_ctrl_opt_class,            "#EC NEEDED
          ls_error      TYPE mds_ctrls_error,
          ls_id_fl_key  TYPE mdst_sync_id_fl_key,
          ls_api_id_fl  TYPE mdst_load_api_data_id_fl,
          lt_api_id_fl  TYPE mdst_load_api_id_fl_tab,
          ls_id_fl      TYPE mdst_sync_id_fl,
          lt_id_fl      TYPE mdst_sync_id_fl_tab,
          ls_seltab_key TYPE mdst_sync_seltab_key,
          ls_seltab     TYPE mdst_sync_seltab,
          lt_seltab     TYPE mdst_sync_seltab_tab,
          ls_api_seltab TYPE mdst_load_api_data_seltab,
          lt_api_seltab TYPE mdst_load_api_seltab_tab,
          lv_msgtext    TYPE char72,                        "#EC NEEDED
          ls_bapiret2   TYPE bapiret2,
          lv_dummy_1    TYPE char1,
          lv_dummy_2    TYPE char1.

    CALL METHOD mds_ctrl_customizing=>sync_object_check
      EXPORTING
        iv_sync_object = is_object_tuple-sync_obj_source
      IMPORTING
        es_error       = es_error.

    IF es_error-is_error = 'X'.
      RETURN.
    ENDIF.

    es_load_api-sync_object     = is_object_tuple-sync_obj_source. "ls_get_valid-sync_obj_source.
    es_load_api-sync_obj_target = is_object_tuple-sync_obj_target. "ls_get_valid-sync_obj_target.

* get extractor class...
    CALL METHOD mds_ctrl_customizing=>extractor_class_read
      EXPORTING
        iv_source_object   = is_object_tuple-sync_obj_source
      IMPORTING
        ev_extractor_class = es_load_api-extract_class
        ev_block_size      = es_load_api-block_size
        es_error           = ls_error.

* get queue name
    CALL METHOD mds_ctrl_customizing=>class_for_sync_step_read
      EXPORTING
        iv_source_object  = is_object_tuple-sync_obj_source
        iv_target_object  = is_object_tuple-sync_obj_target
      IMPORTING
        ev_strategy_class = lv_strategy
        ev_queue_name     = es_load_api-queue_name
        es_error          = ls_error.

* set max processes to '1' as default
    es_load_api-max_processes = 1.

    CLEAR: ls_error.
*    ls_proc_key-sync_object = es_load_api-sync_object.

    ls_id_fl_key-sync_obj = es_load_api-sync_object.

    SELECT * FROM mdse_sync_id_fl
      INTO TABLE lt_id_fl
      WHERE sync_obj = ls_id_fl_key-sync_obj.

    IF NOT sy-subrc IS INITIAL.
      CLEAR lt_id_fl.

      MESSAGE e020(mds_load) WITH 'MDSE_SYNC_ID_FL' lv_dummy_1 lv_dummy_2 INTO lv_msgtext.
      ls_bapiret2 = mds_ctrl_toolbox=>fill_bapiret2( ).
      APPEND ls_bapiret2 TO es_error-messages.

    ENDIF.

    LOOP AT lt_id_fl INTO ls_id_fl.
      ls_api_id_fl-id_sel_table = ls_id_fl-id_sel_table.
      ls_api_id_fl-id_sel_field = ls_id_fl-id_sel_field.
      APPEND ls_api_id_fl TO lt_api_id_fl.
      CLEAR ls_api_id_fl.
    ENDLOOP.

    es_load_api-fields = lt_api_id_fl.
    CLEAR lt_api_id_fl.

    ls_seltab_key-sync_object = es_load_api-sync_object.

    SELECT * FROM mdse_sync_seltab
      INTO TABLE lt_seltab
      WHERE sync_object = ls_seltab_key-sync_object.

    IF NOT sy-subrc IS INITIAL.
      CLEAR lt_seltab.

      MESSAGE e020(mds_load) WITH 'MDSE_SYNC_SELTAB' lv_dummy_1 lv_dummy_2 INTO lv_msgtext.
      ls_bapiret2 = mds_ctrl_toolbox=>fill_bapiret2( ).
      APPEND ls_bapiret2 TO es_error-messages.

    ENDIF.
    IF NOT ls_error IS INITIAL.
      APPEND LINES OF ls_error-messages TO es_error-messages.
      CLEAR ls_error.
    ENDIF.

    LOOP AT lt_seltab INTO ls_seltab.
      ls_api_seltab-extract_table = ls_seltab-extract_table.
      APPEND ls_api_seltab TO lt_api_seltab.
      CLEAR ls_api_seltab.
    ENDLOOP.

    es_load_api-extr_tab = lt_api_seltab.
    CLEAR lt_api_seltab.

  ENDMETHOD.

ENDCLASS.

**&---------------------------------------------------------------------*
**&       Class lcl_cvi_check_factory
**&---------------------------------------------------------------------*
CLASS lcl_cvi_check_factory IMPLEMENTATION.

  " -----------------------------------------------------------------------
  " Method exec_create_check
  " -----------------------------------------------------------------------
  METHOD exec_create_check.

    DATA: lv_jobname       TYPE btcjob,
          lv_jobname_short TYPE char6.

    " convert to short for duplicate check
    "lv_jobname = lv_jobname_short = iv_jobname.

    CASE iv_check_type.
        " Create objects performing checks.
      WHEN 'CHECK_DOCS'.
        CREATE OBJECT er_check_object TYPE lcl_documents_check
          EXPORTING
            it_sel_options = it_sel_options
            iv_obj_source  = iv_obj_source
            iv_dupg        = iv_dupg
            iv_duppadr     = iv_duppadr
            iv_duppnam     = iv_duppnam
            iv_duppustid   = iv_duppustid
            iv_check_type  = iv_check_type
            iv_excarc      = iv_excarc
            iv_excdel      = iv_excdel
            iv_uuid        = iv_uuid
            iv_runid       = iv_runid
            iv_timestamp   = iv_timestamp
            iv_jobcount    = iv_jobcount
            iv_jobname     = iv_jobname
            iv_save        = iv_save.

      WHEN 'CHECK_DUPL'.
        CREATE OBJECT er_check_object TYPE lcl_duplicate_check
          EXPORTING
            it_sel_options = it_sel_options
            iv_obj_source  = iv_obj_source
            iv_dupg        = iv_dupg
            iv_duppadr     = iv_duppadr
            iv_duppnam     = iv_duppnam
            iv_duppustid   = iv_duppustid
            iv_type        = iv_type
            iv_excarc      = iv_excarc
            iv_excdel      = iv_excdel
            iv_uuid        = iv_uuid
            iv_runid       = iv_runid
            iv_timestamp   = iv_timestamp
            iv_jobcount    = iv_jobcount
            iv_jobname     = iv_jobname
            iv_save        = iv_save.

      WHEN 'CHECK_QUAL'.
        CREATE OBJECT er_check_object TYPE lcl_s4qual_check
          EXPORTING
            it_sel_options = it_sel_options
            iv_obj_source  = iv_obj_source
            iv_dupg        = iv_dupg
            iv_duppadr     = iv_duppadr
            iv_duppnam     = iv_duppnam
            iv_duppustid   = iv_duppustid
            iv_type        = iv_type
            iv_excarc      = iv_excarc
            iv_excdel      = iv_excdel
            iv_uuid        = iv_uuid
            iv_runid       = iv_runid
            iv_timestamp   = iv_timestamp
            iv_jobcount    = iv_jobcount
            iv_jobname     = iv_jobname
            iv_save        = iv_save.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.                    "lcl_cvi_check_factory IMPLEMENTATION

**&---------------------------------------------------------------------*
**&       Class lcl_documents_check
**&---------------------------------------------------------------------*
CLASS lcl_documents_check IMPLEMENTATION.
  " -----------------------------------------------------------------------
  " Method exec_check.
  " -----------------------------------------------------------------------
  METHOD exec_check.

    DATA: lv_objtype TYPE char1,
          lt_hitlist TYPE /qchk/tt_documents_hitlist.

    " Define object type
    lv_objtype = gr_check->gv_obj_source.

    " Execute FI documents check for customer
    IF gr_check->gv_obj_source = gc_objtype_cust.
      exec_customer_doc_checks( EXPORTING iv_archiv_flag = gv_beleg
                                          iv_objtype     = lv_objtype
                                          is_data        = is_data
                                IMPORTING et_hitlist     = lt_hitlist ).
    ENDIF.

    " Execute FI documents check for vendor
    IF gr_check->gv_obj_source = gc_objtype_vend.
      exec_vendor_doc_checks( EXPORTING iv_archiv_flag = gv_beleg
                                        iv_objtype     = lv_objtype
                                        is_data        = is_data
                              IMPORTING et_hitlist     = lt_hitlist ).
    ENDIF.

    " Saving results to global table
    IF lt_hitlist IS NOT INITIAL.
      IF gt_hitlist IS NOT INITIAL.
        APPEND LINES OF lt_hitlist TO gt_hitlist.
      ELSE.
        gt_hitlist = lt_hitlist.
      ENDIF.
    ELSE.
      " RAISE EXCEPTION
    ENDIF.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method exec_initializing.
  " -----------------------------------------------------------------------
  METHOD exec_initializing.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method exec_processing_completion.
  " -----------------------------------------------------------------------
  METHOD exec_processing_completion.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method exec_save
  " -----------------------------------------------------------------------
  METHOD exec_save.

    DATA: lv_counter   TYPE i,
          lv_tabix     TYPE i,

          ls_doc_check TYPE /qchk/docs_use,
          ls_run_stat  TYPE /qchk/st_cvi_run,

          lt_doc_check TYPE /qchk/tt_documents_check_res,
          lt_hitlist   TYPE /qchk/tt_documents_hitlist.

    lt_hitlist = gt_hitlist.

    IF lt_hitlist IS INITIAL.
      RETURN.
    ENDIF.

    "   Save results in DB
    CLEAR lv_counter.
    LOOP AT lt_hitlist ASSIGNING FIELD-SYMBOL(<fs_hitlist>).
      MOVE-CORRESPONDING <fs_hitlist> TO ls_doc_check.
      ls_doc_check-run_id    = gv_runid.
      ls_doc_check-timestamp = gv_timestamp.
      lv_counter = lv_counter + 1.
      ls_doc_check-rec_counter = lv_counter.
      APPEND ls_doc_check TO lt_doc_check.
      lv_tabix = lv_tabix + 1.
      IF lv_tabix = 1000.
        " Save results
        INSERT /qchk/docs_use FROM TABLE lt_doc_check.
        CLEAR lv_tabix.
        REFRESH lt_doc_check.
      ENDIF.
      AT LAST.
        IF lv_tabix > 0.
          INSERT /qchk/docs_use FROM TABLE lt_doc_check.
          COMMIT WORK.
        ENDIF.
      ENDAT.
    ENDLOOP.


    " Update Run info
    ls_run_stat-mandt       = sy-mandt.
    ls_run_stat-run_id      = gv_runid.
    ls_run_stat-job_guid    = gv_uuid.
    ls_run_stat-object_type = gv_obj_source.
    ls_run_stat-jobcount    = gv_jobcount.
    ls_run_stat-jobname     = gv_jobname.
    ls_run_stat-timestamp   = gv_timestamp.
    ls_run_stat-created_by  = sy-uname.

    UPDATE /qchk/check_run FROM ls_run_stat.
    COMMIT WORK AND WAIT.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_all_fi_documents
  " -----------------------------------------------------------------------
  METHOD get_all_fi_documents.
    DATA lt_vbeln_check TYPE /qchk/tt_vbeln_check.

    IF iv_objtype = gc_objtype_c.        " Сustomer
      " Sales-Documents:
      "  Get all the documents from the VBPA.
      SELECT DISTINCT vbeln kunnr                       "#EC CI_NOFIRST
        FROM vbpa
        INTO CORRESPONDING FIELDS OF TABLE lt_vbeln_check
        WHERE kunnr = iv_cvnum.
      IF sy-subrc = 0.
        et_documents = lt_vbeln_check.
      ENDIF.
    ELSEIF iv_objtype = gc_objtype_v.    " Vendor
      SELECT DISTINCT vbeln lifnr                       "#EC CI_NOFIRST
        FROM vbpa
        INTO CORRESPONDING FIELDS OF TABLE lt_vbeln_check
        WHERE lifnr = iv_cvnum.
      IF sy-subrc = 0.
        et_documents = lt_vbeln_check.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method exec_customer_doc_checks
  " -----------------------------------------------------------------------
  METHOD exec_customer_doc_checks.

    DATA: lv_val_text  TYPE val_text,
          lv_domvalue  TYPE domvalue_l,

          ls_cvnum     TYPE /qchk/cvi_cvnum,
          ls_hitlist   TYPE /qchk/st_documents_hitlist,
          ls_comwa     TYPE vbco6,

          "lt_docflow   TYPE tdt_docflow,
          "lt_doc_sort  TYPE tdt_docflow,

          lt_doc_flow  TYPE STANDARD TABLE OF vbfa,
          lt_flow_sort TYPE STANDARD TABLE OF vbfa,

          lt_hitlist   TYPE /qchk/tt_documents_hitlist,
          lt_t077d     TYPE STANDARD TABLE OF t077d,
          lt_t077x     TYPE STANDARD TABLE OF t077x,
          lt_documents TYPE /qchk/tt_vbeln_check,
          lt_bsid      TYPE TABLE OF bsid,
          lt_bsad      TYPE TABLE OF bsad,
          lt_data_c    TYPE TABLE OF cmds_ei_extern. "<> is_data TYPE cmds_ei_main.

    FIELD-SYMBOLS: <fs_cmds_ei_main>   TYPE any,
                   <fs_cmds_ei_extern> TYPE STANDARD TABLE.

    " Get debitor conto group, number range.
    SELECT * FROM t077d INTO TABLE lt_t077d.
    " Get  debitor conto group descritpion.
    SELECT * FROM t077x INTO TABLE lt_t077x.

    " Getting an internal table from a nested structure
    ASSIGN is_data->* TO <fs_cmds_ei_main>.
    ASSIGN COMPONENT 'CUSTOMERS' OF STRUCTURE <fs_cmds_ei_main> TO <fs_cmds_ei_extern>.
    lt_data_c = <fs_cmds_ei_extern>.

    IF lt_data_c IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_data_c BY header ASCENDING.

    LOOP AT lt_data_c ASSIGNING FIELD-SYMBOL(<fs_data_c>).

      CLEAR: lt_documents,
        lt_doc_flow,
        lt_flow_sort.
      " lt_docflow,
      " lt_doc_sort.

      " Get all documents
      get_all_fi_documents( EXPORTING iv_objtype   = iv_objtype                                  " Einstelliges Kennzeichen
                                      iv_cvnum     = <fs_data_c>-header-object_instance-kunnr    " Customer Vendor number range structure
                            IMPORTING et_documents = lt_documents   ).                            " Table type for receipts

      " Document flow check
      IF lt_documents IS INITIAL.
        CONTINUE.
      ENDIF.

      TRY.
          " Get documents Document flow
          LOOP AT lt_documents ASSIGNING FIELD-SYMBOL(<fs_doc>).
            CLEAR: lt_doc_flow.
            ls_comwa-mandt = sy-mandt.
            ls_comwa-vbeln = <fs_doc>-vbeln.

            CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
              EXPORTING
                comwa         = ls_comwa                 " Ausgangsbeleg für Flussverarbeitung
              TABLES
                vbfa_tab      = lt_doc_flow                " Belegflussinformationen
              EXCEPTIONS
                no_vbfa       = 1
                no_vbuk_found = 2
                OTHERS        = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            IF lt_doc_flow IS NOT INITIAL.
              SORT lt_doc_flow BY erdat DESCENDING
                                 erzet DESCENDING.
              " Get the last document from flow
              READ TABLE lt_doc_flow ASSIGNING FIELD-SYMBOL(<fs_docflow>) INDEX 1.
              IF sy-subrc = 0.
                APPEND <fs_docflow> TO lt_flow_sort.
              ENDIF.
            ENDIF.
          ENDLOOP.

        CATCH cx_root.

      ENDTRY.

      " Processing of relevant documents
      SORT lt_flow_sort BY erdat DESCENDING
                           erzet DESCENDING.

      READ TABLE lt_flow_sort ASSIGNING FIELD-SYMBOL(<fs_doc_sort>) INDEX 1.
      IF sy-subrc = 0.
        ls_hitlist-cvi_cvnum      = <fs_data_c>-header-object_instance-kunnr.
        ls_hitlist-last_use_date  = <fs_doc_sort>-erdat.
        ls_hitlist-account_group  = <fs_data_c>-central_data-central-data-ktokd.
        ls_hitlist-cv_link        =  |{ <fs_data_c>-central_data-central-data-lifnr ALPHA = OUT }|. " <fs_data_c>-central_data-central-data-lifnr.
        IF <fs_doc_sort>-vbelv IS NOT INITIAL.
          ls_hitlist-vbeln          = <fs_doc_sort>-vbelv.
        ELSE.
          ls_hitlist-vbeln          = <fs_doc_sort>-vbeln.
        ENDIF.
        IF <fs_doc_sort>-vbtyp_n IS NOT INITIAL.
          lv_domvalue = <fs_doc_sort>-vbtyp_n.
          " Get document type description
          CALL FUNCTION 'RV_DOMAIN_VALUE_TEXTS'
            EXPORTING
              domname  = 'VBTYP'
              domvalue = lv_domvalue
              single   = ' '
            IMPORTING
              ddtext   = lv_val_text.
          IF <fs_doc_sort>-vbelv IS NOT INITIAL.
            ls_hitlist-last_use_descr = lv_val_text && ` ` && <fs_doc_sort>-vbelv.
          ELSE.
            ls_hitlist-last_use_descr = lv_val_text && ` ` && <fs_doc_sort>-vbeln.
          ENDIF.
        ENDIF.

        " Account group descritpion
        READ TABLE lt_t077x ASSIGNING FIELD-SYMBOL(<fs_t077x>) WITH KEY ktokd = <fs_data_c>-central_data-central-data-ktokd spras = sy-langu.
        IF sy-subrc = 0.
          ls_hitlist-account_descr = <fs_t077x>-txt30.
        ELSEIF sy-subrc <> 0.
          " Get text in english
          READ TABLE lt_t077x ASSIGNING <fs_t077x> WITH KEY ktokd = <fs_data_c>-central_data-central-data-ktokd spras = 'E'.
          IF sy-subrc = 0.
            ls_hitlist-account_descr = <fs_t077x>-txt30.
          ENDIF.
        ENDIF.
        " Number range
        READ TABLE lt_t077d ASSIGNING FIELD-SYMBOL(<fs_t077d>) WITH KEY ktokd = <fs_data_c>-central_data-central-data-ktokd.
        IF sy-subrc = 0.
          ls_hitlist-numkr = <fs_t077d>-numkr.
        ENDIF.

        " Deletion note central
        ls_hitlist-loevm_central = <fs_data_c>-central_data-central-data-loevm.
        " Deletion note central bukrs
        LOOP AT <fs_data_c>-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company>).
          IF <fs_company>-data-loevm IS NOT INITIAL.
            ls_hitlist-loevm_bukrs = <fs_company>-data-loevm.
          ENDIF.
        ENDLOOP.
        " Deletion note sales
        LOOP AT <fs_data_c>-sales_data-sales ASSIGNING FIELD-SYMBOL(<fs_sales>).
          IF <fs_sales>-data-loevm IS NOT INITIAL.
            ls_hitlist-loevm_sales = <fs_sales>-data-loevm.
          ENDIF.
        ENDLOOP.

        " Fill result table.
        APPEND ls_hitlist TO lt_hitlist.
        SORT lt_hitlist BY vbeln ASCENDING.

      ENDIF.

    ENDLOOP.

    " Check FI archiv documents
    IF iv_archiv_flag IS NOT INITIAL.
      " Get archiv docs from db
      SELECT *
        FROM bsid
        INTO CORRESPONDING FIELDS OF TABLE lt_bsid
        WHERE xarch = iv_archiv_flag.

      SORT lt_bsid BY belnr ASCENDING.

      SELECT *
          FROM bsad
          INTO CORRESPONDING FIELDS OF TABLE lt_bsad
          WHERE xarch = iv_archiv_flag.

      SORT lt_bsad BY belnr ASCENDING.

      " Delete archiv documents from result list
      LOOP AT lt_hitlist ASSIGNING FIELD-SYMBOL(<fs_hitlist>).
        READ TABLE lt_bsid TRANSPORTING NO FIELDS WITH KEY belnr = <fs_hitlist>-vbeln.
        IF sy-subrc = 0.
          DELETE lt_hitlist WHERE vbeln = <fs_hitlist>-vbeln.          " FROM <fs_hitlist>.
        ENDIF.
        READ TABLE lt_bsad TRANSPORTING NO FIELDS WITH KEY belnr = <fs_hitlist>-vbeln.
        IF sy-subrc = 0.
          DELETE lt_hitlist WHERE vbeln = <fs_hitlist>-vbeln.          " FROM <fs_hitlist>.
        ENDIF.
      ENDLOOP.

    ENDIF.

    SORT lt_hitlist BY cvi_cvnum ASCENDING.
    " Return result
    et_hitlist = lt_hitlist.


  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method exec_vendor_doc_checks
  " -----------------------------------------------------------------------
  METHOD exec_vendor_doc_checks.

    DATA: lv_val_text  TYPE val_text,
          lv_domvalue  TYPE domvalue_l,

          ls_cvnum     TYPE /qchk/cvi_cvnum,
          ls_hitlist   TYPE /qchk/st_documents_hitlist,
          ls_comwa     TYPE vbco6,

          lt_doc_flow  TYPE STANDARD TABLE OF vbfa,
          lt_flow_sort TYPE STANDARD TABLE OF vbfa,
          lt_hitlist   TYPE /qchk/tt_documents_hitlist,
          lt_t077k     TYPE STANDARD TABLE OF t077k,
          lt_t077y     TYPE STANDARD TABLE OF t077y,
          lt_documents TYPE /qchk/tt_vbeln_check,
          lt_bsik      TYPE TABLE OF bsik,
          lt_bsak      TYPE TABLE OF bsak,
          lt_data_v    TYPE TABLE OF vmds_ei_extern. " <> vmds_ei_main.

    FIELD-SYMBOLS: <fs_vmds_ei_main>   TYPE any,
                   <fs_vmds_ei_extern> TYPE STANDARD TABLE.

    " Get creditor conto group, number range.
    SELECT * FROM t077k INTO TABLE lt_t077k.
    " Get creditor conto group descritpion.
    SELECT * FROM t077y INTO TABLE lt_t077y.

    " Getting an internal table from a nested structure
    ASSIGN is_data->* TO <fs_vmds_ei_main>.
    ASSIGN COMPONENT 'VENDORS' OF STRUCTURE <fs_vmds_ei_main> TO <fs_vmds_ei_extern>.
    lt_data_v = <fs_vmds_ei_extern>.

    IF lt_data_v IS INITIAL.
      RETURN.
    ENDIF.

    SORT lt_data_v BY header ASCENDING.

    LOOP AT lt_data_v ASSIGNING FIELD-SYMBOL(<fs_data_v>).

      CLEAR: lt_documents,
        lt_doc_flow,
        lt_flow_sort.

      " Get all documents
      get_all_fi_documents( EXPORTING iv_objtype   = iv_objtype  " iv_objtype                                        " Einstelliges Kennzeichen
                                      iv_cvnum     = <fs_data_v>-header-object_instance-lifnr                       " Customer Vendor number range structure
                            IMPORTING et_documents = lt_documents   ).                                               " Table type for receipts

      " Document flow check
      IF lt_documents IS INITIAL.
        CONTINUE.
      ENDIF.

      TRY.
          " Get documents Document flow
          LOOP AT lt_documents ASSIGNING FIELD-SYMBOL(<fs_doc>).
            CLEAR: lt_doc_flow.
            ls_comwa-mandt = sy-mandt.
            ls_comwa-vbeln = <fs_doc>-vbeln.

            CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
              EXPORTING
                comwa         = ls_comwa                 " Ausgangsbeleg für Flussverarbeitung
              TABLES
                vbfa_tab      = lt_doc_flow                " Belegflussinformationen
              EXCEPTIONS
                no_vbfa       = 1
                no_vbuk_found = 2
                OTHERS        = 3.
            IF sy-subrc <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            IF lt_doc_flow IS NOT INITIAL.
              SORT lt_doc_flow BY erdat DESCENDING
                                 erzet DESCENDING.
              " Get the last document from flow
              READ TABLE lt_doc_flow ASSIGNING FIELD-SYMBOL(<fs_docflow>) INDEX 1.
              IF sy-subrc = 0.
                APPEND <fs_docflow> TO lt_flow_sort.
              ENDIF.
            ENDIF.
          ENDLOOP.

        CATCH cx_root.

      ENDTRY.

      " Processing of relevant documents
      SORT lt_flow_sort BY erdat DESCENDING
                          erzet DESCENDING.

      READ TABLE lt_flow_sort ASSIGNING FIELD-SYMBOL(<fs_doc_sort>) INDEX 1.
      IF sy-subrc = 0.
        ls_hitlist-cvi_cvnum      = <fs_data_v>-header-object_instance-lifnr.
        ls_hitlist-last_use_date  = <fs_doc_sort>-erdat.
        ls_hitlist-account_group  = <fs_data_v>-central_data-central-data-ktokk.
        ls_hitlist-cv_link        =  |{ <fs_data_v>-central_data-central-data-kunnr ALPHA = OUT }|.   "  <fs_data_v>-central_data-central-data-kunnr.
        IF <fs_doc_sort>-vbelv IS NOT INITIAL.
          ls_hitlist-vbeln          = <fs_doc_sort>-vbelv.
        ELSE.
          ls_hitlist-vbeln          = <fs_doc_sort>-vbeln.
        ENDIF.
        IF <fs_doc_sort>-vbtyp_n IS NOT INITIAL.
          lv_domvalue = <fs_doc_sort>-vbtyp_n.
          " Get document type description
          CALL FUNCTION 'RV_DOMAIN_VALUE_TEXTS'
            EXPORTING
              domname  = 'VBTYP'
              domvalue = lv_domvalue
              single   = ' '
            IMPORTING
              ddtext   = lv_val_text.
          IF <fs_doc_sort>-vbelv IS NOT INITIAL.
            ls_hitlist-last_use_descr = lv_val_text && ` ` && <fs_doc_sort>-vbelv.
          ELSE.
            ls_hitlist-last_use_descr = lv_val_text && ` ` && <fs_doc_sort>-vbeln.
          ENDIF.
        ENDIF.
        " Account group descritpion
        READ TABLE lt_t077y ASSIGNING FIELD-SYMBOL(<fs_t077y>) WITH KEY ktokk = <fs_data_v>-central_data-central-data-ktokk spras = sy-langu.
        IF sy-subrc = 0.
          ls_hitlist-account_descr = <fs_t077y>-txt30.
        ELSEIF sy-subrc <> 0.
          " get text in english
          READ TABLE lt_t077y ASSIGNING <fs_t077y> WITH KEY ktokk = <fs_data_v>-central_data-central-data-ktokk spras = 'E'.
          ls_hitlist-account_descr = <fs_t077y>-txt30.
        ENDIF.
        " Number range
        READ TABLE lt_t077k ASSIGNING FIELD-SYMBOL(<fs_t077k>) WITH KEY ktokd = <fs_data_v>-central_data-central-data-ktokk.
        IF sy-subrc = 0.
          ls_hitlist-numkr = <fs_t077k>-numkr.
        ENDIF.
        " Deletion note
        ls_hitlist-loevm_central = <fs_data_v>-central_data-central-data-loevm.
        LOOP AT <fs_data_v>-company_data-company ASSIGNING FIELD-SYMBOL(<fs_company>).
          IF <fs_company>-data-loevm IS NOT INITIAL.
            ls_hitlist-loevm_bukrs = <fs_company>-data-loevm.
          ENDIF.
        ENDLOOP.
        " Deletion note sales
        LOOP AT <fs_data_v>-purchasing_data-purchasing ASSIGNING FIELD-SYMBOL(<fs_purchasing>).
          IF <fs_purchasing>-data-loevm IS NOT INITIAL.
            ls_hitlist-loevm_sales = <fs_purchasing>-data-loevm.
          ENDIF.
        ENDLOOP.
        " Fill result table.
        APPEND ls_hitlist TO lt_hitlist.
      ENDIF.

    ENDLOOP.

    " Check FI archiv documents
    IF iv_archiv_flag IS NOT INITIAL.
      " get archiv docs from db
      SELECT *
        FROM bsik
        INTO CORRESPONDING FIELDS OF TABLE lt_bsik
        WHERE xarch = iv_archiv_flag.

      SORT lt_bsik BY belnr ASCENDING.

      SELECT *
          FROM bsak
          INTO CORRESPONDING FIELDS OF TABLE lt_bsak
          WHERE xarch = iv_archiv_flag.

      SORT lt_bsak BY belnr ASCENDING.

      " Delete archiv documents from result list
      LOOP AT lt_hitlist ASSIGNING FIELD-SYMBOL(<fs_hitlist>).
        READ TABLE lt_bsik TRANSPORTING NO FIELDS WITH KEY belnr = <fs_hitlist>-vbeln.
        IF sy-subrc = 0.
          DELETE lt_hitlist WHERE vbeln = <fs_hitlist>-vbeln.          " FROM <fs_hitlist>.
        ENDIF.
        READ TABLE lt_bsak TRANSPORTING NO FIELDS WITH KEY belnr = <fs_hitlist>-vbeln.
        IF sy-subrc = 0.
          DELETE lt_hitlist WHERE vbeln = <fs_hitlist>-vbeln.          " FROM <fs_hitlist>.
        ENDIF.
      ENDLOOP.

    ENDIF.

    SORT lt_hitlist BY cvi_cvnum ASCENDING.
    " return result
    et_hitlist = lt_hitlist.

  ENDMETHOD.

ENDCLASS.
"---------------------------------------------------------------------------------------------

**&---------------------------------------------------------------------*
**&       Class lcl_cvi_service_proc
**&---------------------------------------------------------------------*
CLASS lcl_cvi_service_proc IMPLEMENTATION.


  " -----------------------------------------------------------------------
  " Method def_master_data_type.
  " -----------------------------------------------------------------------
  METHOD def_master_data_type.
    " parameters transfer
    gv_dupg       = p_dupg.
    gv_duppadr    = c_dupadr.
    gv_duppnam    = c_dupnam.
    gv_duppustid  = c_dupst.
    gv_ucomm      = p_ucomm.
    gv_uuid       = p_uuid.
    gv_timestamp  = p_timest.
    gv_jobcount   = p_jobcou.
    gv_jobname    = p_jobnam.
    gv_loevm      = c_loevm.
    gv_beleg      = c_beleg.
    gv_check_type = p_chtype.
    gv_runid      = p_run_id.

    " gv_excarc    = p_excarc.
    " gv_excdel    = p_excdel.
    " gv_fremd     = p_fremd.
    " gv_save      = p_save.

    " Define master data type
    CASE abap_true.
      WHEN p_kunst.
        ev_obj_source = gc_objtype_cust.
        ev_check_type = gv_check_type.
      WHEN p_lifnst.
        gv_obj_source = gc_objtype_vend.
        ev_check_type = gv_check_type.
    ENDCASE.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method def_sel_options.
  " -----------------------------------------------------------------------
  METHOD def_sel_options.
    DATA: ls_rsselect TYPE rsselect,
          ls_kunnr    LIKE LINE OF so_kunnr,
          ls_lifnr    LIKE LINE OF so_lifnr.

    CLEAR:
    gs_sel_param-source,
    gs_sel_param-data,
    gs_sel_param-sel_criteria.
    REFRESH gs_sel_param-object_list.

    CASE abap_true.
      WHEN p_kunst.
        LOOP AT so_kunnr INTO ls_kunnr.
          ls_rsselect-fieldnm = 'KUNNR'.
          MOVE-CORRESPONDING ls_kunnr TO ls_rsselect.
          APPEND ls_rsselect TO gs_sel_param-sel_criteria-sel_options.
        ENDLOOP.
      WHEN p_lifnst.
        LOOP AT so_lifnr INTO ls_lifnr.
          ls_rsselect-fieldnm = 'LIFNR'.
          MOVE-CORRESPONDING ls_lifnr TO ls_rsselect.
          APPEND ls_rsselect TO gs_sel_param-sel_criteria-sel_options.
        ENDLOOP.
    ENDCASE.

    CLEAR ls_rsselect.

    CASE abap_true.
      WHEN c_loevm.
        ls_rsselect-fieldnm = 'LOEVM'.
        ls_rsselect-sign    = 'E'.
        ls_rsselect-option  = 'EQ'.
        ls_rsselect-low     = abap_true.
        APPEND ls_rsselect TO gs_sel_param-sel_criteria-sel_options.

    ENDCASE.
  ENDMETHOD.


ENDCLASS.


**&---------------------------------------------------------------------*
**&       Class lcl_s4qual_check
**&---------------------------------------------------------------------*
CLASS lcl_s4qual_check IMPLEMENTATION.
  " -----------------------------------------------------------------------
  " Method exec_check.
  " -----------------------------------------------------------------------
  METHOD exec_check.

    DATA: lv_objtype   TYPE char1,
          ls_cust_data TYPE cmds_ei_main,
          ls_vend_data TYPE vmds_ei_main,
          lt_customers TYPE cmds_ei_extern_t,
          lt_vendors   TYPE vmds_ei_extern_t.

    FIELD-SYMBOLS <fs_data> TYPE any.

    " Define object type
    lv_objtype = gr_check->gv_obj_source.

    " cast structure to field-symbols
    ASSIGN is_data->* TO <fs_data>.

    IF gr_check->gv_obj_source = gc_objtype_cust.
      " cast is_data to customer table
      ls_cust_data = <fs_data>.
      REFRESH lt_customers.
      APPEND LINES OF ls_cust_data-customers TO lt_customers.
    ELSE.
      " cast is_data to vendor table
      ls_vend_data = <fs_data>.
      REFRESH lt_vendors.
      APPEND LINES OF ls_vend_data-vendors TO lt_vendors.
    ENDIF.

    " execute consistency check
    consistency_check( it_customers = lt_customers
                       it_vendors   = lt_vendors ).
  ENDMETHOD.

  METHOD exec_processing_completion.
  ENDMETHOD.

  METHOD exec_save.
    DATA: lv_tabix      TYPE i,
          lv_cnt        TYPE i,

          ls_qual_check TYPE /qchk/s4_quality,
          ls_run_stat   TYPE /qchk/st_cvi_run,

          lt_qual_check TYPE TABLE OF /qchk/s4_quality.

    FIELD-SYMBOLS <fs_qual_check_result> LIKE LINE OF gt_qual_check_result.

    "  SORT gt_check_result_bp BY sync_obj_sour_id.
    SORT gt_qual_check_result BY sync_obj_sour_id.


    IF gt_qual_check_result IS INITIAL.
      " Cancel the job
*      CALL FUNCTION 'BP_JOB_ABORT'
*        EXPORTING
*          jobcount = gv_jobcount                 " Job count
*          jobname  = gv_jobname.                 " Jobname
      RETURN.
    ENDIF.

    "   Save results in DB
    LOOP AT gt_qual_check_result ASSIGNING <fs_qual_check_result>.
      MOVE-CORRESPONDING <fs_qual_check_result> TO ls_qual_check ##ENH_OK.
      ls_qual_check-timestamp = gv_timestamp.
      ls_qual_check-unique_id = gv_uuid.
      ls_qual_check-run_id    = gv_runid.
      lv_cnt = lv_cnt + 1.
      ls_qual_check-msg_counter = lv_cnt.
      APPEND ls_qual_check TO lt_qual_check.
      lv_tabix = lv_tabix + 1.
      IF lv_tabix = 1000.
        INSERT /qchk/s4_quality FROM TABLE lt_qual_check.
        COMMIT WORK.
        CLEAR lv_tabix.
        REFRESH lt_qual_check.
      ENDIF.
      AT LAST.
        IF lv_tabix > 0.
          INSERT /qchk/s4_quality FROM TABLE lt_qual_check.
          COMMIT WORK.
        ENDIF.
      ENDAT.
    ENDLOOP.

    " save run id
    ls_run_stat-mandt       = sy-mandt.
    ls_run_stat-run_id      = gv_runid.
    ls_run_stat-job_guid    = gv_uuid.
    ls_run_stat-object_type = gv_obj_source.
    ls_run_stat-jobcount    = gv_jobcount.
    ls_run_stat-jobname     = gv_jobname.
    ls_run_stat-timestamp   = gv_timestamp.
    ls_run_stat-created_by  = sy-uname.

    CASE gv_obj_source.
      WHEN gc_objtype_cust.
        ls_run_stat-object_type = gc_objtype_c.
      WHEN OTHERS. " VENDOR
        ls_run_stat-object_type = gc_objtype_v.
    ENDCASE.

    UPDATE /qchk/check_run FROM ls_run_stat.
    COMMIT WORK.
  ENDMETHOD.                    " lcl_object_processing~save

  METHOD exec_initializing.
    initialize_customizing_data( ).
  ENDMETHOD.                    " lcl_object_processing~initializing

  METHOD consistency_check.
    DATA: lv_connected_bp TYPE /qchk/cvi_obj_source_id,
          lv_partner      TYPE bu_partner,
          lt_customers    TYPE cmds_ei_extern_t,
          lt_vendors      TYPE vmds_ei_extern_t,
          lt_errors       TYPE bapiret2_t.

    FIELD-SYMBOLS <ls_customer> TYPE cmds_ei_extern.
    FIELD-SYMBOLS <ls_vendor>   TYPE vmds_ei_extern.

    lt_customers[] = it_customers[].
    lt_vendors[]   = it_vendors[].

    LOOP AT lt_customers ASSIGNING <ls_customer>.

      " Prüfung ob für einen Debitor bereits ein Partner existiert
      get_partner_customer( EXPORTING iv_kunnr   = <ls_customer>-header-object_instance-kunnr
                            IMPORTING ev_partner = lv_partner ).

      " Überprüfung der Adresse
      check_address( EXPORTING is_address_data = <ls_customer>-central_data-address-postal-data
                     IMPORTING et_errors       = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Überprüfung Email-Adresse
      check_email( EXPORTING it_smtp   = <ls_customer>-central_data-address-communication-smtp-smtp[]
                   IMPORTING et_errors = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Nummernkreisüberprüfung
      check_number_range( EXPORTING iv_nrobj         = gc_debitor
                                    iv_objnr         = <ls_customer>-header-object_instance-kunnr
                                    iv_account_group = <ls_customer>-central_data-central-data-ktokd
                          IMPORTING et_errors        = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Überprüfung Bankdaten
      check_bank_data( EXPORTING iv_nrobj       = gc_debitor
                                 it_bankdetails = <ls_customer>-central_data-bankdetail-bankdetails
                       IMPORTING et_errors      = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Überprüfung Steuer-Daten
      check_tax_type( EXPORTING iv_nrobj                 = gc_debitor
                                it_eu_vat_nums           = <ls_customer>-central_data-vat_number-vat_numbers
                                is_customer_central_data = <ls_customer>-central_data-central-data
                                is_address_data          = <ls_customer>-central_data-address-postal-data
                      IMPORTING et_errors                = lt_errors ).

      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Überprüfung Steuer-Standort
      check_tax_jur( EXPORTING iv_nrobj        = gc_debitor
                               is_address_data = <ls_customer>-central_data-address-postal-data
                     IMPORTING et_errors       = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Prüfung von Postleitzahlformat, Regionalcode und Länderschlüssel
      check_postcode( EXPORTING is_address_data = <ls_customer>-central_data-address-postal-data
                      IMPORTING et_errors       = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Prüfung der Transportzone
      check_transzone( EXPORTING iv_nrobj        = gc_debitor
                                 is_address_data = <ls_customer>-central_data-address-postal-data
                                 iv_ktokd        = <ls_customer>-central_data-central-data-ktokd
                       IMPORTING et_errors       = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Prüfung der Branche
      check_industry( EXPORTING iv_brsch  = <ls_customer>-central_data-central-data-brsch
                      IMPORTING et_errors = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      "
      "     R1 367 Konsistenzfehler => IF_EX_CVI_CUSTOM_MAPPER~MAP_CUSTOMER_TO_BP
      "     wird gesetzt bei
      "     - Inconsistent postal code
      "     - Inconsistent bank data                      - ist hier schon geprüft
      "     - Inconsistent data between customer/vendor
      "       - Check whether the customer is assigned to a vendor

**      IF sy-subrc = 0.
*        check_r1_367_map_custom_to_bp( EXPORTING is_customer = <ls_customer>
*                                       IMPORTING et_errors   = lt_errors
*                                                 ev_bp       = lv_connected_bp ).
*
*        " Fehlerverarbeitung
*        IF lt_errors IS NOT INITIAL.
*          fill_check_result_bp_from_ret2( iv_objnr   = <ls_customer>-header-object_instance-kunnr
*                                          iv_bp      = lv_connected_bp
*                                          iv_partner = lv_partner
*                                          it_errors  = lt_errors ).
*        ENDIF.
*      ENDIF.
*****


    ENDLOOP.

    LOOP AT lt_vendors ASSIGNING <ls_vendor>.


      " Prüfung ob für einen Debitor bereits ein Partner existiert
      get_partner_vendor( EXPORTING iv_lifnr   = <ls_vendor>-header-object_instance-lifnr
                          IMPORTING ev_partner = lv_partner ).

      " Überprüfung der Adresse
      check_address( EXPORTING is_address_data = <ls_vendor>-central_data-address-postal-data
                     IMPORTING et_errors       = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Überprüfung Email-Adresse
      check_email( EXPORTING it_smtp   = <ls_vendor>-central_data-address-communication-smtp-smtp[]
                   IMPORTING et_errors = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Nummernkreisüberprüfung
      check_number_range( EXPORTING iv_nrobj         = gc_kreditor
                                    iv_objnr         = <ls_vendor>-header-object_instance-lifnr
                                    iv_account_group = <ls_vendor>-central_data-central-data-ktokk
                          IMPORTING et_errors        = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Überprüfung Bankdaten
      check_bank_data( EXPORTING iv_nrobj       = gc_kreditor
                                 it_bankdetails = <ls_vendor>-central_data-bankdetail-bankdetails
                       IMPORTING et_errors      = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Überprüfung Steuer-Daten
      check_tax_type( EXPORTING iv_nrobj               = gc_kreditor
                                it_eu_vat_nums         = <ls_vendor>-central_data-vat_number-vat_numbers
                                is_vendor_central_data = <ls_vendor>-central_data-central-data
                                is_address_data        = <ls_vendor>-central_data-address-postal-data
                      IMPORTING et_errors              = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Überprüfung Steuer-Standort
      check_tax_jur( EXPORTING iv_nrobj        = gc_kreditor
                               is_address_data = <ls_vendor>-central_data-address-postal-data
                     IMPORTING et_errors       = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Prüfung von Postleitzahlformat, Regionalcode und Länderschlüssel
      check_postcode( EXPORTING is_address_data = <ls_vendor>-central_data-address-postal-data
                      IMPORTING et_errors       = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Prüfung der Transportzone
      check_transzone( EXPORTING iv_nrobj        = gc_kreditor
                                 is_address_data = <ls_vendor>-central_data-address-postal-data
                                 iv_ktokk        = <ls_vendor>-central_data-central-data-ktokk
                       IMPORTING et_errors       = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

      " Prüfung der Branche
      check_industry( EXPORTING iv_brsch  = <ls_vendor>-central_data-central-data-brsch
                      IMPORTING et_errors = lt_errors ).
      " Fehlerverarbeitung
      IF lt_errors IS NOT INITIAL.
        fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
                                        iv_partner = lv_partner
                                        it_errors  = lt_errors ).
      ENDIF.

**     "R1_367
*      IF sy-subrc = 0.
*        check_r1_367_map_vendor_to_bp( EXPORTING is_vendor = <ls_vendor>
*                                       IMPORTING et_errors = lt_errors
*                                                 ev_bp     = lv_connected_bp ).
*
*        " Fehlerverarbeitung
*        IF lt_errors IS NOT INITIAL.
*          fill_check_result_bp_from_ret2( iv_objnr   = <ls_vendor>-header-object_instance-lifnr
*                                          iv_bp      = lv_connected_bp
*                                          iv_partner = lv_partner
*                                          it_errors  = lt_errors ).
*        ENDIF.
*      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    " consistency_check

  METHOD check_r1_367_map_vendor_to_bp.

    DATA: lv_kunnr           TYPE kunnr,
          lv_message         TYPE bapi_msg,

          ls_customer        TYPE cmds_ei_extern,
          ls_customer_read   TYPE cmds_ei_main,
          ls_customer_detail TYPE cmds_ei_extern,
          ls_cust_bankdetail TYPE cvis_ei_cvi_bankdetail,
          ls_vend_bankdetail TYPE cvis_ei_cvi_bankdetail,
          ls_errors          TYPE cvis_error,
          ls_error           TYPE bapiret2,

          lt_customer        TYPE cmds_ei_main.

    DATA lr_struct TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS: <fs_vend_comp>  TYPE any,
                   <fs_vend_struc> TYPE any,
                   <fs_cust_comp>  TYPE any,
                   <fs_cust_struc> TYPE any,
                   <fs_col>        TYPE abap_compdescr.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = is_vendor-central_data-central-data-kunnr
      IMPORTING
        output = lv_kunnr
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc <> 0.
      lv_kunnr = is_vendor-central_data-central-data-kunnr.
    ENDIF.

    " wurde kein verbundener Geschäftspartner gefunden soll die Methode verlassen werden
    IF lv_kunnr = space.
      RETURN.
    ENDIF.

    " Display of customer/vendor number in case of error
    ev_bp = lv_kunnr.

    " Read Data
    ls_customer-header-object_instance-kunnr = lv_kunnr.
    ls_customer-header-object_task = 'M'.
    APPEND ls_customer TO lt_customer-customers.

    " Read Customer Data
    cmd_ei_api_extract=>get_data( EXPORTING is_master_data = lt_customer
                                  IMPORTING es_master_data = ls_customer_read
                                            es_error       = ls_errors ).

    IF ls_errors IS NOT INITIAL.
      APPEND LINES OF ls_errors-messages TO et_errors.
      IF ls_errors-is_error = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF     ls_customer_read-customers             IS INITIAL
       AND is_vendor-header-object_instance-lifnr IS NOT INITIAL.

      " Inconsistent data between customer/vendor -Exit
      MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 028
              WITH lv_kunnr is_vendor-header-object_instance-lifnr
              INTO lv_message.

      ls_error-id        = sy-msgid.
      ls_error-type      = sy-msgty.
      ls_error-number    = sy-msgno.
      ls_error-message   = lv_message.
      ls_error-parameter = 'Verknüpfung Vendor <=> Customer'(036).

      APPEND ls_error TO et_errors.
      RETURN.

    ENDIF.

    READ TABLE ls_customer_read-customers INTO ls_customer_detail
         WITH KEY header-object_instance-kunnr = lv_kunnr.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Check postal data from customer and vendor are the same
    "       -----------
    IF ls_customer_detail-central_data-address-postal-data <> is_vendor-central_data-address-postal-data.

      ASSIGN is_vendor-central_data-address-postal-data TO <fs_vend_struc>.
      ASSIGN ls_customer_detail-central_data-address-postal-data TO <fs_cust_struc>.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_vend_struc> TO <fs_vend_comp>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_cust_struc> TO <fs_cust_comp>.

        IF <fs_vend_comp> = <fs_cust_comp>.
          CONTINUE.
        ENDIF.

        lr_struct ?= cl_abap_structdescr=>describe_by_data( p_data = <fs_vend_struc> ).

        READ TABLE lr_struct->components[] INDEX sy-index ASSIGNING <fs_col>.
        IF sy-subrc = 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 035
                  WITH <fs_col>-name <fs_vend_comp> <fs_cust_comp>
                  INTO lv_message.

          ls_error-id        = sy-msgid.
          ls_error-type      = sy-msgty.
          ls_error-number    = sy-msgno.
          ls_error-message   = lv_message.
          ls_error-parameter = 'Adresse'(013).

          APPEND ls_error TO et_errors.
        ENDIF.

      ENDDO.

**       MESSAGE e367(r1) INTO lv_message WITH is_vendor-header-object_instance-lifnr.
*        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 026
*         WITH is_vendor-header-object_instance-lifnr lv_kunnr
*         INTO lv_message.
*
*        ls_error-id      = sy-msgid.
*        ls_error-type    = sy-msgty.
*        ls_error-number  = sy-msgno.
*        ls_error-message = lv_message.
*        ls_error-parameter = 'Adresse'(013).
*
*        APPEND ls_error TO et_errors.
*
***      Inconsistent postal code data - Exit
*        RETURN.

    ENDIF.

    UNASSIGN: <fs_vend_comp>, <fs_vend_struc>, <fs_cust_comp>, <fs_cust_struc>.

    " Check Bank Data
    LOOP AT is_vendor-central_data-bankdetail-bankdetails INTO ls_vend_bankdetail.
      READ TABLE ls_customer_detail-central_data-bankdetail-bankdetails INTO ls_cust_bankdetail
           WITH KEY data_key-banks = ls_vend_bankdetail-data_key-banks
                    data_key-bankl = ls_vend_bankdetail-data_key-bankl
                    data_key-bankn = ls_vend_bankdetail-data_key-bankn.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF ls_vend_bankdetail-data = ls_cust_bankdetail-data.
        CONTINUE.
      ENDIF.

      ASSIGN ls_vend_bankdetail-data TO <fs_vend_struc>.
      ASSIGN ls_cust_bankdetail-data TO <fs_cust_struc>.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_vend_struc> TO <fs_vend_comp>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_cust_struc> TO <fs_cust_comp>.

        IF <fs_vend_comp> = <fs_cust_comp>.
          CONTINUE.
        ENDIF.

        lr_struct ?= cl_abap_structdescr=>describe_by_data( p_data = <fs_vend_struc> ).

        READ TABLE lr_struct->components[] INDEX sy-index ASSIGNING <fs_col>.
        IF sy-subrc = 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 035
                  WITH <fs_col>-name <fs_vend_comp> <fs_cust_comp>
                  INTO lv_message.

          ls_error-id        = sy-msgid.
          ls_error-type      = sy-msgty.
          ls_error-number    = sy-msgno.
          ls_error-message   = lv_message.
          ls_error-parameter = 'Adresse'(013).

          APPEND ls_error TO et_errors.
        ENDIF.

      ENDDO.

*            MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 027
*             WITH is_vendor-header-object_instance-lifnr lv_kunnr
*             INTO lv_message.
*
*            ls_error-id        = sy-msgid.
*            ls_error-type      = sy-msgty.
*            ls_error-number    = sy-msgno.
*            ls_error-message   = lv_message.
*            ls_error-parameter = 'Bank'(027).
*
*            APPEND ls_error TO et_errors.
*
**           Inconsistent bank data - Exit
*            RETURN
      .
    ENDLOOP.
  ENDMETHOD.                    " check_r1_367_MAP_VENDOR_TO_BP

  METHOD check_r1_367_map_custom_to_bp.
    DATA: lv_lifnr           TYPE lifnr,
*          lv_lifnr_initial   TYPE lifnr,
          ls_vendor          TYPE vmds_ei_extern,
          lt_vendor          TYPE vmds_ei_main,

          ls_vendor_read     TYPE vmds_ei_main,
          ls_vendor_detail   TYPE vmds_ei_extern,
          ls_vend_bankdetail TYPE cvis_ei_cvi_bankdetail,

          ls_cust_bankdetail TYPE cvis_ei_cvi_bankdetail,

          ls_errors          TYPE cvis_error,
          lv_message         TYPE bapi_msg,
          ls_error           TYPE bapiret2.

    DATA lr_struct TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS <fs_vend_comp>  TYPE any.
    FIELD-SYMBOLS <fs_vend_struc> TYPE any.
    FIELD-SYMBOLS <fs_cust_comp>  TYPE any.
    FIELD-SYMBOLS <fs_cust_struc> TYPE any.
    FIELD-SYMBOLS <fs_col>        TYPE abap_compdescr.

    " Check Check Check whether the customer is assigned to a vendor

    " Change Version 1.4
    " Dieser Select wurde als inperformant und nicht benötigt angesehen und wird daher entfernt

*    SELECT  lifnr FROM lfa1 INTO lv_lifnr UP TO 1 ROWS
*      WHERE kunnr  = is_customer-header-object_instance-kunnr
*      AND lifnr NE lv_lifnr_initial
*      ORDER BY PRIMARY KEY.
*    ENDSELECT.
*    IF sy-subrc NE 0.
*      lv_lifnr = is_customer-central_data-central-data-lifnr.
*      IF lv_lifnr EQ space.
*        RETURN.
*      ENDIF.
*    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = is_customer-central_data-central-data-lifnr
      IMPORTING
        output = lv_lifnr
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc <> 0.
      lv_lifnr = is_customer-central_data-central-data-lifnr.
    ENDIF.
    " wurde kein verbundener Geschäftspartner gefunden soll die Methode verlassen werden
    IF lv_lifnr = space.
      RETURN.
    ENDIF.

    " End Change

    " Change Version
    " Anzeige von Debitoren-/Kreditorennummer bei Fehler
    ev_bp = lv_lifnr.
    " End Change

    " Read Data
    ls_vendor-header-object_instance-lifnr = lv_lifnr.
    ls_vendor-header-object_task = 'M'.
    APPEND ls_vendor TO lt_vendor-vendors.

    " Read Vendor Data
    vmd_ei_api_extract=>get_data( EXPORTING is_master_data = lt_vendor
                                  IMPORTING es_master_data = ls_vendor_read
                                            es_error       = ls_errors ).

    IF ls_errors IS NOT INITIAL.
      APPEND LINES OF ls_errors-messages TO et_errors.
      IF ls_errors-is_error = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF     ls_vendor_read-vendors                   IS INITIAL
       AND is_customer-header-object_instance-kunnr IS NOT INITIAL.

      " Inconsistent data between customer/vendor - Exit
      MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 029
              WITH lv_lifnr is_customer-header-object_instance-kunnr
              INTO lv_message.

      ls_error-id        = sy-msgid.
      ls_error-type      = sy-msgty.
      ls_error-number    = sy-msgno.
      ls_error-message   = lv_message.
      ls_error-parameter = 'Verknüpfung Vendor <=> Customer'(036).

      APPEND ls_error TO et_errors.
      RETURN.

    ENDIF.

    READ TABLE ls_vendor_read-vendors INTO ls_vendor_detail
         WITH KEY header-object_instance-lifnr = lv_lifnr.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " Check postal data from customer and vendor are the same
    "       -----------
    IF ls_vendor_detail-central_data-address-postal-data <> is_customer-central_data-address-postal-data.

      ASSIGN ls_vendor_detail-central_data-address-postal-data TO <fs_vend_struc>.
      ASSIGN is_customer-central_data-address-postal-data TO <fs_cust_struc>.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_vend_struc> TO <fs_vend_comp>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_cust_struc> TO <fs_cust_comp>.

        IF <fs_vend_comp> = <fs_cust_comp>.
          CONTINUE.
        ENDIF.

        lr_struct ?= cl_abap_structdescr=>describe_by_data( p_data = <fs_vend_struc> ).

        READ TABLE lr_struct->components[] INDEX sy-index ASSIGNING <fs_col>.
        IF sy-subrc = 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 033
                  WITH <fs_col>-name <fs_cust_comp> <fs_vend_comp>
                  INTO lv_message.

          ls_error-id        = sy-msgid.
          ls_error-type      = sy-msgty.
          ls_error-number    = sy-msgno.
          ls_error-message   = lv_message.
          ls_error-parameter = 'Adresse'(013).

          APPEND ls_error TO et_errors.
        ENDIF.

      ENDDO.

*        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 030
*         WITH is_customer-header-object_instance-kunnr lv_lifnr
*         INTO lv_message.
*
*        ls_error-id      = sy-msgid.
*        ls_error-type    = sy-msgty.
*        ls_error-number  = sy-msgno.
*        ls_error-message = lv_message.
*        ls_error-parameter = 'Adresse'(013).

*        APPEND ls_error TO et_errors.

**      Inconsistent postal code data - Exit
*        RETURN.

    ENDIF.

    UNASSIGN: <fs_vend_comp>, <fs_vend_struc>, <fs_cust_comp>, <fs_cust_struc>.

    " Check Bank Data
    LOOP AT is_customer-central_data-bankdetail-bankdetails INTO ls_cust_bankdetail.

      READ TABLE ls_vendor_detail-central_data-bankdetail-bankdetails INTO ls_vend_bankdetail
           WITH KEY data_key-banks = ls_cust_bankdetail-data_key-banks
                    data_key-bankl = ls_cust_bankdetail-data_key-bankl
                    data_key-bankn = ls_cust_bankdetail-data_key-bankn.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF ls_vend_bankdetail-data = ls_cust_bankdetail-data.
        CONTINUE.
      ENDIF.

      ASSIGN ls_vend_bankdetail-data TO <fs_vend_struc>.
      ASSIGN ls_cust_bankdetail-data TO <fs_cust_struc>.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_vend_struc> TO <fs_vend_comp>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT sy-index OF STRUCTURE <fs_cust_struc> TO <fs_cust_comp>.

        IF <fs_vend_comp> = <fs_cust_comp>.
          CONTINUE.
        ENDIF.

        lr_struct ?= cl_abap_structdescr=>describe_by_data( p_data = <fs_vend_struc> ).

        READ TABLE lr_struct->components[] INDEX sy-index ASSIGNING <fs_col>.
        IF sy-subrc = 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 034
                  WITH <fs_col>-name <fs_cust_comp> <fs_vend_comp>
                  INTO lv_message.

          ls_error-id        = sy-msgid.
          ls_error-type      = sy-msgty.
          ls_error-number    = sy-msgno.
          ls_error-message   = lv_message.
          ls_error-parameter = 'Adresse'(013).

          APPEND ls_error TO et_errors.
        ENDIF.

      ENDDO.

*            MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 031
*             WITH is_customer-header-object_instance-kunnr lv_lifnr
*             INTO lv_message.
*
*            ls_error-id        = sy-msgid.
*            ls_error-type      = sy-msgty.
*            ls_error-number    = sy-msgno.
*            ls_error-message   = lv_message.
*            ls_error-parameter = 'Bank'(027).
*
*            APPEND ls_error TO et_errors.

*           Inconsistent bank data - Exit
*            RETURN.
    ENDLOOP.
  ENDMETHOD.                    " check_r1_367_MAP_CUSTOM_TO_BP

  METHOD check_address.

    DATA: lv_nation     TYPE ad_nation,
          lv_name1      TYPE ad_name1,
          lv_name2      TYPE ad_name2,
          lv_name3      TYPE ad_name3,
          lv_name4      TYPE ad_name4,
          lv_title      TYPE ad_title,
          ls_t005       TYPE t005,
          ls_adrc_struc TYPE adrc_struc,
          ls_tsad3      TYPE tsad3,
          lt_errmess    TYPE TABLE OF addr_error,
          ls_errmess    TYPE addr_error,
          ls_error      TYPE bapiret2,

          lt_errors     TYPE bapiret2_t.


    CONSTANTS lc_max_length TYPE i VALUE 35.

    ls_adrc_struc-name_co          = is_address_data-c_o_name.
    ls_adrc_struc-city1            = is_address_data-city.
    ls_adrc_struc-city2            = is_address_data-district.
    ls_adrc_struc-city_code        = is_address_data-city_no.
    ls_adrc_struc-cityp_code       = is_address_data-distrct_no.
    ls_adrc_struc-home_city        = is_address_data-home_city.
    ls_adrc_struc-cityh_code       = is_address_data-homecityno.
    ls_adrc_struc-chckstatus       = is_address_data-chckstatus.
    ls_adrc_struc-regiogroup       = is_address_data-regiogroup.
    ls_adrc_struc-post_code1       = is_address_data-postl_cod1.
    ls_adrc_struc-post_code2       = is_address_data-postl_cod2.
    ls_adrc_struc-post_code3       = is_address_data-postl_cod3.
    ls_adrc_struc-pcode1_ext       = is_address_data-pcode1_ext.
    ls_adrc_struc-pcode2_ext       = is_address_data-pcode2_ext.
    ls_adrc_struc-pcode3_ext       = is_address_data-pcode3_ext.
    ls_adrc_struc-po_box           = is_address_data-po_box.
    ls_adrc_struc-dont_use_p       = is_address_data-dont_use_p.
    ls_adrc_struc-po_box_num       = is_address_data-po_w_o_no.
    ls_adrc_struc-po_box_loc       = is_address_data-po_box_cit.
    ls_adrc_struc-city_code2       = is_address_data-pboxcit_no.
    ls_adrc_struc-po_box_reg       = is_address_data-po_box_reg.
    ls_adrc_struc-po_box_cty       = is_address_data-pobox_ctry.
    ls_adrc_struc-postalarea       = is_address_data-deliv_dis.
    ls_adrc_struc-transpzone       = is_address_data-transpzone.
    ls_adrc_struc-street           = is_address_data-street.
    ls_adrc_struc-dont_use_s       = is_address_data-dont_use_s.
    ls_adrc_struc-streetcode       = is_address_data-street_no.
    ls_adrc_struc-streetabbr       = is_address_data-str_abbr.
    ls_adrc_struc-house_num1       = is_address_data-house_no.
    ls_adrc_struc-house_num2       = is_address_data-house_no2.
    ls_adrc_struc-house_num3       = is_address_data-house_no3.
    ls_adrc_struc-str_suppl1       = is_address_data-str_suppl1.
    ls_adrc_struc-str_suppl2       = is_address_data-str_suppl2.
    ls_adrc_struc-str_suppl3       = is_address_data-str_suppl3.
    ls_adrc_struc-location         = is_address_data-location.
    ls_adrc_struc-building         = is_address_data-building.
    ls_adrc_struc-floor            = is_address_data-floor.
    ls_adrc_struc-roomnumber       = is_address_data-room_no.
    ls_adrc_struc-country          = is_address_data-country.
    ls_adrc_struc-langu            = is_address_data-langu.
    ls_adrc_struc-region           = is_address_data-region.
    ls_adrc_struc-sort1            = is_address_data-sort1.
    ls_adrc_struc-sort2            = is_address_data-sort2.
    ls_adrc_struc-extension1       = is_address_data-extens_1.
    ls_adrc_struc-extension2       = is_address_data-extens_2.
    ls_adrc_struc-time_zone        = is_address_data-time_zone.
    ls_adrc_struc-taxjurcode       = is_address_data-taxjurcode.
    ls_adrc_struc-address_id       = is_address_data-address_id.
    ls_adrc_struc-langu_crea       = is_address_data-langu_cr.
    ls_adrc_struc-po_box_lobby     = is_address_data-po_box_lobby.
    ls_adrc_struc-deli_serv_type   = is_address_data-deli_serv_type.
    ls_adrc_struc-deli_serv_number = is_address_data-deli_serv_number.

    lv_nation = is_address_data-addr_vers.
    lv_name1 = is_address_data-name.
    lv_name2 = is_address_data-name_2.
    lv_name3 = is_address_data-name_3.
    lv_name4 = is_address_data-name_4.
    ls_adrc_struc-city2 = is_address_data-district.
    lv_title = is_address_data-title.

    READ TABLE gt_t005 INTO ls_t005 WITH KEY land1 = ls_adrc_struc-country.
    IF sy-subrc = 0.
      CALL FUNCTION 'ADDR_REGIONAL_DATA_CHECK'
        EXPORTING
          x_adrc_struc     = ls_adrc_struc
          x_dialog_allowed = space
          x_accept_error   = 'X'
          x_t005           = ls_t005
          iv_nation        = lv_nation
*          IMPORTING
*         y_adrc_struc     = ls_adrc_struc
*         Y_RETCODE        = lv_returncode_e
        TABLES
          error_table      = lt_errmess.

      LOOP AT lt_errmess INTO ls_errmess WHERE msg_type CA 'EAX'.
        MESSAGE ID ls_errmess-msg_id TYPE ls_errmess-msg_type NUMBER ls_errmess-msg_number WITH ls_errmess-msg_var1 ls_errmess-msg_var2 ls_errmess-msg_var3
                                          ls_errmess-msg_var4 INTO ls_error-message.

        ls_error-id         = ls_errmess-msg_id.
        ls_error-type       = ls_errmess-msg_type.
        ls_error-number     = ls_errmess-msg_number.
        ls_error-message_v4 = lv_nation. " ls_errmess-msg_var4.
        ls_error-field      = ls_errmess-fieldname.
        ls_error-parameter  = 'Adresse'(013).
        INSERT ls_error INTO TABLE lt_errors.
      ENDLOOP.

      " Überprüfung, ob der richtige Titel für Person/Organisation gepflegt ist
      IF lv_title IS NOT INITIAL.
        READ TABLE gt_tsad3 INTO ls_tsad3 WITH KEY title = lv_title.
        IF     sy-subrc            = 0
           AND ls_tsad3-person     = 'X'
           AND ls_tsad3-xgroup     = 'X'
           AND ls_tsad3-organizatn = ''.

          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 005 WITH lv_title INTO ls_error-message.
          ls_error-id         = sy-msgid.
          ls_error-type       = sy-msgty.
          ls_error-number     = sy-msgno.
          ls_error-message_v4 = lv_title. " sy-msgv4.
          ls_error-field      = 'ADRC-TITLE'.
          ls_error-parameter  = 'Adresse'(013).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.

      CLEAR ls_error.

      " Längenüberprüfung, Länge darf lc_max_length nicht überschreiten
      ls_error-id        = 'AM'.
      ls_error-number    = '228'.
      ls_error-parameter = 'Adresse'(013).
      IF strlen( lv_name1 ) > lc_max_length.
        ls_error-field      = 'ADRC-NAME1'.
        ls_error-message_v4 = lv_name1. " sy-msgv4.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen( lv_name2 ) > lc_max_length.
        ls_error-message_v4 = lv_name2. " sy-msgv4.
        ls_error-field      = 'ADRC-NAME2'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen( lv_name3 ) > lc_max_length.
        ls_error-message_v4 = lv_name3. " sy-msgv4.
        ls_error-field      = 'ADRC-NAME3'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen( lv_name4 ) > lc_max_length.
        ls_error-message_v4 = lv_name4. " sy-msgv4.
        ls_error-field      = 'ADRC-NAME4'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen( ls_adrc_struc-street ) > lc_max_length.
        ls_error-message_v4 = ls_adrc_struc-street. " sy-msgv4.
        ls_error-field      = 'ADRC-STREET'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen( ls_adrc_struc-city1 ) > lc_max_length.
        ls_error-message_v4 = ls_adrc_struc-city1. " sy-msgv4.
        ls_error-field      = 'ADRC-CITY1'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen( ls_adrc_struc-city2 ) > lc_max_length. " im Original CITY2
        ls_error-message_v4 = ls_adrc_struc-city2. " sy-msgv4.
        ls_error-field      = 'ADRC-CITY2'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen(  ls_adrc_struc-str_suppl1 ) > lc_max_length.
        ls_error-message_v4 = ls_adrc_struc-str_suppl1. " sy-msgv4.
        ls_error-field      = 'ADRC-STR_SUPPL1'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen(  ls_adrc_struc-str_suppl2 ) > lc_max_length.
        ls_error-message_v4 = ls_adrc_struc-str_suppl2. " sy-msgv4.
        ls_error-field      = 'ADRC-STR_SUPPL2'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen(  ls_adrc_struc-str_suppl3 ) > lc_max_length.
        ls_error-message_v4 = ls_adrc_struc-str_suppl3. " sy-msgv4.
        ls_error-field      = 'ADRC-STR_SUPPL3'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      IF strlen(  ls_adrc_struc-location ) > lc_max_length.
        ls_error-message_v4 = ls_adrc_struc-location. " sy-msgv4.
        ls_error-field      = 'ADRC-LOCATION'.
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 004 WITH ls_error-field INTO ls_error-message.
        ls_error-type = sy-msgty.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.

    ENDIF.

    et_errors = lt_errors.
  ENDMETHOD.                    " check_address

  METHOD check_email.
    DATA lt_smtp        TYPE cvis_ei_smtp_t.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_addr_length TYPE i.
    DATA lv_char        TYPE c LENGTH 1.
    DATA ls_unstruct    TYPE sx_address.
    DATA ls_error       TYPE bapiret2.
    DATA lt_errors      TYPE bapiret2_t.

    FIELD-SYMBOLS <ls_smtp> TYPE cvis_ei_smtp_str.

    lt_smtp[] = it_smtp[].

    ls_error-field     = 'ADR6-SMTP_ADDR'.
    ls_error-parameter = 'E-Mail'(014).

    LOOP AT lt_smtp ASSIGNING <ls_smtp>.
      TRY.
          lv_addr_length = strlen( <ls_smtp>-contact-data-e_mail ) - 1.
          lv_char = <ls_smtp>-contact-data-e_mail+lv_addr_length.
          IF lv_char = ';'.

            MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 006 WITH <ls_smtp>-contact-data-e_mail INTO ls_error-message.
            ls_error-id     = sy-msgid.
            ls_error-type   = sy-msgty.
            ls_error-number = sy-msgno.
*            ls_error-message_v4 = sy-msgv4.

            INSERT ls_error INTO TABLE lt_errors.

          ELSE.
            CLEAR ls_unstruct.
            ls_unstruct-type    = 'INT'.
            ls_unstruct-address = <ls_smtp>-contact-data-e_mail.
            CALL FUNCTION 'SX_INTERNET_ADDRESS_TO_NORMAL'
              EXPORTING
                address_unstruct   = ls_unstruct
                complete_address   = 'X'
              EXCEPTIONS
                error_address_type = 1
                error_address      = 2.
            IF sy-subrc <> 0.
              ls_error-id     = sy-msgid.
              ls_error-type   = sy-msgty.
              ls_error-number = sy-msgno.
              MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 006 WITH <ls_smtp>-contact-data-e_mail INTO ls_error-message.
*              ls_error-message_v4 = sy-msgv4.
              INSERT ls_error INTO TABLE lt_errors.

            ENDIF.
          ENDIF.
        CATCH cx_root.
      ENDTRY.
    ENDLOOP.

    et_errors = lt_errors.

  ENDMETHOD.                    " check_email

  METHOD check_number_range.
    DATA lv_numkr  TYPE numkr.
    DATA ls_nriv   TYPE nriv.
    DATA lt_errors TYPE bapiret2_t.
    DATA ls_error  TYPE bapiret2.
    DATA ls_t077d  TYPE t077d.
    DATA ls_t077k  TYPE t077k.

    CLEAR lv_numkr.
    IF iv_nrobj = gc_debitor.
      READ TABLE gt_t077d INTO ls_t077d WITH KEY ktokd = iv_account_group TRANSPORTING numkr.
      IF sy-subrc = 0.
        lv_numkr = ls_t077d-numkr.
      ENDIF.
    ELSEIF iv_nrobj = gc_kreditor.
      READ TABLE gt_t077k INTO ls_t077k WITH KEY ktokk = iv_account_group TRANSPORTING numkr.
      IF sy-subrc = 0.
        lv_numkr = ls_t077k-numkr.
      ENDIF.
    ENDIF.

    IF lv_numkr IS NOT INITIAL.
      READ TABLE gt_nriv INTO ls_nriv WITH KEY object = iv_nrobj nrrangenr = lv_numkr.
      IF sy-subrc = 0 AND iv_objnr IS NOT INITIAL AND ( iv_objnr < ls_nriv-fromnumber OR iv_objnr > ls_nriv-tonumber ).
        CLEAR ls_error.
        ls_error-parameter = 'Nummernkreis'(026).
        ls_error-id        = 'R1'.
        ls_error-number    = '099'.
        ls_error-field     = 'NUMBERRANGE'.
        IF iv_nrobj = gc_debitor.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 007 WITH iv_objnr ls_nriv-fromnumber ls_nriv-tonumber INTO ls_error-message.
          ls_error-type       = sy-msgty.
          ls_error-message_v4 = iv_objnr. " sy-msgv4.
          INSERT ls_error INTO TABLE lt_errors.
        ELSEIF iv_nrobj = gc_kreditor.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 008 WITH iv_objnr ls_nriv-fromnumber ls_nriv-tonumber INTO ls_error-message.
          ls_error-message_v4 = iv_objnr. " sy-msgv4.
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.
    ENDIF.

    et_errors = lt_errors.

  ENDMETHOD.                    " check_number_range

  METHOD check_bank_data.
    DATA ls_bnka        TYPE bnka.
    DATA ls_error       TYPE bapiret2.
    DATA lt_errors      TYPE bapiret2_t.
    DATA lt_bankdetails TYPE cvis_ei_bankdetail_t.
    DATA lv_tabname     TYPE tabname.

    FIELD-SYMBOLS <ls_bankdetail> TYPE cvis_ei_cvi_bankdetail.

    IF iv_nrobj = gc_debitor.
      lv_tabname = 'KNBK'.
    ELSE.
      lv_tabname = 'LFBK'.
    ENDIF.

    lt_bankdetails = it_bankdetails.

    LOOP AT lt_bankdetails ASSIGNING <ls_bankdetail>.
      IF iv_nrobj = gc_debitor.
        CLEAR ls_bnka.
        CALL FUNCTION 'READ_BANK_ADDRESS'
          EXPORTING
            bank_country = <ls_bankdetail>-data_key-banks
            bank_number  = <ls_bankdetail>-data_key-bankl
          IMPORTING
            bnka_wa      = ls_bnka
          EXCEPTIONS
            not_found    = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
          ls_error-id     = sy-msgid.
          ls_error-type   = sy-msgty.
          ls_error-number = sy-msgno.
          MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 009 WITH <ls_bankdetail>-data_key-banks <ls_bankdetail>-data_key-bankl INTO ls_error-message.
          ls_error-message_v4 = <ls_bankdetail>-data_key-bankl. " sy-msgv4.
          CONCATENATE lv_tabname '-BANKL' INTO ls_error-field.
          ls_error-parameter = 'Bank'(027).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.

        IF sy-subrc = 0 AND ls_bnka-xpgro IS NOT INITIAL.
          CALL FUNCTION 'CHECK_END_BANK_ADDRESS'
            EXPORTING
              bank_account     = <ls_bankdetail>-data_key-bankn
              bank_control_key = <ls_bankdetail>-data-bkont
              bank_country     = <ls_bankdetail>-data_key-banks
              bank_number      = <ls_bankdetail>-data_key-bankl
              bank_xpgro       = ls_bnka-xpgro
            EXCEPTIONS
              not_valid        = 1
              OTHERS           = 2.
          IF sy-subrc <> 0 AND sy-msgty <> 'W'.
            ls_error-id     = sy-msgid.
            ls_error-type   = sy-msgty.
            ls_error-number = sy-msgno.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 010 WITH <ls_bankdetail>-data_key-bankl
                                                                           <ls_bankdetail>-data_key-bankn
                                                                           <ls_bankdetail>-data_key-banks INTO ls_error-message.
            CONCATENATE lv_tabname '-BANKL' INTO ls_error-field.
            ls_error-message_v4 = <ls_bankdetail>-data_key-bankn. " sy-msgv4.
            ls_error-parameter  = 'Bank'(027).
            INSERT ls_error INTO TABLE lt_errors.

          ENDIF.
        ENDIF.
      ENDIF.

      " Below checks common for customer and vendor.
      CALL FUNCTION 'BANK_NUMBER_CHECK'
        EXPORTING
          bank_account = <ls_bankdetail>-data_key-bankn
          bank_country = <ls_bankdetail>-data_key-banks
          bank_number  = <ls_bankdetail>-data_key-bankl
        EXCEPTIONS
          not_valid    = 1
          OTHERS       = 2.
      IF sy-subrc <> 0 AND sy-msgty <> 'W'.
        ls_error-id        = sy-msgid.
        ls_error-number    = sy-msgno.
        ls_error-type      = sy-msgty.
        ls_error-parameter = 'Bank'(027).
        IF sy-msgv2 IS NOT INITIAL.
          MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 011 WITH <ls_bankdetail>-data_key-bankl sy-msgv2 INTO ls_error-message.
        ELSE.
          MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 010 WITH <ls_bankdetail>-data_key-bankl
                                                                         <ls_bankdetail>-data_key-bankn
                                                                         <ls_bankdetail>-data_key-banks INTO ls_error-message.
        ENDIF.

        CONCATENATE lv_tabname '-BANKL' INTO ls_error-field.
        ls_error-message_v4 = <ls_bankdetail>-data_key-bankl. " sy-msgv4.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.

      IF <ls_bankdetail>-data_key-bankn IS INITIAL. " IBAN bank data.

        IF <ls_bankdetail>-data-iban IS INITIAL.
          ls_error-id        = 'AR'.
          ls_error-number    = '141'.
          ls_error-parameter = 'Bank'(027).
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 012 WITH <ls_bankdetail>-data_key-banks INTO ls_error-message.
          ls_error-type = sy-msgty.
          CONCATENATE lv_tabname '-BANKN' INTO ls_error-field.
          ls_error-message_v4 = <ls_bankdetail>-data_key-bankn. " sy-msgv4.
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ELSE.
        CALL FUNCTION 'BANK_ACCOUNT_CHECK'
          EXPORTING
            bank_account     = <ls_bankdetail>-data_key-bankn
            bank_control_key = <ls_bankdetail>-data-bkont " important for countries Brazil, France, Spain, Portugal, and Italy
            bank_country     = <ls_bankdetail>-data_key-banks
            bank_number      = <ls_bankdetail>-data_key-bankl
          EXCEPTIONS
            not_valid        = 1
            OTHERS           = 2.
        IF sy-subrc <> 0 AND sy-msgty <> 'W'.
          ls_error-id     = sy-msgid.
          ls_error-type   = sy-msgty.
          ls_error-number = sy-msgno.
          IF sy-msgv2 IS NOT INITIAL AND sy-msgv3 <> <ls_bankdetail>-data_key-bankn.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 011 WITH <ls_bankdetail>-data_key-bankn sy-msgv2 INTO ls_error-message.
          ELSE.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 010 WITH <ls_bankdetail>-data_key-bankn
                                                                           <ls_bankdetail>-data_key-bankl
                                                                           <ls_bankdetail>-data_key-banks INTO ls_error-message.
          ENDIF.
          ls_error-message_v4 = <ls_bankdetail>-data_key-bankl. " sy-msgv4.
          CONCATENATE lv_tabname '-BANKN' INTO ls_error-field.
          ls_error-parameter = 'Bank'(027).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.
    ENDLOOP.

    et_errors = lt_errors.
  ENDMETHOD.                    " check_bank_data

  METHOD check_tax_type.
    DATA ls_error    TYPE bapiret2.
    DATA lt_errors   TYPE bapiret2_t.
    DATA lv_tax_chk  TYPE tfktaxnumtype-taxtype.
    DATA lv_land     TYPE land1.
    DATA lv_stceg    TYPE stceg.
    DATA lv_stcdt    TYPE j_1atoid.
    DATA lv_stcd1    TYPE stcd1.
    DATA lv_stcd2    TYPE stcd2.
    DATA lv_stcd3    TYPE stcd3.
    DATA lv_stcd4    TYPE stcd4.
    DATA lv_stcd5    TYPE stcd5.
    DATA lv_stkzu    TYPE stkzu.
    DATA lv_stkzn    TYPE stkzn.
    DATA lv_regio    TYPE regio.
    DATA lv_tabname1 TYPE tabname.
    DATA lv_tabname2 TYPE tabname.

    CONSTANTS: lc_msg_class_old TYPE string VALUE 'FKBPTAX', " Additional msg class for tax type from RIG report
               lc_msg_no_old    TYPE string VALUE '033'.     " Additional msg class for tax type from RIG report

    FIELD-SYMBOLS <ls_eu_vat_num> TYPE cvis_ei_vat.

    IF iv_nrobj = gc_debitor.
      lv_tabname1 = 'KNA1'.
      lv_tabname2 = 'KNAS'.
    ELSE.
      lv_tabname1 = 'LFA1'.
      lv_tabname2 = 'LFAS'.
    ENDIF.

    IF is_customer_central_data IS SUPPLIED.
      lv_land = is_address_data-country.
      lv_regio = is_address_data-region.
      lv_stceg = is_customer_central_data-stceg.
      lv_stcdt = is_customer_central_data-stcdt.
      lv_stcd1 = is_customer_central_data-stcd1.
      lv_stcd2 = is_customer_central_data-stcd2.
      lv_stcd3 = is_customer_central_data-stcd3.
      lv_stcd4 = is_customer_central_data-stcd4.
      lv_stcd5 = is_customer_central_data-stcd5.
      lv_stkzu = is_customer_central_data-stkzu.
      lv_stkzn = is_customer_central_data-stkzn.
    ELSEIF is_vendor_central_data IS SUPPLIED.
      lv_land = is_address_data-country.
      lv_regio = is_address_data-region.
      lv_stceg = is_vendor_central_data-stceg.
      lv_stcdt = is_vendor_central_data-stcdt.
      lv_stcd1 = is_vendor_central_data-stcd1.
      lv_stcd2 = is_vendor_central_data-stcd2.
      lv_stcd3 = is_vendor_central_data-stcd3.
      lv_stcd4 = is_vendor_central_data-stcd4.
      lv_stcd5 = is_vendor_central_data-stcd5.
      lv_stkzu = is_vendor_central_data-stkzu.
      lv_stkzn = is_vendor_central_data-stkzn.
    ENDIF.

    IF lv_stceg IS NOT INITIAL.  " Check for country which is not an EU member.
      CALL FUNCTION 'EU_TAX_NUMBER_CHECK'
        EXPORTING
          country       = lv_land
          eu_tax_number = lv_stceg
*         PARTNER       =
        EXCEPTIONS
          not_valid     = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        ls_error-id     = sy-msgid.
        ls_error-type   = sy-msgty.
        ls_error-number = sy-msgno.
        MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 013 WITH lv_stceg lv_land INTO ls_error-message.

        CONCATENATE lv_tabname1 '-STCEG' INTO ls_error-field.
        ls_error-message_v4 = lv_stceg. " sy-msgv4.
        ls_error-parameter  = 'Steuerart'(028).
        INSERT ls_error INTO TABLE lt_errors.

      ENDIF.

      LOOP AT it_eu_vat_nums ASSIGNING <ls_eu_vat_num>.
        CALL FUNCTION 'EU_TAX_NUMBER_CHECK'
          EXPORTING
            country       = <ls_eu_vat_num>-data_key-land1
            eu_tax_number = <ls_eu_vat_num>-data-stceg
*           PARTNER       =
          EXCEPTIONS
            not_valid     = 1
            OTHERS        = 2.
        IF sy-subrc <> 0.
          ls_error-id     = sy-msgid.
          ls_error-type   = sy-msgty.
          ls_error-number = sy-msgno.
          MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 013 WITH <ls_eu_vat_num>-data-stceg <ls_eu_vat_num>-data_key-land1 INTO ls_error-message.
          ls_error-message_v4 = <ls_eu_vat_num>-data-stceg. " sy-msgv4.
          CONCATENATE lv_tabname2 '-STCEG' INTO ls_error-field.
          ls_error-parameter = 'Steuerart'(028).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDLOOP.

      " tax type check table TFKTAXNUMTYPE
      IF lv_stcdt IS INITIAL.
        CONCATENATE lv_land '0' INTO lv_tax_chk.
        READ TABLE gt_tfktaxnumtype WITH KEY taxtype = lv_tax_chk TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 014 WITH lv_tax_chk lv_land INTO ls_error-message.
          ls_error-id         = lc_msg_class_old.
          ls_error-number     = lc_msg_no_old.
          ls_error-type       = sy-msgty.
          ls_error-message_v4 = lv_tax_chk. " sy-msgv4.
          CONCATENATE lv_tabname1 '-LAND1' INTO ls_error-field.
          ls_error-parameter = 'Steuerart'(028).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.
    ENDIF.

    " Tax Number STCD1-STCD5 Check
    IF lv_stcd1 <> space.
      CALL FUNCTION 'TAX_NUMBER_CHECK'
        EXPORTING
          country             = lv_land
          natural_person_flag = lv_stkzn
          region              = lv_regio
          stkzu               = lv_stkzu
          tax_code_1          = lv_stcd1
        EXCEPTIONS
          not_valid           = 1
          different_fprcd     = 2  " Not handle due to not raised inside the FM.
          OTHERS              = 3.

      IF sy-subrc <> 0.
        ls_error-id     = sy-msgid.
        ls_error-type   = sy-msgty.
        ls_error-number = sy-msgno.
        MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 015 WITH '1' lv_stcd1 lv_land INTO ls_error-message.
        ls_error-message_v4 = lv_stcd1. " sy-msgv4.
        CONCATENATE lv_tabname1 '-STCD1' INTO ls_error-field.
        ls_error-parameter = 'Steuerart'(028).
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      " tax type check table TFKTAXNUMTYPE

      IF lv_stcdt IS INITIAL.
        CONCATENATE lv_land '1' INTO lv_tax_chk.
        READ TABLE gt_tfktaxnumtype WITH KEY taxtype = lv_tax_chk TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 014 WITH lv_tax_chk lv_land INTO ls_error-message.
          ls_error-id         = lc_msg_class_old.
          ls_error-number     = lc_msg_no_old.
          ls_error-type       = sy-msgty.
          ls_error-message_v4 = lv_tax_chk. " sy-msgv4.
          CONCATENATE lv_tabname1 '-LAND1' INTO ls_error-field.
          ls_error-parameter = 'Steuerart'(028).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_stcd2 <> space.
      CALL FUNCTION 'TAX_NUMBER_CHECK'
        EXPORTING
          country             = lv_land
          natural_person_flag = lv_stkzn
          region              = lv_regio
          stkzu               = lv_stkzu
          tax_code_2          = lv_stcd2
        EXCEPTIONS
          not_valid           = 1
          different_fprcd     = 2  " Not handle due to not raised inside the FM.
          OTHERS              = 3.

      IF sy-subrc <> 0.
        ls_error-id     = sy-msgid.
        ls_error-type   = sy-msgty.
        ls_error-number = sy-msgno.
        MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 015 WITH '2' lv_stcd2 lv_land INTO ls_error-message.
        ls_error-message_v4 = lv_stcd2. " sy-msgv4.
        CONCATENATE lv_tabname1 '-STCD2' INTO ls_error-field.
        ls_error-parameter = 'Steuerart'(028).
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      "
      " tax type check table TFKTAXNUMTYPE

      IF lv_stcdt IS INITIAL.
        CONCATENATE lv_land '2' INTO lv_tax_chk.
        READ TABLE gt_tfktaxnumtype WITH KEY taxtype = lv_tax_chk TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 014 WITH lv_tax_chk lv_land INTO ls_error-message.
          ls_error-id         = lc_msg_class_old.
          ls_error-type       = sy-msgty.
          ls_error-number     = lc_msg_no_old.
          ls_error-message_v4 = lv_tax_chk. " sy-msgv4.
          CONCATENATE lv_tabname1 '-LAND1' INTO ls_error-field.
          ls_error-parameter = 'Steuerart'(028).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_stcd3 <> space.
      CALL FUNCTION 'TAX_NUMBER_CHECK'
        EXPORTING
          country             = lv_land
          natural_person_flag = lv_stkzn
          region              = lv_regio
          stkzu               = lv_stkzu
          tax_code_3          = lv_stcd3
        EXCEPTIONS
          not_valid           = 1
          different_fprcd     = 2  " Not handle due to not raised inside the FM.
          OTHERS              = 3.

      IF sy-subrc <> 0.
        ls_error-id     = sy-msgid.
        ls_error-type   = sy-msgty.
        ls_error-number = sy-msgno.
        MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 015 WITH '3' lv_stcd3 lv_land INTO ls_error-message.
        ls_error-message_v4 = lv_stcd3. " sy-msgv4.
        CONCATENATE lv_tabname1 '-STCD3' INTO ls_error-field.
        ls_error-parameter = 'Steuerart'(028).
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
      " tax type check table TFKTAXNUMTYPE

      IF lv_stcdt IS INITIAL.
        CONCATENATE lv_land '3' INTO lv_tax_chk.
        READ TABLE gt_tfktaxnumtype WITH KEY taxtype = lv_tax_chk TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 014 WITH lv_tax_chk lv_land INTO ls_error-message.
          ls_error-id         = lc_msg_class_old.
          ls_error-type       = sy-msgty.
          ls_error-number     = lc_msg_no_old.
          ls_error-message_v4 = lv_tax_chk. " sy-msgv4.
          CONCATENATE lv_tabname1 '-LAND1' INTO ls_error-field.
          ls_error-parameter = 'Steuerart'(028).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_stcd4 <> space.
      CALL FUNCTION 'TAX_NUMBER_CHECK'
        EXPORTING
          country             = lv_land
          natural_person_flag = lv_stkzn
          region              = lv_regio
          stkzu               = lv_stkzu
          tax_code_4          = lv_stcd4
        EXCEPTIONS
          not_valid           = 1
          different_fprcd     = 2  " Not handle due to not raised inside the FM.
          OTHERS              = 3.

      IF sy-subrc <> 0.
        ls_error-id     = sy-msgid.
        ls_error-type   = sy-msgty.
        ls_error-number = sy-msgno.
        MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 015 WITH '4' lv_stcd4 lv_land INTO ls_error-message.
        ls_error-message_v4 = lv_stcd4. " sy-msgv4.
        CONCATENATE lv_tabname1 '-STCD4' INTO ls_error-field.
        ls_error-parameter = 'Steuerart'(028).
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.

      " tax type check table TFKTAXNUMTYPE
      IF lv_stcdt IS INITIAL.
        CONCATENATE lv_land '4' INTO lv_tax_chk.
        READ TABLE gt_tfktaxnumtype WITH KEY taxtype = lv_tax_chk TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 014 WITH lv_tax_chk lv_land INTO ls_error-message.
          ls_error-id         = lc_msg_class_old.
          ls_error-type       = sy-msgty.
          ls_error-number     = lc_msg_no_old.
          ls_error-message_v4 = lv_tax_chk. " sy-msgv4.
          CONCATENATE lv_tabname1 '-LAND1' INTO ls_error-field.
          ls_error-parameter = 'Steuerart'(028).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_stcd5 <> space.
      CALL FUNCTION 'TAX_NUMBER_CHECK'
        EXPORTING
          country             = lv_land
          natural_person_flag = lv_stkzn
          region              = lv_regio
          stkzu               = lv_stkzu
          tax_code_5          = lv_stcd5
        EXCEPTIONS
          not_valid           = 1
          different_fprcd     = 2  " Not handle due to not raised inside the FM.
          OTHERS              = 3.

      IF sy-subrc <> 0.
        ls_error-id     = sy-msgid.
        ls_error-type   = sy-msgty.
        ls_error-number = sy-msgno.
        MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 015 WITH '5' lv_stcd5 lv_land INTO ls_error-message.
        ls_error-message_v4 = lv_stcd5. " sy-msgv4.
        CONCATENATE lv_tabname1 '-STCD5' INTO ls_error-field.
        ls_error-parameter = 'Steuerart'(028).
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.

      " tax type check table TFKTAXNUMTYPE
      IF lv_stcdt IS INITIAL.
        CONCATENATE lv_land '5' INTO lv_tax_chk.
        READ TABLE gt_tfktaxnumtype WITH KEY taxtype = lv_tax_chk TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 014 WITH lv_tax_chk lv_land INTO ls_error-message.
          ls_error-id         = lc_msg_class_old.
          ls_error-type       = sy-msgty.
          ls_error-number     = lc_msg_no_old.
          ls_error-message_v4 = lv_tax_chk. " sy-msgv4.
          CONCATENATE lv_tabname1 '-LAND1' INTO ls_error-field.
          ls_error-parameter = 'Steuerart'(028).
          INSERT ls_error INTO TABLE lt_errors.
        ENDIF.
      ENDIF.
    ENDIF.

    et_errors = lt_errors.

  ENDMETHOD.                    " check_tax_type

  METHOD check_tax_jur.
    DATA ls_error   TYPE bapiret2.
    DATA lt_errors  TYPE bapiret2_t.
    DATA lv_tabname TYPE tabname.

    IF     is_address_data            IS NOT INITIAL
       AND is_address_data-taxjurcode IS NOT INITIAL.

      IF iv_nrobj = gc_debitor.
        lv_tabname = 'KNA1'.
      ELSE.
        lv_tabname = 'LFA1'.
      ENDIF.

      CALL FUNCTION 'TAX_TXJCD_DETERMINE_CHECK'
        EXPORTING
          im_country        = is_address_data-country
          im_region         = is_address_data-region
          im_zipcode        = is_address_data-postl_cod1
          im_city           = is_address_data-city
          im_county         = is_address_data-district
          im_taxjurcode     = is_address_data-taxjurcode
          im_no_dialog      = 'X'
        EXCEPTIONS
          rfcdest_not_found = 3
          OTHERS            = 1.
      IF sy-subrc <> 0.
        ls_error-id     = sy-msgid.
        ls_error-type   = sy-msgty.
        ls_error-number = sy-msgno.
        CONCATENATE lv_tabname '-TXJCD' INTO ls_error-field.
        ls_error-parameter = 'Steuerstandort'(029).
        IF sy-subrc = 3.
          MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 017 WITH is_address_data-taxjurcode INTO ls_error-message.
        ELSE.
          MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 016 WITH is_address_data-taxjurcode is_address_data-country INTO ls_error-message.

        ENDIF.
        ls_error-message_v4 = is_address_data-taxjurcode. " sy-msgv4.
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
    ENDIF.

    et_errors = lt_errors.
  ENDMETHOD.                    " check_tax_jur

  METHOD check_postcode.
    DATA ls_error          TYPE bapiret2.
    DATA ls_postal_address TYPE adrs_post.
    DATA lt_errors         TYPE bapiret2_t.

    IF is_address_data IS NOT INITIAL.

      CLEAR ls_postal_address.

      ls_postal_address-name_co          = is_address_data-c_o_name.
      ls_postal_address-street           = is_address_data-street.
      ls_postal_address-house_num1       = is_address_data-house_no.
      ls_postal_address-house_num2       = is_address_data-house_no2.
      ls_postal_address-str_suppl1       = is_address_data-str_suppl1.
      ls_postal_address-str_suppl2       = is_address_data-str_suppl2.
      ls_postal_address-str_suppl3       = is_address_data-str_suppl3.
      ls_postal_address-city1            = is_address_data-city.
      ls_postal_address-city2            = is_address_data-district.
      ls_postal_address-home_city        = is_address_data-home_city.
      ls_postal_address-post_code1       = is_address_data-postl_cod1.
      ls_postal_address-post_code2       = is_address_data-postl_cod2.
      ls_postal_address-post_code3       = is_address_data-postl_cod3.
      ls_postal_address-pcode1_ext       = is_address_data-pcode1_ext.
      ls_postal_address-pcode2_ext       = is_address_data-pcode2_ext.
      ls_postal_address-pcode3_ext       = is_address_data-pcode3_ext.
      ls_postal_address-po_box           = is_address_data-po_box.
      ls_postal_address-po_box_num       = is_address_data-po_w_o_no.
      ls_postal_address-po_box_loc       = is_address_data-po_box_cit.
      ls_postal_address-po_box_reg       = is_address_data-po_box_reg.
      ls_postal_address-po_box_cty       = is_address_data-pobox_ctry.
      ls_postal_address-location         = is_address_data-location.
      ls_postal_address-region           = is_address_data-region.
      ls_postal_address-country          = is_address_data-country.
      ls_postal_address-po_box_lobby     = is_address_data-po_box_lobby.
      ls_postal_address-deli_serv_type   = is_address_data-deli_serv_type.
      ls_postal_address-deli_serv_number = is_address_data-deli_serv_number.

      CALL FUNCTION 'ADDR_POSTAL_CODE_CHECK'
        EXPORTING
          country                        = ls_postal_address-country
          postal_address                 = ls_postal_address
        EXCEPTIONS
          country_not_valid              = 01
          region_not_valid               = 02
          postal_code_city_not_valid     = 03
          postal_code_po_box_not_valid   = 04
          postal_code_company_not_valid  = 05
          po_box_missing                 = 06
          postal_code_po_box_missing     = 07
          postal_code_missing            = 08
          postal_code_pobox_comp_missing = 09
          po_box_region_not_valid        = 10
          po_box_country_not_valid       = 11
          pobox_and_poboxnum_filled      = 12
          OTHERS                         = 99.
      IF sy-subrc <> 0.
        ls_error-id        = sy-msgid.
        ls_error-type      = sy-msgty.
        ls_error-number    = sy-msgno.
        ls_error-parameter = 'Postleitzahl'(030).
        CASE sy-subrc.
          WHEN 01.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 025 WITH ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-country. " sy-msgv4.
            ls_error-field      = 'ADRC-COUNTRY'.
          WHEN 02.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 025 WITH ls_postal_address-region ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-region. " sy-msgv4.
            ls_error-field      = 'ADRC-REGION'.
          WHEN 03.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 025 WITH ls_postal_address-post_code1 ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-post_code1. " sy-msgv4.
            ls_error-field      = 'ADRC-POST_COD1'.
          WHEN 04.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 025 WITH ls_postal_address-post_code2 ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-post_code2. " sy-msgv4.
            ls_error-field      = 'ADRC-POST_COD2'.
          WHEN 05.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 025 WITH ls_postal_address-post_code3 ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-post_code3. " sy-msgv4.
            ls_error-field      = 'ADRC-POST_COD3'.
          WHEN 06.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 019 WITH 'PO_BOX' ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-po_box_cty. " sy-msgv4.
            ls_error-field      = 'ADRC-PO_BOX'.
          WHEN 07.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 019 WITH 'POST_CODE2' ls_postal_address-country INTO ls_error-message.
            ls_error-field = ls_postal_address-post_code2.
            ls_error-field = 'ADRC-POST_COD2'.
          WHEN 08.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 019 WITH 'POST_CODE1' ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-post_code1. " sy-msgv4.
            ls_error-field      = 'ADRC-POST_COD1'.
          WHEN 09.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO ls_error-message.
*            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 019 WITH 'PO_BOX_LOC' ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-po_box_loc. " sy-msgv4.
            ls_error-field      = 'ADRC-PO_BOX_LOC'.
          WHEN 10.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 025 WITH ls_postal_address-po_box_reg ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-po_box_reg. " sy-msgv4.
            ls_error-field      = 'ADRC-PO_BOX_REG'.
          WHEN 11.
            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 025 WITH ls_postal_address-po_box_cty ls_postal_address-country INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-po_box_cty. " sy-msgv4.
            ls_error-field      = 'ADRC-POBOX_CTY'.
          WHEN 12.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO ls_error-message.
*            MESSAGE ID gc_msg_class_precheck TYPE sy-msgty NUMBER 020 WITH ls_postal_address-po_box_num INTO ls_error-message.
            ls_error-message_v4 = ls_postal_address-po_box_num. " sy-msgv4.
            ls_error-field      = 'ADRC-PO_BOX_NUM'.
          WHEN OTHERS.
        ENDCASE.

        INSERT ls_error INTO TABLE lt_errors.

      ENDIF.
    ENDIF.

    et_errors = lt_errors.

  ENDMETHOD.                    " check_postcode

  METHOD check_transzone.
    DATA lv_fausa  TYPE fausa_077d.
    DATA ls_error  TYPE bapiret2.
    DATA ls_t077d  TYPE t077d.
    DATA ls_t077k  TYPE t077k.
    DATA lt_errors TYPE bapiret2_t.

    IF is_address_data-transpzone IS INITIAL.
      CLEAR lv_fausa.
      IF iv_nrobj = gc_debitor.
        READ TABLE gt_t077d INTO ls_t077d WITH KEY ktokd = iv_ktokd TRANSPORTING fausa.
        IF sy-subrc = 0.
          lv_fausa = ls_t077d-fausa.
        ENDIF.
      ELSEIF iv_nrobj = gc_kreditor.
        READ TABLE gt_t077k INTO ls_t077k WITH KEY ktokk = iv_ktokk TRANSPORTING fausa.
        IF sy-subrc = 0.
          lv_fausa = ls_t077k-fausa.
        ENDIF.
      ENDIF.

      IF lv_fausa IS NOT INITIAL AND ls_t077d-fausa+24(1) = '+'. " Transportzone is mandatory
        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 022 WITH is_address_data-transpzone is_address_data-country INTO ls_error-message.
        ls_error-id         = '00'.
        ls_error-type       = sy-msgty.
        ls_error-number     = '055'.
        ls_error-message_v4 = ''. " sy-msgv4.
        ls_error-field      = 'ADRC-TRANSPZONE'.
        ls_error-parameter  = 'Transportzone'(034).
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
    ELSE.
      READ TABLE gt_tzone WITH KEY land1 = is_address_data-country zone1 = is_address_data-transpzone TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " The transport zone Z000000001 is not defined for country DE

        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 023 WITH is_address_data-transpzone is_address_data-country INTO ls_error-message.
        ls_error-id         = 'AM'.
        ls_error-type       = sy-msgty.
        ls_error-number     = '129'.
        ls_error-message_v4 = is_address_data-transpzone. " sy-msgv4.
        ls_error-field      = 'ADRC-TRANSPZONE'.
        ls_error-parameter  = 'Transportzone'(034).
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
    ENDIF.

    et_errors = lt_errors.

  ENDMETHOD.                    " check_transzone

  METHOD check_industry.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_brsch  TYPE tp038m2-brsch.
    DATA ls_error  TYPE bapiret2.
    DATA lt_errors TYPE bapiret2_t.

    " Perform the execution of checks for Industry regions
    IF iv_brsch IS NOT INITIAL AND gv_istype IS NOT INITIAL.
      SELECT SINGLE brsch FROM tp038m2
                          INTO lv_brsch
                           WHERE brsch  = iv_brsch
                             AND istype = gv_istype.
      IF sy-subrc <> 0.

        MESSAGE ID gc_msg_class_precheck TYPE 'E' NUMBER 024 WITH iv_brsch INTO ls_error-message.
        ls_error-id         = sy-msgid.
        ls_error-type       = sy-msgty.
        ls_error-number     = sy-msgno.
        ls_error-message_v4 = iv_brsch. " sy-msgv4.
        ls_error-field      = 'INDUSTRY'.
        ls_error-parameter  = 'Branche'(035).
        INSERT ls_error INTO TABLE lt_errors.
      ENDIF.
    ENDIF.

    et_errors = lt_errors.

  ENDMETHOD.                    " check_industry

  METHOD fill_check_result_bp_from_ret2.
    DATA ls_qual_check_res TYPE /qchk/st_quality_check_hitlist.

    FIELD-SYMBOLS <ls_error> LIKE LINE OF it_errors.

    LOOP AT it_errors ASSIGNING <ls_error>.
      CLEAR ls_qual_check_res.
      ls_qual_check_res-msgid             = <ls_error>-id.
      ls_qual_check_res-sync_obj_sour_id  = iv_objnr.
      ls_qual_check_res-type              = <ls_error>-type.
      ls_qual_check_res-message           = <ls_error>-message.
      ls_qual_check_res-msgno             = <ls_error>-number.
      ls_qual_check_res-error_category    = <ls_error>-parameter.
      ls_qual_check_res-error_field_value = <ls_error>-message_v4.
      ls_qual_check_res-tech_fieldname    = <ls_error>-field.


      " Anzeige von Debitoren-/Kreditorennummer bei Fehler
*      IF iv_bp IS NOT INITIAL.
*        ls_qual_check_res-connected_bp = iv_bp.
*      ENDIF.
      " Anzeigen bereits vorhandener Geschäftspartner
*      IF iv_partner IS NOT INITIAL.
*        ls_qual_check_res-partner = iv_partner.
*      ENDIF.
      " End Change

      APPEND ls_qual_check_res TO gt_qual_check_result.

    ENDLOOP.
  ENDMETHOD.                    " fill_check_Result_bp_from_bapiret2

  METHOD initialize_customizing_data.

    DATA lv_object TYPE nrobj.

    CLEAR: gt_t005,
           gt_t077d,
           gt_t077k,
           gt_tzone,
           gt_nriv,
           gt_tfktaxnumtype,
           gv_istype.

    " Select countries
    SELECT land1 intca FROM t005 INTO CORRESPONDING FIELDS OF TABLE gt_t005.

    " Select account group specific data
    IF gs_sync_param-source-sync_obj_source = 'CUSTOMER'.
      SELECT ktokd numkr fausa FROM t077d INTO CORRESPONDING FIELDS OF TABLE gt_t077d.
    ELSEIF gs_sync_param-source-sync_obj_source = 'VENDOR'.
      SELECT ktokk numkr fausa FROM t077k INTO CORRESPONDING FIELDS OF TABLE gt_t077k.
    ENDIF.

    " Select titles
    SELECT person organizatn xgroup FROM tsad3 INTO CORRESPONDING FIELDS OF TABLE gt_tsad3.

    " Select transport zone
    SELECT land1 zone1 FROM tzone INTO CORRESPONDING FIELDS OF TABLE gt_tzone.

    IF gs_sync_param-source-sync_obj_source = 'CUSTOMER'.
      lv_object = 'DEBITOR'.
    ELSEIF gs_sync_param-source-sync_obj_source = 'VENDOR'.
      lv_object = 'KREDITOR'.
    ENDIF.
    SELECT object nrrangenr fromnumber tonumber FROM nriv
                                                INTO CORRESPONDING FIELDS OF TABLE gt_nriv
                                                WHERE object = lv_object.

    " Select tax type
    SELECT taxtype FROM tfktaxnumtype INTO CORRESPONDING FIELDS OF TABLE gt_tfktaxnumtype.

    " Select industry.
    SELECT istype FROM tb038 INTO gv_istype
      UP TO 1 ROWS
      WHERE istdef = abap_true
      ORDER BY PRIMARY KEY.
    ENDSELECT.


  ENDMETHOD.                    " initialize_customizing_data

  " -------------------------------------------------------------------------
  " Method get_partner_customer
  " -----------------------------------------------------------------------
  " Checks whether a business partner exists for a customer
  " -----------------------------------------------------------------------
  " no parameters
  " -----------------------------------------------------------------------

  METHOD get_partner_customer.
    " -------------
    DATA lt_partner_guid TYPE STANDARD TABLE OF bu_partner_guid.
    DATA lv_partner_guid TYPE bu_partner_guid.
    " ----- End of local data definition ------------------------------------

    SELECT partner_guid
      FROM cvi_cust_link
      INTO TABLE lt_partner_guid
      WHERE customer = iv_kunnr.

    IF sy-subrc <> 0.
      READ TABLE lt_partner_guid INTO lv_partner_guid INDEX 1.

      SELECT partner
        FROM but000
        INTO ev_partner
        WHERE partner_guid = lv_partner_guid.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.
  " ----------------- eo method get_partner_customer -----------------------------------

  " -------------------------------------------------------------------------
  " Method get_partner_vendor
  " -----------------------------------------------------------------------
  " Checks whether a business partner exists for a vendor
  " -----------------------------------------------------------------------
  " no parameters
  " -----------------------------------------------------------------------
  METHOD get_partner_vendor.
    " -------------
    DATA lt_partner_guid TYPE STANDARD TABLE OF bu_partner_guid.
    DATA lv_partner_guid TYPE bu_partner_guid.
    " ----- End of local data definition ------------------------------------

    SELECT partner_guid
      FROM cvi_vend_link
      INTO TABLE lt_partner_guid
      WHERE vendor = iv_lifnr.

    IF sy-subrc <> 0.
      READ TABLE lt_partner_guid INTO lv_partner_guid INDEX 1.
      SELECT partner
        FROM but000
        INTO ev_partner
        WHERE partner_guid = lv_partner_guid.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.
ENDCLASS.   " lcl_s4qual_check

**&---------------------------------------------------------------------*
**&       Class lcl_duplicate_check
**&---------------------------------------------------------------------*
CLASS lcl_duplicate_check IMPLEMENTATION.

  METHOD exec_check.

    DATA: lv_objtype   TYPE char1,
          ls_cust_data TYPE cmds_ei_main,
          ls_vend_data TYPE vmds_ei_main,
          lt_customers TYPE cmds_ei_extern_t,
          lt_vendors   TYPE vmds_ei_extern_t.

    FIELD-SYMBOLS <fs_data> TYPE any.

    " Define object type
    lv_objtype = gr_check->gv_obj_source.

    " cast structure to field-symbols
    ASSIGN is_data->* TO <fs_data>.

    IF gr_check->gv_obj_source = gc_objtype_cust.
      "     cast is_data to customer table
      ls_cust_data = <fs_data>.
      REFRESH lt_customers.
      APPEND LINES OF ls_cust_data-customers TO lt_customers.
    ELSE.
      " cast is_data to vendor table
      ls_vend_data = <fs_data>.
      REFRESH lt_vendors.
      APPEND LINES OF ls_vend_data-vendors TO lt_vendors.
    ENDIF.

    " execute duplicate check
    duplicate_check(
      EXPORTING
        it_customers = lt_customers
        it_vendors   = lt_vendors ).

  ENDMETHOD.                    "lcl_object_processing~excecute_check

  METHOD exec_processing_completion.

    DATA: lv_param_name            TYPE /qchk/cvi_size_param_name,
          lv_param_address         TYPE /qchk/cvi_size_param_addr,
          lv_param_ustid           TYPE /qchk/cvi_size_param_ustid,
          lv_duplicate_type        TYPE /qchk/st_duplicate_res-duplicate_type,
          lv_duplicate_probability TYPE /qchk/st_duplicate_res-duplicate_probability.

    FIELD-SYMBOLS: <fs_dupl> LIKE LINE OF gt_dupl_check_result,
                   <ls_size> LIKE LINE OF gt_tshirt_sizes.

    gt_dupl_check_result = gt_dupl_check_result_mthrd.

    " Result preparation for duplicate checks
    IF gt_dupl_check_result IS INITIAL.
      RETURN.
    ENDIF.

    IF gt_tshirt_sizes IS INITIAL.
      RETURN.
    ENDIF.

    SORT gt_dupl_check_result ASCENDING BY group_id.

    LOOP AT gt_dupl_check_result ASSIGNING <fs_dupl>.

      CLEAR: lv_duplicate_type,
             lv_duplicate_probability,
             lv_param_address,
             lv_param_name,
             lv_param_ustid.

      " for name
      IF <fs_dupl>-probability_name = 100.
        lv_param_name = 'EQ'.
      ENDIF.
      IF <fs_dupl>-probability_name = 0.
        lv_param_name = 'NE'.
      ENDIF.
      IF     <fs_dupl>-probability_name > 0
         AND <fs_dupl>-probability_name < 100.
        lv_param_name = 'CA'.
      ENDIF.

      " for address
      IF <fs_dupl>-probability_address = 100.
        lv_param_address = 'EQ'.
      ENDIF.
      IF <fs_dupl>-probability_address = 0.
        lv_param_address = 'NE'.
      ENDIF.
      IF     <fs_dupl>-probability_address > 0
         AND <fs_dupl>-probability_address < 100.
        lv_param_address = 'CA'.
      ENDIF.

      " for USTID
      IF <fs_dupl>-probability_ustid = 100.
        lv_param_ustid = 'EQ'.
      ENDIF.
      IF <fs_dupl>-probability_ustid = 0.
        lv_param_ustid = 'NE'.
      ENDIF.
      IF     <fs_dupl>-probability_ustid > 0
         AND <fs_dupl>-probability_ustid < 100.
        lv_param_ustid = 'CA'.
      ENDIF.

      " get duplicate type and t-shirt sizing
      READ TABLE gt_tshirt_sizes ASSIGNING <ls_size>
           WITH KEY option_name    = lv_param_name
                    option_address = lv_param_address
                    option_ustid   = lv_param_ustid.
      IF sy-subrc = 0.
        <fs_dupl>-duplicate_type        = <ls_size>-duplicate_type.
        <fs_dupl>-duplicate_probability = <ls_size>-duplicate_probability.

        lv_duplicate_type = <ls_size>-duplicate_type.
        lv_duplicate_probability = <ls_size>-duplicate_probability.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.                    "lcl_object_processing~execute_posprocessing

  METHOD exec_save.

    DATA: lv_tabix      TYPE i,
          lv_cnt        TYPE i,

          ls_run_stat   TYPE /qchk/st_cvi_run,
          ls_dupl_check TYPE /qchk/duplicate,

          lt_dupl_check TYPE TABLE OF /qchk/duplicate.


    FIELD-SYMBOLS: <ls_result_dupl> LIKE LINE OF gt_dupl_check_result.


    "Delete buffered data
    DELETE FROM DATABASE /qchk/indx_table(zc) ID gv_jobname.

    IF gt_dupl_check_result IS INITIAL.
      " Cancel the job
*      CALL FUNCTION 'BP_JOB_ABORT'
*        EXPORTING
*          jobcount = gv_jobcount                 " Job count
*          jobname  = gv_jobname.                 " Jobname
      RETURN.
    ENDIF.

    " Save results in DB
    CLEAR lv_cnt.
    LOOP AT gt_dupl_check_result ASSIGNING <ls_result_dupl>.
      MOVE-CORRESPONDING <ls_result_dupl> TO ls_dupl_check. "#EC ENHOK
      ls_dupl_check-timestamp = gv_timestamp.
      ls_dupl_check-unique_id = gv_uuid.
      ls_dupl_check-run_id = gv_runid.
      APPEND ls_dupl_check TO lt_dupl_check.
      ADD 1 TO lv_tabix.
      IF lv_tabix = 1000.
        INSERT /qchk/duplicate FROM TABLE lt_dupl_check.
        COMMIT WORK.
        CLEAR lv_tabix.
        REFRESH lt_dupl_check.
      ENDIF.

      AT LAST.
        IF lv_tabix > 0.
          INSERT /qchk/duplicate FROM TABLE lt_dupl_check.
          COMMIT WORK.
        ENDIF.
      ENDAT.
    ENDLOOP.

*   save run id
    ls_run_stat-mandt       = sy-mandt.
    ls_run_stat-run_id      = gv_runid.
    ls_run_stat-job_guid    = gv_uuid.
    ls_run_stat-object_type = gv_obj_source.
    ls_run_stat-jobcount    = gv_jobcount.
    ls_run_stat-jobname     = gv_jobname.
    ls_run_stat-timestamp   = gv_timestamp.
    ls_run_stat-created_by  = sy-uname.

    CASE gv_obj_source.
      WHEN gc_objtype_cust. " Customer
        ls_run_stat-object_type = gc_objtype_c.
      WHEN OTHERS. " Vendor
        ls_run_stat-object_type = gc_objtype_v.
    ENDCASE.

    UPDATE /qchk/check_run FROM ls_run_stat.
    COMMIT WORK.

  ENDMETHOD.                    "lcl_object_processing~save

  METHOD exec_initializing.

    " read customizing plausibility checks
    SELECT *
      FROM /qchk/dupl_plaus
      INTO TABLE gt_dupl_plaus
      WHERE plausi_active = abap_true.

    " T-Shirt Sizing for result preparation of duplicates
    SELECT *                                            "#EC CI_NOWHERE
      FROM /qchk/tshirt_siz
      INTO TABLE gt_tshirt_sizes.

    initialize_id_complete( ).

  ENDMETHOD.                    "lcl_object_processing~initializing

  METHOD duplicate_check.

    DATA: lv_thread18                TYPE char18,
          lv_thread30                TYPE char30,
          lv_search_str1             TYPE string,
          lv_lev_probability         TYPE /qchk/cvi_probability,
          lv_le_max                  TYPE boolean,
          lv_append_score            TYPE boolean,
          lv_is_duplicate            TYPE boolean,
          lv_duplicate_found         TYPE boolean,
          lv_computed                TYPE boolean,
          lv_computed_tmp            TYPE boolean,
          lv_probability             TYPE arch_int-fltp,
          lv_thread                  TYPE string,

          ls_cust_data               TYPE cmds_ei_main,
          ls_vend_data               TYPE vmds_ei_main,
          ls_errmsg                  TYPE c LENGTH 255,

          lt_dupl_check_result_tmp   TYPE /qchk/tt_duplicate_res_mthrd,
          lt_relevant_groups_threads TYPE /qchk/tt_duplicate_res_mthrd.

    FIELD-SYMBOLS: <ls_cust_data> TYPE cmds_ei_extern,
                   <ls_vend_data> TYPE vmds_ei_extern.

    ls_cust_data-customers = it_customers.
    ls_vend_data-vendors = it_vendors.

    IF gr_multi_thread IS INITIAL.
      " create multi threader
      gr_multi_thread = NEW #( i_threads = 10 ).

      " initial export of work table
      IF ls_cust_data IS NOT INITIAL.
        IF lines( gt_customers_compl ) > 0.
          EXPORT tab_work = gt_customers_compl_work[] TO DATABASE /qchk/indx_table(zc) ID gv_jobname.
          gt_customers_compl_work_cl[] = gt_customers_compl_work[].
        ENDIF.
      ENDIF.

      IF ls_vend_data IS NOT INITIAL.
        IF lines( gt_vendors_compl ) > 0.
          EXPORT tab_work = gt_vendors_compl_work[] TO DATABASE /qchk/indx_table(zc) ID gv_jobname.
          gt_vendors_compl_work_cl[] = gt_vendors_compl_work[].
        ENDIF.
      ENDIF.

    ENDIF.

    IF lines( gt_worked ) >= 1000.
      REFRESH gt_worked.
      " update work table in buffer
      DELETE FROM DATABASE /qchk/indx_table(zc) ID gv_jobname.

      IF ls_cust_data IS NOT INITIAL.
        IF lines( gt_customers_compl ) > 0.
          EXPORT tab_work = gt_customers_compl_work_cl[] TO DATABASE /qchk/indx_table(zc) ID gv_jobname.
        ENDIF.
      ENDIF.

      IF ls_vend_data IS NOT INITIAL.
        IF lines( gt_vendors_compl ) > 0.
          EXPORT tab_work = gt_vendors_compl_work_cl[] TO DATABASE /qchk/indx_table(zc) ID gv_jobname.
        ENDIF.
      ENDIF.

    ENDIF.

    IF ls_cust_data IS NOT INITIAL.
      IF lines( gt_customers_compl ) > 0.

        " clear gv_thread_count.

        LOOP AT ls_cust_data-customers ASSIGNING <ls_cust_data>.

          CLEAR: lv_search_str1,
          lv_probability,
          lv_is_duplicate,
          lv_duplicate_found,
          lv_computed,
          lv_computed_tmp,
          lv_le_max,
          lv_lev_probability,
          lv_append_score.

          " save work status
          change_work_status( iv_objnr = <ls_cust_data>-header-object_instance-kunnr ).
          APPEND <ls_cust_data>-header-object_instance-kunnr TO gt_worked.

          " start multithreading
          CLEAR: lv_thread,
          lv_thread18,
          lv_thread30.
          lv_thread = gr_multi_thread->get_free_thread( ).

          gv_thread_count = gv_thread_count + 1.
          lv_thread30 = gv_thread_count.
          SHIFT lv_thread30 LEFT DELETING LEADING space.
          CONCATENATE lv_thread30 lv_thread gv_jobname+8 INTO lv_thread30 SEPARATED BY '_'.

          REFRESH lt_dupl_check_result_tmp.
          REFRESH lt_relevant_groups_threads.
          " Processing of found duplicates
          collect_duplicates( ).

          EXPORT tab_worked = gt_worked[]
                 TO DATABASE /qchk/indx_table(zc) ID lv_thread30. " lv_thread18.

          CALL FUNCTION '/QCHK/CVI_DUPLICATE_CHECK' STARTING NEW TASK lv_thread
            CALLING gr_multi_thread->end_of_thread ON END OF TASK
            EXPORTING
              is_source_cust        = <ls_cust_data>
              iv_thread_id          = lv_thread30 " lv_thread18
              iv_duppadr            = gv_duppadr
              iv_duppnam            = gv_duppnam
              iv_duppustid          = gv_duppustid
              iv_dupg               = gv_dupg
              iv_jobname            = gv_jobname
            EXCEPTIONS
              communication_failure = 1 MESSAGE ls_errmsg
              system_failure        = 2 MESSAGE ls_errmsg
              resource_failure      = 3.

          IF sy-subrc <> 0.
            gr_multi_thread->handle_resource_failure( ).
          ENDIF.

        ENDLOOP.

        WAIT UNTIL gr_multi_thread->all_threads_are_finished( ) = abap_true.

        " Processing of found duplicates
        collect_duplicates( ).

      ENDIF.

    ENDIF.

    IF ls_vend_data IS NOT INITIAL.
      IF lines( gt_vendors_compl ) > 0.
        LOOP AT ls_vend_data-vendors ASSIGNING <ls_vend_data>.
          CLEAR: lv_search_str1,
          lv_probability,
          lv_is_duplicate,
          lv_duplicate_found,
          lv_computed,
          lv_computed_tmp,
          lv_le_max,
          lv_lev_probability.

          " save work status
          change_work_status( iv_objnr = <ls_vend_data>-header-object_instance-lifnr ).
          APPEND <ls_vend_data>-header-object_instance-lifnr TO gt_worked.

          "start multithreading
          CLEAR: lv_thread,
          lv_thread18,
          lv_thread30.

          lv_thread = gr_multi_thread->get_free_thread( ).

          gv_thread_count = gv_thread_count + 1.
          lv_thread30 = gv_thread_count.
          SHIFT lv_thread30 LEFT DELETING LEADING space.
          CONCATENATE lv_thread30 lv_thread gv_jobname+8 INTO lv_thread30 SEPARATED BY '_'.

          REFRESH lt_dupl_check_result_tmp.
          REFRESH lt_relevant_groups_threads.
          " Processing of found duplicates
          collect_duplicates( ).

          EXPORT tab_worked = gt_worked[]
                              TO DATABASE /qchk/indx_table(zc) ID lv_thread30. " lv_thread18.

          CALL FUNCTION '/QCHK/CVI_DUPLICATE_CHECK' STARTING NEW TASK lv_thread
            CALLING gr_multi_thread->end_of_thread ON END OF TASK
            EXPORTING
              is_source_vend        = <ls_vend_data>
              iv_thread_id          = lv_thread30
              iv_duppadr            = gv_duppadr
              iv_duppnam            = gv_duppnam
              iv_duppustid          = gv_duppustid
              iv_dupg               = gv_dupg
              iv_jobname            = gv_jobname
            EXCEPTIONS
              communication_failure = 1 MESSAGE ls_errmsg
              system_failure        = 2 MESSAGE ls_errmsg
              resource_failure      = 3.

          IF sy-subrc <> 0.
            gr_multi_thread->handle_resource_failure( ).
          ENDIF.

        ENDLOOP.

        WAIT UNTIL gr_multi_thread->all_threads_are_finished( ) = abap_true.

        " Processing of found duplicates
        collect_duplicates( ).

      ENDIF.
    ENDIF.
  ENDMETHOD.                    " duplicate_check

  METHOD build_check_string.

    DATA: lv_relative_name TYPE string,
          ls_cust_data     TYPE cmds_ei_extern,
          ls_vend_data     TYPE vmds_ei_extern,
          lt_check_strings TYPE TABLE OF gty_check_strings,
          ls_check_string  LIKE LINE OF lt_check_strings,
          lr_struct_descr  TYPE REF TO cl_abap_typedescr.

    TRY.
        lr_struct_descr =  cl_abap_structdescr=>describe_by_data( is_data ).
        lv_relative_name = lr_struct_descr->get_relative_name( ).

        IF lv_relative_name = 'CMDS_EI_EXTERN'.
          ls_cust_data = is_data.
        ENDIF.

        IF lv_relative_name = 'VMDS_EI_EXTERN'.
          ls_vend_data = is_data.
        ENDIF.

      CATCH cx_root.

    ENDTRY.

    CASE lv_relative_name.
      WHEN 'CMDS_EI_EXTERN'.

        " Name
        IF gv_duppnam IS NOT INITIAL.
          CLEAR ls_check_string.
          ls_check_string-descr = gc_descr_string_name. "'NAME'.
          IF    ls_cust_data-central_data-address-postal-data-name   IS NOT INITIAL
             OR ls_cust_data-central_data-address-postal-data-name_2 IS NOT INITIAL
             OR ls_cust_data-central_data-address-postal-data-name_3 IS NOT INITIAL
             OR ls_cust_data-central_data-address-postal-data-name_4 IS NOT INITIAL.
            CONCATENATE ls_cust_data-central_data-address-postal-data-name
                        ls_cust_data-central_data-address-postal-data-name_2
                        ls_cust_data-central_data-address-postal-data-name_3
                        ls_cust_data-central_data-address-postal-data-name_4
                        INTO ls_check_string-string SEPARATED BY space.

            " save original string
            ls_check_string-original_string = ls_check_string-string.

            TRANSLATE ls_check_string-string TO UPPER CASE.
*          REPLACE ALL OCCURRENCES OF REGEX '-|/|&' IN ls_check_string-string WITH space.
            REPLACE ALL OCCURRENCES OF REGEX '(-|/|&|;|:)' IN ls_check_string-string WITH '_'.
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `.`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `(`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `)`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `_`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `$`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `+`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `\`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `*`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `^`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `&`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `,`
                                                to   = ` ` ).

            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GMBH CO KGAA\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GMBH CO KG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GMBH CO\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GMBH\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<MBH\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<SE CO KGAA\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<SE CO KG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<CO KGAA\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<CO KG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
*            REPLACE REGEX '\<CO  KG\>' IN ls_check_string-string WITH '_'.
*            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<AG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<KG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<E K\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<KGAA\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<LTD\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<UG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<OHG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GBR\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `_`
                                                to   = ` ` ).

            " delete leading & trailing
            CONDENSE ls_check_string-string.
            CONDENSE ls_check_string-original_string.

          ENDIF.
          APPEND ls_check_string TO lt_check_strings.
        ENDIF.

        " Address
        IF gv_duppadr IS NOT INITIAL.
          CLEAR ls_check_string.
          ls_check_string-descr = gc_descr_string_address. "'ADDRESS'.
          IF    ls_cust_data-central_data-address-postal-data-city       IS NOT INITIAL
             OR ls_cust_data-central_data-address-postal-data-postl_cod1 IS NOT INITIAL
             OR ls_cust_data-central_data-address-postal-data-street     IS NOT INITIAL
             OR ls_cust_data-central_data-address-postal-data-house_no   IS NOT INITIAL.
            CONCATENATE ls_cust_data-central_data-address-postal-data-city
                        ls_cust_data-central_data-address-postal-data-postl_cod1
                        ls_cust_data-central_data-address-postal-data-street
                        ls_cust_data-central_data-address-postal-data-house_no
                        INTO ls_check_string-string SEPARATED BY space.

            " save original string
            ls_check_string-original_string = ls_check_string-string.

            TRANSLATE ls_check_string-string TO UPPER CASE.
*          REPLACE ALL OCCURRENCES OF REGEX '-|/|&' IN ls_check_string-string WITH space.
            REPLACE ALL OCCURRENCES OF REGEX '(-|/|&|;|:)' IN ls_check_string-string WITH '_'.
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `.`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `(`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `)`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `_`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `$`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `+`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `\`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `*`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `^`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `&`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `,`
                                                to   = ` ` ).

            REPLACE REGEX 'STRAßE|STRASSE|STRASE|STR' IN ls_check_string-string WITH '_'.
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `_`
                                                to   = ` ` ).

            " delete leading & trailing
            CONDENSE ls_check_string-string.
            CONDENSE ls_check_string-original_string.

          ENDIF.
          APPEND ls_check_string TO lt_check_strings.
        ENDIF.

        " Ust-ID
        IF gv_duppustid IS NOT INITIAL.
          CLEAR ls_check_string.
          ls_check_string-descr = gc_descr_string_ustid. "'USTID'.
          IF ls_cust_data-central_data-central-data-stceg IS NOT INITIAL.
            ls_check_string-string          = ls_cust_data-central_data-central-data-stceg.
            ls_check_string-original_string = ls_check_string-string.
            TRANSLATE ls_check_string-string TO UPPER CASE.
          ENDIF.
          APPEND ls_check_string TO lt_check_strings.
        ENDIF.

      WHEN 'VMDS_EI_EXTERN'.

        " Name
        IF gv_duppnam IS NOT INITIAL.
          CLEAR ls_check_string.
          ls_check_string-descr = gc_descr_string_name. "'NAME'.
          IF    ls_vend_data-central_data-address-postal-data-name   IS NOT INITIAL
             OR ls_vend_data-central_data-address-postal-data-name_2 IS NOT INITIAL
             OR ls_vend_data-central_data-address-postal-data-name_3 IS NOT INITIAL
             OR ls_vend_data-central_data-address-postal-data-name_4 IS NOT INITIAL.
            CONCATENATE ls_vend_data-central_data-address-postal-data-name
                        ls_vend_data-central_data-address-postal-data-name_2
                        ls_vend_data-central_data-address-postal-data-name_3
                        ls_vend_data-central_data-address-postal-data-name_4
                        INTO ls_check_string-string SEPARATED BY space.

            " save original string
            ls_check_string-original_string = ls_check_string-string.

            TRANSLATE ls_check_string-string TO UPPER CASE.
*          REPLACE ALL OCCURRENCES OF REGEX '-|/|&' IN ls_check_string-string WITH space.
            REPLACE ALL OCCURRENCES OF REGEX '(-|/|&|;|:)' IN ls_check_string-string WITH '_'.
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `.`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `(`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `)`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `_`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `$`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `+`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `\`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `*`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `^`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `&`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `,`
                                                to   = ` ` ).

            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GMBH CO KGAA\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GMBH CO KG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GMBH CO\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GMBH\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<MBH\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<SE CO KGAA\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<SE CO KG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<CO KGAA\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<CO KG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
*            REPLACE REGEX '\<CO  KG\>' IN ls_check_string-string WITH '_'.
*            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<AG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<KG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<E K\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<KGAA\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<LTD\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<UG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<OHG\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<GBR\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `_`
                                                to   = ` ` ).

            " delete leading & trailing
            CONDENSE ls_check_string-string.
            CONDENSE ls_check_string-original_string.

          ENDIF.
          APPEND ls_check_string TO lt_check_strings.
        ENDIF.

        " Address
        IF gv_duppadr IS NOT INITIAL.
          CLEAR ls_check_string.
          ls_check_string-descr = gc_descr_string_address. " 'ADDRESS'.
          IF    ls_vend_data-central_data-address-postal-data-city       IS NOT   INITIAL
             OR ls_vend_data-central_data-address-postal-data-postl_cod1 IS NOT INITIAL
             OR ls_vend_data-central_data-address-postal-data-street     IS NOT INITIAL
             OR ls_vend_data-central_data-address-postal-data-house_no   IS NOT INITIAL.
            CONCATENATE ls_vend_data-central_data-address-postal-data-city
                        ls_vend_data-central_data-address-postal-data-postl_cod1
                        ls_vend_data-central_data-address-postal-data-street
                        ls_vend_data-central_data-address-postal-data-house_no
                        INTO ls_check_string-string SEPARATED BY space.
            " save original string
            ls_check_string-original_string = ls_check_string-string.

            TRANSLATE ls_check_string-string TO UPPER CASE.
            REPLACE ALL OCCURRENCES OF REGEX '(-|/|&|;|:)' IN ls_check_string-string WITH '_'.
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `.`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `(`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `)`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `_`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `$`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `+`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `\`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `*`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `^`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `&`
                                                to   = ` ` ).
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `,`
                                                to   = ` ` ).

            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<STRAßE\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<STRASSE\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<STRASE\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            REPLACE REGEX '\<STR\>' IN ls_check_string-string WITH '_'.
            CONDENSE ls_check_string-string.
            ls_check_string-string = translate( val  = ls_check_string-string
                                                from = `_`
                                                to   = ` ` ).

            " delete leading & trailing
            CONDENSE ls_check_string-string.
            CONDENSE ls_check_string-original_string.
          ENDIF.
          APPEND ls_check_string TO lt_check_strings.
        ENDIF.

        " Ust-ID
        IF gv_duppustid IS NOT INITIAL.
          CLEAR ls_check_string.
          ls_check_string-descr = gc_descr_string_ustid. "'USTID'.
          IF ls_vend_data-central_data-central-data-stceg IS NOT INITIAL.
            ls_check_string-string          = ls_vend_data-central_data-central-data-stceg.
            ls_check_string-original_string = ls_check_string-string.
            TRANSLATE ls_check_string-string TO UPPER CASE.
          ENDIF.
          APPEND ls_check_string TO lt_check_strings.
        ENDIF.

    ENDCASE.

    it_check_strings[] = lt_check_strings[].

  ENDMETHOD.                    "build_check_String

  METHOD plausi_check.

    DATA: lv_apply_rule        TYPE boole_d,
          lv_applies_address   TYPE boole_d,
          lv_applies_name      TYPE boole_d,
          lv_rule_address      TYPE boole_d,
          lv_rule_ustid        TYPE boole_d,
          lv_rule_name         TYPE boole_d,
          lv_applies_ustid     TYPE boole_d,
          lv_show_result       TYPE boole_d,

          lt_probability_range TYPE RANGE OF /qchk/cvi_probability,
          ls_probability_range LIKE LINE OF lt_probability_range.

    FIELD-SYMBOLS <ls_plausi> LIKE LINE OF gt_dupl_plaus.

    " plausibility check
    LOOP AT gt_dupl_plaus ASSIGNING <ls_plausi>.
      CLEAR: lv_applies_name,
             lv_applies_address,
             lv_applies_ustid.
      CLEAR: lv_rule_name,
             lv_rule_address,
             lv_rule_ustid.

      IF <ls_plausi>-plausi_name = abap_true.
        REFRESH lt_probability_range.
        ls_probability_range-sign   = 'I'.
        ls_probability_range-option = <ls_plausi>-plausi_name_option.
        ls_probability_range-low    = <ls_plausi>-plausi_name_perc.
        APPEND ls_probability_range TO lt_probability_range.
        lv_rule_name = abap_true.
*        IF gs_check_result_dupl2->probability_name IN lt_probability_range.
        IF is_check_data->probability_name IN lt_probability_range.
          lv_applies_name = abap_true.
        ENDIF.
      ENDIF.
      IF <ls_plausi>-plausi_address = abap_true.
        REFRESH lt_probability_range.
        ls_probability_range-sign   = 'I'.
        ls_probability_range-option = <ls_plausi>-plausi_address_option.
        ls_probability_range-low    = <ls_plausi>-plausi_address_perc.
        APPEND ls_probability_range TO lt_probability_range.
        lv_rule_address = abap_true.
*        IF gs_check_result_dupl2->probability_address IN lt_probability_range.
        IF is_check_data->probability_address IN lt_probability_range.
          lv_applies_address = abap_true.
        ENDIF.
      ENDIF.
      IF <ls_plausi>-plausi_ustid = abap_true.
        REFRESH lt_probability_range.
        ls_probability_range-sign   = 'I'.
        ls_probability_range-option = <ls_plausi>-plausi_ustid_option.
        ls_probability_range-low    = <ls_plausi>-plausi_ustid_perc.
        APPEND ls_probability_range TO lt_probability_range.
        lv_rule_ustid = abap_true.
*        IF gs_check_result_dupl2->probability_ustid = 100.
        IF is_check_data->probability_ustid = 100.
          lv_applies_ustid = abap_true.
        ENDIF.
      ENDIF.

      " apply rule?
      lv_apply_rule = abap_true.
      IF lv_rule_name = abap_true AND lv_applies_name = abap_false.
        CLEAR lv_apply_rule.
      ENDIF.
      IF lv_rule_address = abap_true AND lv_applies_address = abap_false.
        CLEAR lv_apply_rule.
      ENDIF.
      IF lv_rule_ustid = abap_true AND lv_applies_ustid = abap_false.
        CLEAR lv_apply_rule.
      ENDIF.

      " show result?
      IF lv_apply_rule = abap_true.
        lv_show_result = <ls_plausi>-show_result.

        "--> rule found
        EXIT.
      ENDIF.
    ENDLOOP.

    ev_show_result = lv_show_result.

  ENDMETHOD.                    "plausi_check

  METHOD append_score.

    DATA: lv_show_result   TYPE boole_d,
          lv_group_id      TYPE i,
          lv_last_group_id TYPE i.

    lv_last_group_id = gs_current_group-group_id.

    " create group id, based on data
    lv_group_id = create_group_id( is_source = gs_check_result_source
                                   is_target = gs_check_result_source ).

    IF     gs_check_result_source IS NOT INITIAL
       AND gs_check_result_source IS NOT INITIAL.
      plausi_check( EXPORTING is_check_data  = gs_check_result_source
                    IMPORTING ev_show_result = lv_show_result ).
      IF lv_show_result = abap_true.
        IF lv_group_id <> lv_last_group_id.
          gs_check_result_source->group_id = lv_group_id.
          APPEND gs_check_result_source->* TO gt_dupl_check_result.
        ENDIF.
        gs_check_result_source->group_id = lv_group_id.
        APPEND gs_check_result_source->* TO gt_dupl_check_result.
      ENDIF.

      clear_score( ).
    ENDIF.


  ENDMETHOD.                    "append_score

  METHOD initialize_id_complete.

    DATA: lv_relative_name  TYPE string,
          lv_flg_last_block TYPE comt_boolean,
          ls_data           TYPE REF TO data,

          ls_error_objects  TYPE mds_ctrls_error_objects ##NEEDED,
          ls_cust_data      TYPE cmds_ei_main,
          ls_vend_data      TYPE vmds_ei_main,
          ls_sync_param     TYPE mdst_sync_parameter,

          lt_customers      TYPE cmds_ei_extern_t,
          lt_vendors        TYPE vmds_ei_extern_t,
          lt_object_id      TYPE mdst_sync_object_id_tab,
          lt_object_id_part TYPE mdst_sync_object_id_tab,
          lr_struct_descr   TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS <fs_data> TYPE any.

    " ---------------------------
    " Initialize object selection
    " ---------------------------
    ls_sync_param = gs_sync_param.
    REFRESH ls_sync_param-sel_criteria-sel_options.

    TRY.
        gr_ref_extractor->initialize_id( EXPORTING iv_source_object = ls_sync_param-source-sync_obj_source
                                                   it_sel_criteria  = ls_sync_param-sel_criteria
                                         IMPORTING et_messages      = ls_error_objects ).

      CATCH cx_mds_extractor ##NO_HANDLER.
    ENDTRY.

    " -----------------------------------------
    " Select all data
    " -----------------------------------------

    " Caution here - a maximum of 100000 records are selected in one call -
    " LV_FLG_LAST_BLOCK indicates whether  everything has been selected. So here is a while loop
    WHILE lv_flg_last_block IS INITIAL.
      " Extract object ids
      TRY.
          gr_ref_extractor->extract_id( IMPORTING et_object_id  = lt_object_id
                                                  ev_last_block = lv_flg_last_block
                                                  et_messages   = ls_error_objects ).

        CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*        MESSAGE x002 WITH 'extract_id' is_control-source-extract_class.
        CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
*        MESSAGE x005 WITH is_control-source-extract_class.
        CATCH cx_mds_extractor ##NO_HANDLER.
*        MESSAGE x006 WITH 'CX_MDS_EXTRACTOR' 'extract_id' is_control-source-extract_class.
      ENDTRY.

      WHILE lt_object_id[] IS NOT INITIAL.

        " Block Handling for inner loop
        CLEAR lt_object_id_part[].
        APPEND LINES OF lt_object_id FROM 1 TO gs_sync_param-data-block_size
               TO lt_object_id_part.
        DELETE lt_object_id FROM 1 TO gs_sync_param-data-block_size.

        " ----------------------------
        " Initialize object extraction
        " ----------------------------
        TRY.
            gr_ref_extractor->initialize( EXPORTING iv_source_object = gs_sync_param-source-sync_obj_source
                                                    iv_block_size    = gs_sync_param-data-block_size
                                                    it_object_id     = lt_object_id_part
                                          IMPORTING et_messages      = ls_error_objects ).

          CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*      MESSAGE x002 WITH 'initialize' is_control-source-extract_class.
          CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
*      MESSAGE x005 WITH is_control-source-extract_class.
          CATCH cx_mds_extractor ##NO_HANDLER.
*      MESSAGE x006 WITH 'CX_MDS_EXTRACTOR' 'initialize' is_control-source-extract_class.
        ENDTRY.

        " ------------------------------------
        " Extract object data from object list
        " ------------------------------------
        TRY.
            gr_ref_extractor->extract_data( EXPORTING iv_source_object = gs_sync_param-source-sync_obj_source
                                                      iv_sel_kind      = gs_sync_param-data-sel_kind
                                                      it_object_id     = lt_object_id_part " gs_sync_param-object_list
                                            IMPORTING es_data          = ls_data
                                                      et_messages      = ls_error_objects ).

            ASSIGN ls_data->* TO <fs_data>.
            IF sy-subrc <> 0.
              EXIT.
*    MESSAGE x003.
            ENDIF.

            TRY.
                lr_struct_descr =  cl_abap_structdescr=>describe_by_data( <fs_data> ).
                lv_relative_name = lr_struct_descr->get_relative_name( ).

                IF lv_relative_name = 'CMDS_EI_MAIN'.
                  ls_cust_data = <fs_data>.
                  APPEND LINES OF ls_cust_data-customers TO lt_customers.
                ENDIF.

                IF lv_relative_name = 'VMDS_EI_MAIN'.
                  ls_vend_data = <fs_data>.
                  APPEND LINES OF ls_vend_data-vendors TO lt_vendors.
                ENDIF.

              CATCH cx_root.

            ENDTRY.

          CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
*      MESSAGE x002 WITH 'extract_data' is_control-source-extract_class.
          CATCH cx_sy_dyn_call_illegal_class ##NO_HANDLER.
*      MESSAGE x005 WITH is_control-source-extract_class.
          CATCH cx_mds_extractor ##NO_HANDLER.
*      MESSAGE x006 WITH 'CX_MDS_EXTRACTOR' 'extract_data' is_control-source-extract_class.
        ENDTRY.
      ENDWHILE. " lt_object_id[]

    ENDWHILE. " lv_flg_last_block

    IF lines( lt_customers ) > 0.
      gt_customers_compl[] = lt_customers[].
      gt_customers_compl_work[] = lt_customers[].
    ENDIF.

    IF lines( lt_vendors ) > 0.
      gt_vendors_compl[] = lt_vendors[].
      gt_vendors_compl_work[] = lt_vendors[].
    ENDIF.

  ENDMETHOD.                    " initialize_id_for_dup_check

  METHOD change_work_status.

    " delete processed entries from work table
    IF gt_customers_compl IS NOT INITIAL.

      DELETE gt_customers_compl_work_cl WHERE header-object_instance-kunnr = iv_objnr.
      EXIT.

    ENDIF.

    IF gt_vendors_compl_work IS NOT INITIAL.

      DELETE gt_vendors_compl_work_cl WHERE header-object_instance-lifnr = iv_objnr.
      EXIT.
    ENDIF.

  ENDMETHOD.                    " change_work_status


  METHOD calculate_similarity_score.

    DATA: lv_lev_distance    TYPE i,
          lv_lev_probability TYPE p LENGTH 8 DECIMALS 2,
          lv_max             TYPE i.

    CLEAR ev_lev_probability.
    CLEAR ev_le_max.

    IF iv_search_str1 IS INITIAL AND iv_search_str2 IS INITIAL.
      ev_le_max = abap_false.
      EXIT.
    ENDIF.

    IF strlen( iv_search_str1 ) > strlen( iv_search_str2 ).
      lv_max = strlen( iv_search_str1 ).
    ELSE.
      lv_max = strlen( iv_search_str2 ).
    ENDIF.
    lv_max = ( 100 - gv_dupg ) * lv_max / 100 + 1.
    TRY.
        lv_lev_distance = distance( val1 = iv_search_str2 val2 = iv_search_str1 max = lv_max ).
      CATCH cx_sy_strg_par_val.
        EXIT. " No Probability due to LV_MAX error
    ENDTRY.

    IF strlen( iv_search_str1 ) > strlen( iv_search_str2 ).
      lv_lev_probability = ( 1 - lv_lev_distance / strlen( iv_search_str1 ) ) * 100.
    ELSE.
      lv_lev_probability = ( 1 - lv_lev_distance / strlen( iv_search_str2 ) ) * 100.
    ENDIF.

    IF lv_lev_probability >= gv_dupg. " lv_lev_distance < lv_max.
      ev_le_max = abap_true.
      ev_lev_probability = lv_lev_probability.
    ENDIF.

  ENDMETHOD.                    "calculate_similarity_score

  METHOD save_score.

    " check group id if changed
    IF gs_check_result_source IS INITIAL
    OR gs_check_result_source IS INITIAL.
      CREATE DATA gs_check_result_source.
      CREATE DATA gs_check_result_source.

      gs_check_result_source->sync_obj_sour_id = iv_src.
      gs_check_result_source->sync_obj_sour_id = iv_trg.
    ENDIF.

    "save results of comparison
    CASE iv_field_param.
      WHEN gc_descr_string_address. " 'ADDRESS'.
        gs_check_result_source->probability_address = iv_probability.
        gs_check_result_source->probability_address = iv_probability.
        gs_check_result_source->adress_src = iv_string_src.
        gs_check_result_source->adress_src = iv_string_trg.
      WHEN gc_descr_string_name. "'NAME'.
        gs_check_result_source->probability_name = iv_probability.
        gs_check_result_source->probability_name = iv_probability.
        gs_check_result_source->name_src = iv_string_src.
        gs_check_result_source->name_src = iv_string_trg.
      WHEN gc_descr_string_ustid. "'USTID'.
        gs_check_result_source->probability_ustid = iv_probability.
        gs_check_result_source->probability_ustid = iv_probability.
        gs_check_result_source->ustid_src = iv_string_src.
        gs_check_result_source->ustid_src = iv_string_trg.
    ENDCASE.




  ENDMETHOD.                    "save_score

  METHOD clear_score.

    CLEAR gs_check_result_source.
    CLEAR gs_check_result_source.

  ENDMETHOD.                    "clear_score

  METHOD create_group_id.


    IF check_group_id_changed( is_source = is_source
                               is_target = is_target ) EQ abap_true.
      ADD 1 TO gv_group_id.

      gs_current_group-group_id = gv_group_id.
      gs_current_group-source = is_source->sync_obj_sour_id.
      gs_current_group-target = is_target->sync_obj_sour_id.

    ENDIF.


    rv_group_id = gv_group_id.

  ENDMETHOD.                    "create_group_id

  METHOD check_group_id_changed.

    CLEAR rv_result.


    IF gv_group_id IS INITIAL.
      rv_result = abap_true.
      EXIT.
    ENDIF.

    IF is_source->sync_obj_sour_id NE gs_current_group-source.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.                    "check_group_id_changed

  METHOD collect_duplicates.


    DATA: lv_length            TYPE i,
          lv_lines_result      TYPE i,
          lv_current_group_id  TYPE i,
          lv_current_thread_id TYPE char18,
          lv_new_group_id      TYPE i,
          lv_tmp_group_id      TYPE i,
          lv_tmp_thread_id     TYPE char18,
          lv_deleted           TYPE boolean,
          lv_tabix             TYPE sy-tabix,
          lv_del_count         TYPE i,

          ls_temp_res_del      TYPE /qchk/tt_duplicate_res_mthrd,

          lt_temp_results      TYPE /qchk/tt_duplicate_res_mthrd.

    FIELD-SYMBOLS: <ls_temp_res>       LIKE LINE OF lt_temp_results,
                   <ls_temp_res_del>   LIKE LINE OF lt_temp_results,
                   <ls_temp_res_tmp>   LIKE LINE OF lt_temp_results,
                   <ls_temp_res_group> LIKE LINE OF lt_temp_results.

    gr_multi_thread->get_temp_dupl_results( IMPORTING et_temp_results = lt_temp_results ).

    " Gruppen mit gleicher Länge untersuchen, dies trifft dann zu, wenn zwei gleichzeitig gestartete Threads die gleiche Menge zurückliefern können
    SORT lt_temp_results BY group_id
                            thread_id
                            group_length ASCENDING.

    CLEAR lv_current_group_id.
    CLEAR lv_current_thread_id.
    CLEAR ls_temp_res_del.

    LOOP AT lt_temp_results ASSIGNING <ls_temp_res> WHERE     probability_name    = 0
                                                          AND probability_address = 0
                                                          AND probability_ustid   = 0.
      CLEAR ls_temp_res_del.
      CLEAR lv_deleted.
      CLEAR lv_length.
      CLEAR lv_del_count.

      LOOP AT lt_temp_results ASSIGNING <ls_temp_res_tmp> WHERE     sync_obj_sour_id  = <ls_temp_res>-sync_obj_sour_id
                                                                AND thread_id        <> <ls_temp_res>-thread_id
                                                                AND group_length     >= <ls_temp_res>-group_length.

        LOOP AT lt_temp_results ASSIGNING <ls_temp_res_group> WHERE     sync_obj_sour_id <> <ls_temp_res>-sync_obj_sour_id
                                                                    AND group_id          = <ls_temp_res>-group_id
                                                                    AND thread_id         = <ls_temp_res>-thread_id.
          lv_tabix = sy-tabix.
          READ TABLE lt_temp_results ASSIGNING <ls_temp_res_del> WITH KEY sync_obj_sour_id    = <ls_temp_res_group>-sync_obj_sour_id
                                                                          group_id            = <ls_temp_res_tmp>-group_id
                                                                          thread_id           = <ls_temp_res_tmp>-thread_id
                                                                          probability_name    = <ls_temp_res_group>-probability_name
                                                                          probability_address = <ls_temp_res_group>-probability_address
                                                                          probability_ustid   = <ls_temp_res_group>-probability_ustid.
          IF sy-subrc = 0.
            DELETE lt_temp_results INDEX lv_tabix.
            lv_del_count = lv_del_count + 1.

          ENDIF.

        ENDLOOP.

        IF lv_del_count = ( <ls_temp_res>-group_length - 1 ).
          DELETE lt_temp_results WHERE     sync_obj_sour_id = <ls_temp_res>-sync_obj_sour_id
                                       AND group_id         = <ls_temp_res>-group_id
                                       AND thread_id        = <ls_temp_res>-thread_id.
        ENDIF.

        EXIT.

      ENDLOOP.
    ENDLOOP.

*    """Schritt 2: mit bisherigen Ergebnisse vermischen und bereinigen
    IF lines( gt_dupl_check_result_mthrd ) < 1.
      " erst einmal nichts
    ELSE.

      SORT lt_temp_results BY group_id
                              thread_id
                              group_length ASCENDING.
      SORT gt_dupl_check_result_mthrd BY group_id
                                         thread_id
                                         group_length ASCENDING.

      CLEAR lv_current_group_id.
      CLEAR lv_current_thread_id.

      LOOP AT lt_temp_results ASSIGNING <ls_temp_res> WHERE     probability_name    = 0
                                                            AND probability_address = 0
                                                            AND probability_ustid   = 0.
        CLEAR ls_temp_res_del.
        CLEAR lv_deleted.
        CLEAR lv_length.
        CLEAR lv_del_count.

        LOOP AT gt_dupl_check_result_mthrd ASSIGNING <ls_temp_res_tmp> WHERE     sync_obj_sour_id  = <ls_temp_res>-sync_obj_sour_id
                                                                             AND thread_id        <> <ls_temp_res>-thread_id
                                                                             AND group_length     >= <ls_temp_res>-group_length.

          LOOP AT lt_temp_results ASSIGNING <ls_temp_res_group> WHERE     sync_obj_sour_id <> <ls_temp_res>-sync_obj_sour_id
                                                                      AND group_id          = <ls_temp_res>-group_id
                                                                      AND thread_id         = <ls_temp_res>-thread_id.
            lv_tabix = sy-tabix.
            READ TABLE gt_dupl_check_result_mthrd ASSIGNING <ls_temp_res_del> WITH KEY sync_obj_sour_id    = <ls_temp_res_group>-sync_obj_sour_id
                                                                                       group_id            = <ls_temp_res_tmp>-group_id
                                                                                       thread_id           = <ls_temp_res_tmp>-thread_id
                                                                                       probability_name    = <ls_temp_res_group>-probability_name
                                                                                       probability_address = <ls_temp_res_group>-probability_address
                                                                                       probability_ustid   = <ls_temp_res_group>-probability_ustid.
            IF sy-subrc = 0.
              DELETE lt_temp_results INDEX lv_tabix.
              lv_del_count = lv_del_count + 1.
            ENDIF.

          ENDLOOP.

          IF lv_del_count = ( <ls_temp_res>-group_length - 1 ).
            DELETE lt_temp_results WHERE     sync_obj_sour_id = <ls_temp_res>-sync_obj_sour_id
                                         AND group_id         = <ls_temp_res>-group_id
                                         AND thread_id        = <ls_temp_res>-thread_id.
          ENDIF.

          EXIT.

        ENDLOOP.
      ENDLOOP.

    ENDIF.

    IF lines( lt_temp_results ) > 0.
*      "Schritt 3: nach Bereinigung Gruppen Ids vergeben
*      letzte ID nummer holen und umschlüsseln -> danach an original tabelle anhängen
      SORT gt_dupl_check_result_mthrd BY group_id ASCENDING.
      SORT lt_temp_results BY group_id
                              thread_id ASCENDING.

      CLEAR lv_current_group_id.

      lv_lines_result = lines( gt_dupl_check_result_mthrd ).
      READ TABLE gt_dupl_check_result_mthrd INDEX lv_lines_result ASSIGNING <ls_temp_res>.
      IF sy-subrc = 0.
        lv_current_group_id = <ls_temp_res>-group_id.
      ENDIF.

      LOOP AT lt_temp_results ASSIGNING <ls_temp_res>.

        IF lv_tmp_group_id IS INITIAL.
          lv_tmp_group_id = <ls_temp_res>-group_id.
          lv_tmp_thread_id = <ls_temp_res>-thread_id.
        ENDIF.

        IF lv_new_group_id IS INITIAL.
          lv_new_group_id = lv_current_group_id + 1.
        ELSE.
          IF    lv_tmp_group_id  <> <ls_temp_res>-group_id
             OR lv_tmp_thread_id <> <ls_temp_res>-thread_id.
            lv_new_group_id = lv_new_group_id + 1.

            lv_tmp_group_id = <ls_temp_res>-group_id.
            lv_tmp_thread_id = <ls_temp_res>-thread_id.
          ENDIF.
        ENDIF.

        <ls_temp_res>-group_id = lv_new_group_id.

      ENDLOOP.

*
*    "Tabelle mit neuen Dupletten-Kombis erweitern
      APPEND LINES OF lt_temp_results TO gt_dupl_check_result_mthrd.
    ENDIF.
*
*    "--->>> temporäre Tabelle mit Länge der Gruppen löschen
    gr_multi_thread->clear_temp_results( ).
  ENDMETHOD.                    " collect_Duplicates



ENDCLASS.                    "lcl_duplicate_check IMPLEMENTATION


**&---------------------------------------------------------------------*
**&       lcl_multi_thread
**&---------------------------------------------------------------------*
CLASS lcl_multi_thread IMPLEMENTATION.

  METHOD get_temp_dupl_results.


    et_temp_results = gt_dupl_check_result_tmp.

  ENDMETHOD.                    "get_temp_dupl_Results

  METHOD clear_temp_results.

    REFRESH gt_dupl_check_result_tmp.

  ENDMETHOD.                    "clear_Temp_results

  METHOD end_of_thread.
    DATA: lv_task              TYPE char8,
          lv_errmsg            TYPE c LENGTH 255,
          lt_check_result_dupl TYPE /qchk/tt_duplicate_res_mthrd.

    lv_task = p_task.
    clear_thread( lv_task ).

    RECEIVE RESULTS FROM FUNCTION '/QCHK/CVI_DUPLICATE_CHECK'
            IMPORTING et_check_result_dupl = lt_check_result_dupl
            EXCEPTIONS communication_failure = 1 MESSAGE lv_errmsg
                              system_failure = 2 MESSAGE lv_errmsg.

    IF sy-subrc = 0.
      APPEND LINES OF lt_check_result_dupl TO gt_dupl_check_result_tmp.
    ENDIF.


  ENDMETHOD.                    "end_of_thread

  METHOD get_free_threads.
    " Get number of free threads
    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = me->gv_group
      IMPORTING
        free_pbt_wps                   = rv_free_threads
      EXCEPTIONS
        invalid_group_name             = 1
        internal_error                 = 2
        pbt_env_already_initialized    = 3
        currently_no_resources_avail   = 4
        no_pbt_resources_found         = 5
        cant_init_different_pbt_groups = 6
        OTHERS                         = 7.

    CASE sy-subrc.
      WHEN 0. " Do nothing

      WHEN 3.
        " Already initialised - get current number of free threads
        CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
          IMPORTING
            free_pbt_wps                = rv_free_threads
          EXCEPTIONS
            internal_error              = 1
            pbt_env_not_initialized_yet = 2
            OTHERS                      = 3.

        IF sy-subrc IS NOT INITIAL.
          " Something has gone seriously wrong, so end it here.
          MESSAGE ID sy-msgid TYPE 'X' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN OTHERS.
        " Something has gone seriously wrong, so end it here.
        MESSAGE ID sy-msgid TYPE 'X' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    ENDCASE.
  ENDMETHOD.                    "get_free_threads

  METHOD all_threads_are_finished.

    IF gv_used_threads EQ 0.
      rv_empty = abap_true.
      CLEAR me->gv_count.
    ELSE.
      rv_empty = abap_false.
    ENDIF.

  ENDMETHOD.                    "all_threads_are_finished

  METHOD clear_thread.

    FIELD-SYMBOLS <ls_thread> LIKE LINE OF me->gt_threads_list.

    READ TABLE me->gt_threads_list WITH KEY used = abap_true thread = i_task
         ASSIGNING <ls_thread>.
    IF sy-subrc EQ 0.
      <ls_thread>-used = abap_false.
      SUBTRACT 1 FROM gv_used_threads.
    ENDIF.

  ENDMETHOD.                    "clear_thread


  METHOD constructor.

    DATA: lv_free_threads TYPE i,
          lv_threadn      TYPE i,
          lv_threadc      TYPE c LENGTH 2.

    FIELD-SYMBOLS <ls_thread> LIKE LINE OF me->gt_threads_list.

    me->gv_group       = i_group.
    me->gv_task_prefix = i_task_prefix.

    " No more than 10 threads
    IF i_threads > 10.
      me->gv_threads = 10.
    ELSEIF i_threads <= 0.
      me->gv_threads = 1.
    ELSE.
      me->gv_threads = i_threads.
    ENDIF.

    lv_free_threads = get_free_threads( ).

    IF lv_free_threads <= 0.
      MESSAGE ID '/QCHK/CL_CVI_MESSAGE' TYPE 'X' NUMBER 041.
    ENDIF.

    lv_free_threads = lv_free_threads - 2.

    IF lv_free_threads < me->gv_threads.
      me->gv_threads = lv_free_threads.
    ENDIF.

    " Initialize threads
    DO me->gv_threads TIMES.
      APPEND INITIAL LINE TO me->gt_threads_list ASSIGNING <ls_thread>.
      lv_threadc = lv_threadn.
      CONCATENATE me->gv_task_prefix '_' lv_threadc INTO <ls_thread>-thread.
      <ls_thread>-used = abap_false.
      lv_threadn = lv_threadn + 1.
    ENDDO.

  ENDMETHOD.                    " constructor

  METHOD handle_resource_failure.
    DATA lv_free_threads TYPE i.

    lv_free_threads = get_free_threads( ).

    IF lv_free_threads <= 1 AND me->gv_threads > 1.
      me->gv_threads = me->gv_threads - 1.
    ENDIF.

    WAIT UP TO 5 SECONDS. " Long enough for the system to update
    WAIT UNTIL me->gv_used_threads < me->gv_threads. " Now there's an available thread
  ENDMETHOD.                    " handle_resource_failure

  METHOD get_free_thread.

    FIELD-SYMBOLS <ls_thread> LIKE LINE OF me->gt_threads_list.

    " Wait for a free thread
    me->gv_count = me->gv_count + 1.
    IF gv_count >= me->gv_threads.
      WAIT UNTIL me->all_threads_are_finished( ) = abap_true.
    ENDIF.

    " dynamic adjustment
    DO.
      IF me->get_free_threads( ) > me->gv_used_threads.
        EXIT.
      ELSE.
        WAIT UNTIL me->gv_used_threads < get_free_threads( ).
      ENDIF.
    ENDDO.

    " Get number of first free thread
    READ TABLE me->gt_threads_list WITH KEY used = abap_false ASSIGNING <ls_thread>.
    IF sy-subrc = 0.
      gv_used_threads = gv_used_threads + 1.
      <ls_thread>-used = abap_true.
      rv_thread = <ls_thread>-thread.
    ENDIF.

  ENDMETHOD.                    " get_free_thread

  METHOD initialize_dupl_work_tab.

    gt_dupl_check_result_tmp = it_work_tab.

  ENDMETHOD.                    " initialize_dupl_work_tab

ENDCLASS.                    "lcl_multi_thread IMPLEMENTATION
