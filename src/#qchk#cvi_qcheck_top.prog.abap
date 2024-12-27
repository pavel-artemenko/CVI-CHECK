*&---------------------------------------------------------------------*
*&  Include           /QCHK/CVI_QCHECK_TOP
*&---------------------------------------------------------------------*

* Data declaration
*---------------------------------------

TABLES: icon.

TYPES: BEGIN OF gty_log_res,
         unique_id       TYPE uuid,
         timestamp       TYPE timestamp,
         dateandtime(45),
         mode(30),
         status_complete TYPE xfeld,
         jobcount        TYPE btcjobcnt,
         jobname         TYPE btcjob,
         status          TYPE icon-name,
       END OF gty_log_res.

CONSTANTS: gc_dummy_uuid           TYPE sysuuid_x16 VALUE '1234567812345678',
           gc_check_docs           TYPE /qchk/cvi_check_type VALUE 'CHECK_DOCS',
           gc_check_dupl           TYPE /qchk/cvi_check_type VALUE 'CHECK_DUPL',
           gc_check_qual           TYPE /qchk/cvi_check_type VALUE 'CHECK_QUAL',
           gc_job_name_docs        TYPE char4 VALUE 'DOCS',
           gc_job_name_dupl        TYPE char4 VALUE 'DUPL',
           gc_job_name_qual        TYPE char4 VALUE 'QUAL',
           gc_objtype_v            TYPE char1 VALUE 'V',
           gc_objtype_c            TYPE char1 VALUE 'C',
           gc_run_status_inprocess TYPE string VALUE '01',
           gc_run_status_completed TYPE string VALUE '02',
           gc_report_name          TYPE rsvar-report VALUE '/QCHK/CVI_CHECK_PROCESSING'.

" gc_report_name TYPE string VALUE '/QCHK/CVI_CHECK_PROCESSING'.
" gc_customer_ext TYPE string VALUE 'CVI_FREMD_CUSTOMER.DAT',
" gc_vendor_ext   TYPE string VALUE 'CVI_FREMD_VENDOR.DAT'.

DATA: gv_lifnr          TYPE lfa1-lifnr,
      gv_kunnr          TYPE kna1-kunnr,
      gv_name1          TYPE name1_gp,
      gv_adrnr          TYPE adrnr,
      gv_stceg          TYPE stceg,
      gv_objtype        TYPE /qchk/cvi_objtype,
      "   gv_subscreen_new       TYPE sy-dynnr VALUE '0110',
      "  gv_subscreen_testfr    TYPE sy-dynnr VALUE '0130',
      "   gv_subscreen_actual    TYPE sy-dynnr,
      gv_ok_code        TYPE sy-ucomm,
      "  gv_filename_short      TYPE epsfilnam,
      "  gv_apdir               TYPE epsdirnam,
      "  gv_frdir               TYPE epsdirnam,
      "  gv_apdir_phys          TYPE epsdirnam,
      "  gv_frdir_phys          TYPE epsdirnam,
      "  gv_filename_apdir_phys TYPE string,
      "  gv_filename_frdir_phys TYPE string,
      "  gv_variant             TYPE raldb_vari,

      gt_log_res        TYPE TABLE OF /qchk/st_log_results_hitlist,
      gt_quality_res    TYPE /qchk/tt_quality_check_hitlist,
      gt_dupl_res       TYPE /qchk/tt_duplicate_res,
      gt_documents_res  TYPE /qchk/tt_documents_hitlist,
      gt_valutab        TYPE TABLE OF rsparams,

      gr_cust_container TYPE REF TO cl_gui_custom_container,
      gr_event_handler  TYPE REF TO lcl_event_handler,
      gr_log_table      TYPE REF TO cl_salv_table,
      gr_docs_result    TYPE REF TO cl_salv_table,
      gr_dupl_result    TYPE REF TO cl_salv_table,
      gr_quality_result TYPE REF TO cl_salv_table.
