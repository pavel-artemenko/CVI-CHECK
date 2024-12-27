*&---------------------------------------------------------------------*
*&  Include  /qchk/cvi_check_proc_top
*&---------------------------------------------------------------------*

"Types
TYPES: BEGIN OF gty_check_strings,
         descr           TYPE char20,
         string          TYPE string,
         original_string TYPE string,
       END OF gty_check_strings.

DATA:
  gv_dupg       TYPE int1,
  gv_duppadr    TYPE xfeld,
  gv_duppnam    TYPE xfeld,
  gv_duppustid  TYPE xfeld,
  gv_ucomm      TYPE sy-ucomm,
  gv_check_type TYPE char32,
  gv_uuid       TYPE uuid,
  gv_timestamp  TYPE timestamp,
  gv_jobcount   TYPE btcjobcnt,
  gv_jobname    TYPE btcjob,
  gv_excarc     TYPE xfeld,
  gv_excdel     TYPE xfeld,
  gv_save       TYPE int1,
  gv_fremd      TYPE xfeld,
  gv_kna        TYPE xfeld,
  gv_lfa        TYPE xfeld,
  gv_loevm      TYPE xfeld,
  gv_beleg      TYPE xfeld,
  gv_use        TYPE xfeld,
  gv_lifnr      TYPE lfa1-lifnr,
  gv_kunnr      TYPE kna1-kunnr,
  gv_runid      TYPE /qchk/cvi_runid,
  gv_type       TYPE string,
  gv_obj_source TYPE mds_ctrl_obj_source,

  gs_sel_param  TYPE mdst_sync_parameter,
  gs_cust_data  TYPE cmds_ei_main,
  gs_cust_data1 TYPE cmds_ei_main,
  gs_vend_data  TYPE vmds_ei_main,

  gr_check      TYPE REF TO lcl_cvi_checks.


CONSTANTS: gc_objtype_cust         TYPE string VALUE 'CUSTOMER',
           gc_objtype_vend         TYPE string VALUE 'VENDOR',
           gc_objtype_v            TYPE CHAR1 VALUE 'V',
           gc_objtype_c            TYPE CHAR1 VALUE 'C',
           gc_descr_string_address TYPE string VALUE 'ADDRESS',
           gc_run_status_inprocess TYPE string VALUE '01',
           gc_run_status_completed TYPE string VALUE '02',
           gc_descr_string_name    TYPE string VALUE 'NAME',
           gc_descr_string_ustid   TYPE string VALUE 'USTID'.

*           gc_customer             TYPE string VALUE 'CVI_FREMD_CUSTOMER',
*           gc_customer_ext         TYPE string VALUE 'CVI_FREMD_CUSTOMER.DAT',
*           gc_customer_appl        TYPE string VALUE './CVI_FREMD_CUSTOMER.DAT',
*
*           gc_vendor               TYPE string VALUE 'CVI_FREMD_VENDOR',
*           gc_vendor_ext           TYPE string VALUE 'CVI_FREMD_VENDOR.DAT',
*           gc_vendor_appl          TYPE string VALUE './CVI_FREMD_VENDOR.DAT'.
