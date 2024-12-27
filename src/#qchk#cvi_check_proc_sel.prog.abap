*&---------------------------------------------------------------------*
*&  Include  /qchk/cvi_check_proc_sel
*&---------------------------------------------------------------------*


INITIALIZATION.

  SELECTION-SCREEN BEGIN OF SCREEN 100."only used from variant

  PARAMETERS: p_kunst  TYPE xfeld NO-DISPLAY,
              p_lifnst TYPE xfeld NO-DISPLAY,
              c_loevm  TYPE xfeld NO-DISPLAY,
              c_dupnam TYPE xfeld NO-DISPLAY,
              c_dupst  TYPE xfeld NO-DISPLAY,
              c_dupadr TYPE xfeld NO-DISPLAY,
              c_beleg  TYPE xfeld NO-DISPLAY.

  SELECT-OPTIONS: so_kunnr FOR gv_kunnr NO-DISPLAY,
                  so_lifnr FOR gv_lifnr NO-DISPLAY.

  PARAMETERS: p_dupg   TYPE int1      NO-DISPLAY,
              p_duppad TYPE xfeld     NO-DISPLAY,
              p_duppna TYPE xfeld     NO-DISPLAY,
              p_duppus TYPE xfeld     NO-DISPLAY,
              p_ucomm  TYPE sy-ucomm  NO-DISPLAY,
              p_chtype TYPE char32    NO-DISPLAY,
              p_uuid   TYPE uuid      NO-DISPLAY,
              p_timest TYPE timestamp NO-DISPLAY,
              p_jobcou TYPE btcjobcnt NO-DISPLAY,
              p_jobnam TYPE btcjob    NO-DISPLAY,
              p_excdel TYPE xfeld     NO-DISPLAY,
              p_excarc TYPE xfeld     NO-DISPLAY,
              p_save   TYPE int1      NO-DISPLAY,
              p_run_id TYPE /qchk/cvi_runid   NO-DISPLAY,

              p_fremd  TYPE xfeld     NO-DISPLAY.

  SELECTION-SCREEN END OF SCREEN 100.
