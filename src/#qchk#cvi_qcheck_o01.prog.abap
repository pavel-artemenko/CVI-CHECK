*&---------------------------------------------------------------------*
*&  Include           /QCHK/CVI_QCHECK_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  SET_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_screen OUTPUT.

  IF gv_ok_code = 'UPD'.
    CLEAR gv_ok_code.
    LEAVE TO SCREEN 0100.
  ENDIF.

ENDMODULE.                 " SET_SUBSCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_RESULT_CONTAINER  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_log_container OUTPUT.

  DATA: lo_display         TYPE REF TO lcl_cvi_display,
        ls_log_results_dum TYPE /qchk/st_log_results_hitlist,
        lv_selected_kna    TYPE xfeld,
        lv_selected_lfa    TYPE xfeld.


  CREATE OBJECT lo_display
    EXPORTING
      is_log_result = ls_log_results_dum
      iv_title   = ''.



ENDMODULE.
