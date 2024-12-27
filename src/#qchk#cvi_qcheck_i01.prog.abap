*&---------------------------------------------------------------------*
*&  Include           /QCHK/CVI_QCHECK_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: " lv_chck_type TYPE /QCHK/CVI_CHECK_TYPE,
       lt_check_list TYPE  /qchk/tt_cvi_check_list.

  CASE gv_ok_code.

      " Batch call
    WHEN'EXECUTE'.

      IF     p_kunst  IS NOT INITIAL
         AND so_kunnr IS INITIAL
         AND so_namku IS INITIAL
         AND so_adkun IS INITIAL
         AND so_stkun IS INITIAL.
        MESSAGE i046(/qchk/cl_cvi_message) DISPLAY LIKE 'I'.   " Bitte Kundendaten eingeben!
      ENDIF.

      IF     p_lifnst IS NOT INITIAL
         AND so_lifnr IS INITIAL
         AND so_namli IS INITIAL
         AND so_adlif IS INITIAL
         AND so_stlif IS INITIAL.
        MESSAGE i047(/qchk/cl_cvi_message) DISPLAY LIKE 'I'.   " Bitte Lieferantendaten eingeben!
      ENDIF.

    " get runs
      lcl_cvi_service=>get_run_list( IMPORTING et_check_list = lt_check_list ).

      IF lt_check_list IS INITIAL.
        gv_ok_code = 'UPD'.
        MESSAGE i043(/qchk/cl_cvi_message) DISPLAY LIKE 'I'. " Bitte mindestens eine Prüfung auswählen
        RETURN.
      ENDIF.

      LOOP AT lt_check_list ASSIGNING FIELD-SYMBOL(<fs_check_list>).
        CASE <fs_check_list>-check_type.
          WHEN gc_check_docs.
            IF <fs_check_list>-check_selected = abap_true.
              " Create Job for Proof of documents use check
              lcl_cvi_service=>create_job( gc_check_docs ).
            ENDIF.
          WHEN gc_check_qual.
            IF <fs_check_list>-check_selected = abap_true.
              " Create Job for S4HANA pre check
              lcl_cvi_service=>create_job( gc_check_qual ).
            ENDIF.
          WHEN gc_check_dupl.
            IF <fs_check_list>-check_selected = abap_true.
              " Create Job for Duplicates check
              IF c_dupadr <> 'X' AND c_dupnam <> 'X' AND c_dupst <> 'X'.
                gv_ok_code = 'UPD'.
                MESSAGE i041(/qchk/cl_cvi_message) DISPLAY LIKE 'I'. " Für die Dublettenprüfung bitte mindestens einen Parameter auswählen.
              ELSE.
                lcl_cvi_service=>create_job( gc_check_dupl ).
              ENDIF.
            ENDIF.

          WHEN OTHERS.
            gv_ok_code = 'UPD'.
            MESSAGE i043(/qchk/cl_cvi_message) DISPLAY LIKE 'I'. " Bitte mindestens eine Prüfung auswählen

        ENDCASE.

      ENDLOOP.

*      IF c_use IS NOT INITIAL.
*        " Create Job for Proof of documents use check
*        lcl_cvi_service=>create_job( gc_check_docs ).
*        " Create Job for S4HANA pre check
*      ELSEIF c_s4 IS NOT INITIAL.
*        lcl_cvi_service=>create_job( gc_check_qual ).
*        " Create Job for Duplicates check
*      ELSEIF c_dubl IS NOT INITIAL.
*        IF c_dupadr <> 'X' AND c_dupnam <> 'X' AND c_dupst <> 'X'.
*          gv_ok_code = 'UPD'.
*          MESSAGE i041(/qchk/cl_cvi_message) DISPLAY LIKE 'I'. " Für die Dublettenprüfung bitte mindestens einen Parameter auswählen.
*        ELSE.
*          lcl_cvi_service=>create_job( gc_check_dupl ).
*        ENDIF.
*      ELSE.
*        gv_ok_code = 'UPD'.
*        MESSAGE i043(/qchk/cl_cvi_message) DISPLAY LIKE 'I'. " Bitte mindestens eine Prüfung auswählen
*      ENDIF.
*

    WHEN 'BACK' OR 'CANCEL'.
      LEAVE PROGRAM.

  ENDCASE.

ENDMODULE.
