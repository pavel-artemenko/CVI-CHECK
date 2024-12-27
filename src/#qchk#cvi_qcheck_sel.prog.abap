*&---------------------------------------------------------------------*
*&  Include           /QCHK/CVI_QCHECK_SEL
*&---------------------------------------------------------------------*

LOAD-OF-PROGRAM.

  SELECTION-SCREEN BEGIN OF SCREEN 0110 AS SUBSCREEN .
* master data selection
  SELECTION-SCREEN BEGIN OF BLOCK b1  WITH FRAME TITLE TEXT-010. "Stammdatenauswahl
  PARAMETERS: p_kunst  RADIOBUTTON GROUP rbg1 DEFAULT 'X' USER-COMMAND mode,
              p_lifnst RADIOBUTTON GROUP rbg1.
  SELECTION-SCREEN END OF BLOCK b1.
* search parameters
  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE sel_txt.

  SELECT-OPTIONS: so_kunnr FOR gv_kunnr MODIF ID kun,
                  so_lifnr FOR gv_lifnr MODIF ID lif,
                  so_namku FOR gv_name1 MATCHCODE OBJECT /qchk/sh_name_kun MODIF ID kun,
                  so_namli FOR gv_name1 MATCHCODE OBJECT /qchk/sh_name_lif MODIF ID lif,
                  so_adkun FOR gv_adrnr MATCHCODE OBJECT /qchk/sh_adrnr_kun MODIF ID kun,
                  so_adlif FOR gv_adrnr MATCHCODE OBJECT /qchk/sh_adrnr_lif MODIF ID lif,
                  so_stkun FOR gv_stceg MATCHCODE OBJECT /qchk/sh_stceg_kun MODIF ID kun,
                  so_stlif FOR gv_stceg MATCHCODE OBJECT /qchk/sh_stceg_lif MODIF ID lif.

  TYPES: ty_it_screen TYPE STANDARD TABLE OF screen WITH DEFAULT KEY.

  DATA: it_screen TYPE ty_it_screen.

  SELECTION-SCREEN END OF BLOCK b2.

  SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-006. " Aktion ausführen


  PARAMETERS: c_s4   AS CHECKBOX DEFAULT ''   USER-COMMAND mode.  "S/4HANA Pre Checks

  PARAMETERS: c_dubl AS CHECKBOX DEFAULT ''    USER-COMMAND mode. "Dubletten-Prüfung
  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 5.
  PARAMETERS: c_dupnam AS CHECKBOX MODIF ID  dup.
  SELECTION-SCREEN COMMENT (6) TEXT-040 MODIF ID dup.      "Name

  PARAMETERS: c_dupadr AS CHECKBOX MODIF ID dup.           "Adresse
  SELECTION-SCREEN COMMENT (8) TEXT-041 MODIF ID dup.

  PARAMETERS: c_dupst AS CHECKBOX MODIF ID dup.           "USt-Id.Nr
  SELECTION-SCREEN COMMENT (10) TEXT-042 MODIF ID dup.

  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 5.
  "PARAMETERS: c_cchk AS CHECKBOX MODIF ID dub.             " Kreuzcheck Kreditoren-/Debitorenstamm
  "SELECTION-SCREEN COMMENT (77) TEXT-013 MODIF ID dub.     " Kreuzcheck Kreditoren- /Debitorenstamm
  SELECTION-SCREEN END OF LINE.
  PARAMETERS: c_use AS CHECKBOX DEFAULT '' USER-COMMAND mode.                  "Verwendungsnachweis

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 5.
  PARAMETERS: c_loevm AS CHECKBOX MODIF ID use.
  SELECTION-SCREEN COMMENT (77) TEXT-004 MODIF ID use.      "Löschvermerke
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN POSITION 5.
  PARAMETERS: c_beleg AS CHECKBOX MODIF ID use.
  SELECTION-SCREEN COMMENT (36) TEXT-005 MODIF ID use.      "Archievierte Belege ausschlißen (FI)
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
  "SELECTION-SCREEN COMMENT (15) TEXT-012 FOR FIELD c_serv NO-DISPLAY.
  PARAMETERS c_serv TYPE spta_rfcgr MATCHCODE OBJECT spta_server_group VISIBLE LENGTH 10 NO-DISPLAY.
  "SELECT-OPTIONS s_run_on FOR ls_run_on-run_on NO-DISPLAY.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN END OF BLOCK b4.

  SELECTION-SCREEN END OF SCREEN 0110.


AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN. "INTO DATA(ls_screen).

    IF screen-group1 = 'KUN'. "ID Muessen groß geschrieben werden"
      IF p_kunst = 'X'.
        screen-active = 1.
        sel_txt = TEXT-001.
        gv_objtype = 'C'.   " Set the object type Customer
      ELSE.
        screen-active = 0.
      ENDIF.
      "ENDIF.
    ELSEIF screen-group1 = 'LIF'.
      IF p_lifnst = 'X'.
        screen-active = 1.
        sel_txt = TEXT-002.
        gv_objtype = 'V'. " Set the object type Vendor
      ELSE.
        screen-active = 0.
      ENDIF.
      "ENDIF.
    ELSEIF screen-group1 = 'USE'.
      IF c_use = 'X'.
        screen-input = 1.
      ELSE.
        screen-input = 0.
        c_loevm = ''.
        c_beleg = ''.
      ENDIF.

    ELSEIF screen-group1 = 'DUP'.
      IF c_dubl = 'X'.
        screen-input = 1.
      ELSE.
        screen-input = 0.
        c_dupadr = ''.
        c_dupnam = ''.
        c_dupst  = ''.
      ENDIF.

*    ELSEIF screen-group1 = 'NAM'.
*      IF c_dubl = 'X'.
*        screen-input = 1.
*      ELSE.
*        screen-input = 0.
*      ENDIF.

    ENDIF.

    MODIFY SCREEN. "FROM ls_screen.
    "MODIFY SCREEN.

  ENDLOOP.

*    LOOP AT SCREEN.
*    IF screen-group1 = 'KUN'. "ID Muessen groß geschrieben werden"
*      IF p_kunst = 'X'.
*        screen-active = 1.
*        sel_txt = TEXT-001.
*        gv_objtype = 'C'.   " Set the object type Customer
*      ELSE.
*        screen-active = 0.
*      ENDIF.
*    ENDIF.
*    IF screen-group1 = 'LIF'.
*      IF p_lifnst = 'X'.
*        screen-active = 1.
*        sel_txt = TEXT-002.
*        gv_objtype = 'V'. " Set the object type Vendor
*      ELSE.
*        screen-active = 0.
*      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*
*    IF screen-group1 = 'USE'.
*      IF c_use = 'X'.
*        screen-input = 1.
*      ELSE.
*        screen-input = 0.
*        c_loevm = ''.
*        c_beleg = ''.
*      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*
*    IF screen-group1 = 'DUB'.
*      IF c_dubl = 'X'.
*        screen-input = 1.
*      ELSE.
*        screen-input = 0.
*        "c_cchk = ''.
*      ENDIF.
*    ENDIF.
*
*    MODIFY SCREEN.
*  ENDLOOP.
