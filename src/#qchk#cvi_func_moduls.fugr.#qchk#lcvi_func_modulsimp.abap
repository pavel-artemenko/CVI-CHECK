CLASS lcl_cvi_service_dupl IMPLEMENTATION.

  " -----------------------------------------------------------------------
  " Method build_check_string.
  " -----------------------------------------------------------------------
  METHOD build_check_string.

    DATA: lv_relative_name TYPE string,

          ls_cust_data     TYPE cmds_ei_extern,
          ls_vend_data     TYPE vmds_ei_extern,
          ls_check_string  TYPE gty_check_strings,

          lt_check_strings TYPE TABLE OF gty_check_strings,

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
        IF iv_duppnam IS NOT INITIAL.
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
        IF iv_duppadr IS NOT INITIAL.
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
        IF iv_duppustid IS NOT INITIAL.
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
        IF iv_duppnam IS NOT INITIAL.
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
        IF iv_duppadr IS NOT INITIAL.
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
        IF iv_duppustid IS NOT INITIAL.
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

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method calculate_similarity_score.
  " -----------------------------------------------------------------------
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
    lv_max = ( 100 - iv_dupg ) * lv_max / 100 + 1.
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

    IF lv_lev_probability >= iv_dupg. " lv_lev_distance < lv_max.
      ev_le_max = abap_true.
      ev_lev_probability = lv_lev_probability.
    ENDIF.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method save_score.
  " -----------------------------------------------------------------------
  METHOD save_score.
    " check group id if changed
    IF    gs_dupl_check_result_sourc IS INITIAL
       OR gs_dupl_check_result_targ  IS INITIAL.

      gs_dupl_check_result_sourc-sync_obj_sour_id = iv_src.
      gs_dupl_check_result_targ-sync_obj_sour_id = iv_trg.

    ENDIF.

    " save results of comparison
    CASE iv_field_param.
      WHEN gc_descr_string_address. " 'ADDRESS'.
        gs_dupl_check_result_targ-probability_address = iv_probability.
        gs_dupl_check_result_sourc-adress_src = iv_string_src.
        gs_dupl_check_result_targ-adress_src = iv_string_trg.
      WHEN gc_descr_string_name. "'NAME'.
        gs_dupl_check_result_targ-probability_name = iv_probability.
        gs_dupl_check_result_sourc-name_src = iv_string_src.
        gs_dupl_check_result_targ-name_src = iv_string_trg.
      WHEN gc_descr_string_ustid. "'USTID'.
        gs_dupl_check_result_targ-probability_ustid = iv_probability.
        gs_dupl_check_result_sourc-ustid_src = iv_string_src.
        gs_dupl_check_result_targ-ustid_src = iv_string_trg.
    ENDCASE.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method save_score.
  " -----------------------------------------------------------------------
  METHOD append_score.

    DATA: lv_show_result   TYPE boole_d,
          lv_group_id      TYPE i,
          lv_last_group_id TYPE i.


    FIELD-SYMBOLS <ls_check_result> TYPE /qchk/st_duplicate_res.

    lv_last_group_id = gs_current_group-group_id.

    " create group id, based on data
    lv_group_id = create_group_id( is_source = gs_dupl_check_result_sourc
                                   is_target = gs_dupl_check_result_targ ).

    IF    gs_dupl_check_result_sourc IS INITIAL
       OR gs_dupl_check_result_targ  IS INITIAL.
      RETURN.
    ENDIF.

    plausi_check( EXPORTING is_check_data  = gs_dupl_check_result_targ
                  IMPORTING ev_show_result = lv_show_result ).

    IF lv_show_result = abap_true.

      READ TABLE gt_dupl_check_result  WITH KEY sync_obj_sour_id = gs_dupl_check_result_sourc-sync_obj_sour_id
                                                group_id         = lv_group_id
           ASSIGNING <ls_check_result>.
      IF sy-subrc <> 0.
        gs_dupl_check_result_sourc-group_id = lv_group_id.
        APPEND gs_dupl_check_result_sourc TO gt_dupl_check_result.
      ENDIF.

      gs_dupl_check_result_targ-group_id = lv_group_id.
      APPEND gs_dupl_check_result_targ TO gt_dupl_check_result.

    ENDIF.

    clear_score( ).

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method save_score.
  " -----------------------------------------------------------------------
  METHOD check_group_id_changed.
    CLEAR rv_result.

    IF gv_group_id IS INITIAL.
      rv_result = abap_true.
      EXIT.
    ENDIF.

    IF is_source-sync_obj_sour_id <> gs_current_group-source.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method clear_score.
  " -----------------------------------------------------------------------
  METHOD clear_score.
    CLEAR: gs_dupl_check_result_sourc,
           gs_dupl_check_result_targ.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method create_group_id.
  " -----------------------------------------------------------------------
  METHOD create_group_id.
    IF check_group_id_changed( is_source = is_source
                               is_target = is_target ) = abap_true.

      gv_group_id = gv_group_id + 1.

      gs_current_group-group_id = gv_group_id.
      gs_current_group-source   = is_source-sync_obj_sour_id.
      gs_current_group-target   = is_target-sync_obj_sour_id.

    ENDIF.

    rv_group_id = gv_group_id.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method get_results.
  " -----------------------------------------------------------------------
  METHOD get_results.
    et_results = gt_dupl_check_result.
  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method plausi_check.
  " -----------------------------------------------------------------------
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

    FIELD-SYMBOLS <ls_plausi> LIKE LINE OF gt_dupl_plausi.

    IF lines( gt_dupl_plausi ) < 1.
      " read customizing
      " plausibility checks
      SELECT *
        FROM /qchk/dupl_plaus
        INTO TABLE gt_dupl_plausi
        WHERE plausi_active = abap_true.
    ENDIF.

    " plausibility check
    LOOP AT gt_dupl_plausi ASSIGNING <ls_plausi>.
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
        IF is_check_data-probability_name IN lt_probability_range.
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
        IF is_check_data-probability_address IN lt_probability_range.
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
        IF is_check_data-probability_ustid = 100.
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
  ENDMETHOD.


  " -----------------------------------------------------------------------
  " Method dupl_check_customers.
  " -----------------------------------------------------------------------
  METHOD dupl_check_customers.

    DATA: lv_search_str1        TYPE string,
          lv_search_str2        TYPE string,
          lv_lev_probability    TYPE /qchk/cvi_probability,
          lv_le_max             TYPE boolean,
          lv_append_score       TYPE boolean,
          lv_yes                TYPE boolean,
          lv_is_duplicate       TYPE boolean,

          lt_check_strings_main TYPE TABLE OF gty_check_strings,
          lt_check_strings_opp  TYPE TABLE OF gty_check_strings.


    FIELD-SYMBOLS: <ls_cust_data_base>    TYPE cmds_ei_extern,
                   <ls_check_string_main> LIKE LINE OF lt_check_strings_main,
                   <ls_check_string_opp>  LIKE LINE OF lt_check_strings_opp.


    " Build main strings for duplicate check
    ir_service_dupl->build_check_string( EXPORTING is_data          = is_source_cust
                                                   iv_duppadr       = iv_duppadr
                                                   iv_duppnam       = iv_duppnam
                                                   iv_duppustid     = iv_duppustid
                                         IMPORTING it_check_strings = lt_check_strings_main ).

    " Check against complete master data
    LOOP AT it_customers_compl_work ASSIGNING <ls_cust_data_base>.
      READ TABLE it_worked WITH KEY table_line = <ls_cust_data_base>-header-object_instance-kunnr
           BINARY SEARCH
           TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CLEAR lv_search_str2.
      CLEAR lv_lev_probability.
      CLEAR lv_yes.
      CLEAR lv_is_duplicate.
      CLEAR lv_le_max.
      CLEAR lv_append_score.

      " Build strings for duplicate check
      ir_service_dupl->build_check_string( EXPORTING is_data          = <ls_cust_data_base>
                                                     iv_duppadr       = iv_duppadr
                                                     iv_duppnam       = iv_duppnam
                                                     iv_duppustid     = iv_duppustid
                                           IMPORTING it_check_strings = lt_check_strings_opp ).

      " compute duplicate probability
      LOOP AT lt_check_strings_main ASSIGNING <ls_check_string_main>.

        lv_search_str1 = <ls_check_string_main>-string.

        LOOP AT lt_check_strings_opp ASSIGNING <ls_check_string_opp> WHERE descr = <ls_check_string_main>-descr.
          CLEAR lv_lev_probability.
          CLEAR lv_yes.
          CLEAR lv_le_max.

          lv_search_str2 = <ls_check_string_opp>-string.

          " Comparison routines
          " compare USTID
          IF <ls_check_string_main>-descr = ir_service_dupl->gc_descr_string_ustid.
            IF     lv_search_str1  = lv_search_str2
               AND lv_search_str1 IS NOT INITIAL.
              lv_lev_probability = 100.
              lv_append_score = abap_true.
            ENDIF.
          ELSE.

            " compare
            ir_service_dupl->calculate_similarity_score( EXPORTING iv_search_str1     = lv_search_str1
                                                                   iv_search_str2     = lv_search_str2
                                                                   iv_dupg            = iv_dupg
                                                         IMPORTING ev_lev_probability = lv_lev_probability
                                                                   ev_le_max          = lv_le_max ).
            IF lv_le_max = abap_true.
              lv_append_score = abap_true.
            ENDIF.

          ENDIF. " compare USTID

          ir_service_dupl->save_score( iv_src         = is_source_cust-header-object_instance-kunnr " <ls_cust_data>-header-object_instance-kunnr
                                       iv_trg         = <ls_cust_data_base>-header-object_instance-kunnr
                                       iv_string_src  = <ls_check_string_main>-original_string
                                       iv_string_trg  = <ls_check_string_opp>-original_string
                                       iv_probability = lv_lev_probability
                                       iv_field_param = <ls_check_string_main>-descr ).

        ENDLOOP.

      ENDLOOP.

      " append score to result table
      IF lv_append_score = abap_true.
        ir_service_dupl->append_score( ).
      ELSE.
        ir_service_dupl->clear_score( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  " -----------------------------------------------------------------------
  " Method dupl_check_vendors.
  " -----------------------------------------------------------------------
  METHOD dupl_check_vendors.


    DATA: lv_search_str1        TYPE string,
          lv_search_str2        TYPE string,
          lv_lev_probability    TYPE /qchk/cvi_probability,
          lv_le_max             TYPE boolean,
          lv_append_score       TYPE boolean,
          lv_yes                TYPE boolean,
          lv_is_duplicate       TYPE boolean,

          lt_check_strings_main TYPE TABLE OF gty_check_strings,
          lt_check_strings_opp  TYPE TABLE OF gty_check_strings.

    FIELD-SYMBOLS: <ls_vend_data_base>    TYPE vmds_ei_extern,
                   <ls_check_string_main> LIKE LINE OF lt_check_strings_main,
                   <ls_check_string_opp>  LIKE LINE OF lt_check_strings_opp.

    " Build main strings for duplicate check
    ir_service_dupl->build_check_string( EXPORTING is_data          = is_source_vend
                                                   iv_duppadr       = iv_duppadr
                                                   iv_duppnam       = iv_duppnam
                                                   iv_duppustid     = iv_duppustid
                                         IMPORTING it_check_strings = lt_check_strings_main ).

    " Check against complete master data
    LOOP AT it_vendors_compl_work ASSIGNING <ls_vend_data_base>.
      READ TABLE it_worked WITH KEY table_line = <ls_vend_data_base>-header-object_instance-lifnr
           BINARY SEARCH
           TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      CLEAR lv_search_str2.
      CLEAR lv_lev_probability.
      CLEAR lv_yes.
      CLEAR lv_is_duplicate.
      CLEAR lv_le_max.
      CLEAR lv_append_score.

      " Build strings for duplicate check
      ir_service_dupl->build_check_string( EXPORTING is_data          = <ls_vend_data_base>
                                                     iv_duppadr       = iv_duppadr
                                                     iv_duppnam       = iv_duppnam
                                                     iv_duppustid     = iv_duppustid
                                           IMPORTING it_check_strings = lt_check_strings_opp ).

      "Compute duplicate probability
      LOOP AT lt_check_strings_main ASSIGNING <ls_check_string_main>.

        lv_search_str1 = <ls_check_string_main>-string.

        LOOP AT lt_check_strings_opp ASSIGNING <ls_check_string_opp> WHERE descr = <ls_check_string_main>-descr.
          CLEAR lv_lev_probability.
          CLEAR lv_yes.
          CLEAR lv_le_max.

          lv_search_str2 = <ls_check_string_opp>-string.

          " Comparison routines
          " compare USTID
          IF <ls_check_string_main>-descr = ir_service_dupl->gc_descr_string_ustid.
            IF     lv_search_str1  = lv_search_str2
               AND lv_search_str1 IS NOT INITIAL.
              lv_lev_probability = 100.
              lv_append_score = abap_true.
            ENDIF.
          ELSE.

            " Compare
            ir_service_dupl->calculate_similarity_score( EXPORTING iv_search_str1     = lv_search_str1
                                                                   iv_search_str2     = lv_search_str2
                                                                   iv_dupg            = iv_dupg
                                                         IMPORTING ev_lev_probability = lv_lev_probability
                                                                   ev_le_max          = lv_le_max ).
            IF lv_le_max = abap_true.
              lv_append_score = abap_true.
            ENDIF.

          ENDIF. " Compare USTID

          ir_service_dupl->save_score( iv_src         = is_source_vend-header-object_instance-lifnr " <ls_cust_data>-header-object_instance-kunnr
                                       iv_trg         = <ls_vend_data_base>-header-object_instance-lifnr
                                       iv_string_src  = <ls_check_string_main>-original_string
                                       iv_string_trg  = <ls_check_string_opp>-original_string
                                       iv_probability = lv_lev_probability
                                       iv_field_param = <ls_check_string_main>-descr ).

        ENDLOOP.

      ENDLOOP.

      " append score to result table
      IF lv_append_score = abap_true.
        ir_service_dupl->append_score( ).
      ELSE.
        ir_service_dupl->clear_score( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
