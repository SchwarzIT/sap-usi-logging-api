*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Test-Double for DAO (To inject customizing)
" ---------------------------------------------------------------------
CLASS lcl_test_double_cust_dao DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_mock_data_line,
             log_object       TYPE balobj_d,
             sub_object       TYPE balsubobj,
             log_level        TYPE REF TO /usi/cl_bal_enum_log_level,
             retention_params TYPE /usi/bal_retention_parameters,
           END OF ty_mock_data_line,
           ty_mock_data_lines TYPE STANDARD TABLE OF ty_mock_data_line WITH EMPTY KEY.

    INTERFACES /usi/if_bal_cd_retention.

    METHODS clear_mock_data.

    METHODS insert_mock_data_lines
      IMPORTING
        i_new_lines TYPE ty_mock_data_lines.

    METHODS insert_mock_data_line
      IMPORTING
        i_new_line TYPE ty_mock_data_line.

  PRIVATE SECTION.
    DATA mock_data TYPE SORTED TABLE OF /usi/if_bal_cd_retention=>ty_record WITH UNIQUE KEY log_level
                                                                                            log_object
                                                                                            sub_object.

ENDCLASS.


CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD /usi/if_bal_cd_retention~get_records.
    LOOP AT mock_data ASSIGNING FIELD-SYMBOL(<mock_data_line>)
         WHERE     log_object IN i_log_object_range
               AND sub_object IN i_sub_object_range
               AND log_level   = i_log_level.
      INSERT <mock_data_line> INTO TABLE r_result.
    ENDLOOP.

    IF r_result IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.

  METHOD clear_mock_data.
    CLEAR mock_data.
  ENDMETHOD.

  METHOD insert_mock_data_lines.
    LOOP AT i_new_lines REFERENCE INTO DATA(new_line).
      insert_mock_data_line( new_line->* ).
    ENDLOOP.
  ENDMETHOD.

  METHOD insert_mock_data_line.
    INSERT VALUE #( log_object           = i_new_line-log_object
                    sub_object           = i_new_line-sub_object
                    log_level            = i_new_line-log_level->value
                    retention_parameters = i_new_line-retention_params )
           INTO TABLE mock_data.
    IF sy-subrc <> 0.
      cl_abap_unit_assert=>fail( msg = `Mock data could not be inserted!` ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test
" ---------------------------------------------------------------------
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: test_double_cust_dao TYPE REF TO lcl_test_double_cust_dao,
          cut                  TYPE REF TO /usi/cl_bal_ce_retention.

    METHODS setup.

    METHODS test_fallback_for_no_cust     FOR TESTING.
    METHODS test_no_match_for_log_object  FOR TESTING.
    METHODS test_no_match_for_sub_object  FOR TESTING.
    METHODS test_full_generic_matches_all FOR TESTING.
    METHODS test_generic_log_object       FOR TESTING.
    METHODS test_generic_sub_object       FOR TESTING.
    METHODS test_perfect_match            FOR TESTING.

    METHODS get_non_fallback_retention_par
      IMPORTING
        i_offset_days   TYPE int2
      RETURNING
        VALUE(r_result) TYPE /usi/bal_retention_parameters.

    METHODS assert_cut_returns_expected
      IMPORTING
        i_log_object      TYPE balobj_d
        i_sub_object      TYPE balsubobj                         OPTIONAL
        i_log_level       TYPE REF TO /usi/cl_bal_enum_log_level DEFAULT /usi/cl_bal_enum_log_level=>additional_info
        i_expected_result TYPE /usi/bal_retention_parameters.
ENDCLASS.


CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    test_double_cust_dao = NEW #( ).
    cut = NEW #( test_double_cust_dao ).
  ENDMETHOD.

  METHOD test_fallback_for_no_cust.
    assert_cut_returns_expected( i_log_object      = 'DUMMY'
                                 i_expected_result = cut->get_fallback( ) ).
  ENDMETHOD.

  METHOD test_no_match_for_log_object.
    test_double_cust_dao->insert_mock_data_line( VALUE #( log_object       = 'CUST_LOG_OBJECT'
                                                          log_level        = /usi/cl_bal_enum_log_level=>additional_info
                                                          retention_params = get_non_fallback_retention_par( 10 ) ) ).

    assert_cut_returns_expected( i_log_object      = 'NOT_IN_CUST'
                                 i_expected_result = cut->get_fallback( ) ).
  ENDMETHOD.

  METHOD test_no_match_for_sub_object.
    test_double_cust_dao->insert_mock_data_line( VALUE #( sub_object       = 'CUST_SUB_OBJECT'
                                                          log_level        = /usi/cl_bal_enum_log_level=>additional_info
                                                          retention_params = get_non_fallback_retention_par( 20 ) ) ).

    assert_cut_returns_expected( i_log_object      = space
                                 i_sub_object      = 'NOT_IN_CUST'
                                 i_expected_result = cut->get_fallback( ) ).
  ENDMETHOD.

  METHOD test_full_generic_matches_all.
    DATA(expected_result) = get_non_fallback_retention_par( 4 ).

    test_double_cust_dao->insert_mock_data_lines(
        i_new_lines = VALUE #( log_level = /usi/cl_bal_enum_log_level=>additional_info
                               ( log_object       = space
                                 sub_object       = space
                                 retention_params = expected_result )
                               ( log_object       = space
                                 sub_object       = 'CUST_SUB_OBJECT'
                                 retention_params = get_non_fallback_retention_par( 3 ) )
                               ( log_object       = 'CUST_LOG_OBJECT'
                                 sub_object       = space
                                 retention_params = get_non_fallback_retention_par( 2 )  )
                               ( log_object       = 'CUST_LOG_OBJECT'
                                 sub_object       = 'CUST_SUB_OBJECT'
                                 retention_params = get_non_fallback_retention_par( 1 ) ) ) ).

    assert_cut_returns_expected( i_log_object      = 'NOT_IN_CUST'
                                 i_sub_object      = 'NOT_IN_CUST'
                                 i_expected_result = expected_result ).
  ENDMETHOD.

  METHOD test_generic_log_object.
    DATA(expected_result) = get_non_fallback_retention_par( 3 ).

    test_double_cust_dao->insert_mock_data_lines(
        i_new_lines = VALUE #( log_level = /usi/cl_bal_enum_log_level=>additional_info
                               ( log_object       = space
                                 sub_object       = space
                                 retention_params = get_non_fallback_retention_par( 4 ) )
                               ( log_object       = space
                                 sub_object       = 'CUST_SUB_OBJECT'
                                 retention_params = expected_result )
                               ( log_object       = 'CUST_LOG_OBJECT'
                                 sub_object       = space
                                 retention_params = get_non_fallback_retention_par( 2 )  )
                               ( log_object       = 'CUST_LOG_OBJECT'
                                 sub_object       = 'CUST_SUB_OBJECT'
                                 retention_params = get_non_fallback_retention_par( 1 ) ) ) ).

    assert_cut_returns_expected( i_log_object      = 'NOT_IN_CUST'
                                 i_sub_object      = 'CUST_SUB_OBJECT'
                                 i_expected_result = expected_result ).
  ENDMETHOD.

  METHOD test_generic_sub_object.
    DATA(expected_result) = get_non_fallback_retention_par( 2 ).

    test_double_cust_dao->insert_mock_data_lines(
        i_new_lines = VALUE #( log_level = /usi/cl_bal_enum_log_level=>additional_info
                               ( log_object       = space
                                 sub_object       = space
                                 retention_params = get_non_fallback_retention_par( 4 ) )
                               ( log_object       = space
                                 sub_object       = 'CUST_SUB_OBJECT'
                                 retention_params = get_non_fallback_retention_par( 3 ) )
                               ( log_object       = 'CUST_LOG_OBJECT'
                                 sub_object       = space
                                 retention_params = expected_result  )
                               ( log_object       = 'CUST_LOG_OBJECT'
                                 sub_object       = 'CUST_SUB_OBJECT'
                                 retention_params = get_non_fallback_retention_par( 1 ) ) ) ).

    assert_cut_returns_expected( i_log_object      = 'CUST_LOG_OBJECT'
                                 i_sub_object      = 'NOT_IN_CUST'
                                 i_expected_result = expected_result ).
  ENDMETHOD.

  METHOD test_perfect_match.
    DATA(expected_result) = get_non_fallback_retention_par( 1 ).

    test_double_cust_dao->insert_mock_data_lines(
        i_new_lines = VALUE #( log_level = /usi/cl_bal_enum_log_level=>additional_info
                               ( log_object       = space
                                 sub_object       = space
                                 retention_params = get_non_fallback_retention_par( 4 ) )
                               ( log_object       = space
                                 sub_object       = 'CUST_SUB_OBJECT'
                                 retention_params = get_non_fallback_retention_par( 3 ) )
                               ( log_object       = 'CUST_LOG_OBJECT'
                                 sub_object       = space
                                 retention_params = get_non_fallback_retention_par( 2 )  )
                               ( log_object       = 'CUST_LOG_OBJECT'
                                 sub_object       = 'CUST_SUB_OBJECT'
                                 retention_params = expected_result ) ) ).

    assert_cut_returns_expected( i_log_object      = 'CUST_LOG_OBJECT'
                                 i_sub_object      = 'CUST_SUB_OBJECT'
                                 i_expected_result = expected_result ).
  ENDMETHOD.

  METHOD get_non_fallback_retention_par.
    r_result = cut->get_fallback( ).

    TRY.
        r_result-retention_time = r_result-retention_time + i_offset_days.
      CATCH cx_sy_conversion_overflow.
        " Fix overflow - we need a number GT 0!
        r_result-retention_time = r_result-retention_time + 32769.
    ENDTRY.

    r_result-no_early_delete = boolc( r_result-no_early_delete = abap_false ).
  ENDMETHOD.

  METHOD assert_cut_returns_expected.
    DATA(actual_result) = cut->/usi/if_bal_ce_retention~get_parameters( i_log_object = i_log_object
                                                                        i_sub_object = i_sub_object
                                                                        i_log_level  = i_log_level ).

    cl_abap_unit_assert=>assert_equals( exp  = i_expected_result
                                        act  = actual_result
                                        msg  = |REQUEST: { i_log_object } { i_sub_object }|
                                        quit = if_aunit_constants=>no ).
  ENDMETHOD.
ENDCLASS.
