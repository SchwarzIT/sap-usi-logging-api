*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Test-Double for DAO (To inject customizing)
*--------------------------------------------------------------------*
CLASS lcl_test_double_cust_dao DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_retention.

    METHODS clear_mock_data.

    METHODS insert_mock_data_line
      IMPORTING
        i_log_object       TYPE balobj_d  OPTIONAL
        i_sub_object       TYPE balsubobj OPTIONAL
        i_log_level        TYPE REF TO /usi/cl_bal_enum_log_level DEFAULT /usi/cl_bal_enum_log_level=>additional_info
        i_retention_params TYPE /usi/bal_retention_parameters.

  PRIVATE SECTION.
    DATA mock_data TYPE /usi/if_bal_cd_retention=>ty_records.
ENDCLASS.

CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD /usi/if_bal_cd_retention~get_records.
    FIELD-SYMBOLS <mock_data_line> TYPE LINE OF /usi/if_bal_cd_retention=>ty_records.

    LOOP AT mock_data ASSIGNING <mock_data_line>
        WHERE log_object IN i_log_object_range
          AND sub_object IN i_sub_object_range
          AND log_level  EQ i_log_level.
      INSERT <mock_data_line> INTO TABLE r_result.
    ENDLOOP.

    IF r_result IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.

  METHOD clear_mock_data.
    CLEAR mock_data.
  ENDMETHOD.

  METHOD insert_mock_data_line.
    DATA mock_data_line TYPE LINE OF /usi/if_bal_cd_retention=>ty_records.
    mock_data_line-log_object           = i_log_object.
    mock_data_line-sub_object           = i_sub_object.
    mock_data_line-log_level            = i_log_level->value.
    mock_data_line-retention_parameters = i_retention_params.
    INSERT mock_data_line INTO TABLE mock_data.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test
*--------------------------------------------------------------------*
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: test_double_cust_dao TYPE REF TO lcl_test_double_cust_dao,
          cut                  TYPE REF TO /usi/cl_bal_ce_retention.

    METHODS setup.
    METHODS reset_cut.

    METHODS test_fallback_for_no_cust     FOR TESTING.
    METHODS test_no_match_for_log_object  FOR TESTING.
    METHODS test_no_match_for_sub_object  FOR TESTING.
    METHODS test_cust_entry_priorities    FOR TESTING.

    METHODS get_non_fallback_retention_par
      IMPORTING
        i_offset_days   TYPE int2
      RETURNING
        VALUE(r_result) TYPE /usi/bal_retention_parameters.

    METHODS assert_expected_result
      IMPORTING
        i_log_object      TYPE balobj_d
        i_sub_object      TYPE balsubobj OPTIONAL
        i_log_level       TYPE REF TO /usi/cl_bal_enum_log_level DEFAULT /usi/cl_bal_enum_log_level=>additional_info
        i_expected_result TYPE /usi/bal_retention_parameters.
ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT test_double_cust_dao.
    reset_cut( ).
  ENDMETHOD.

  METHOD reset_cut.
    DATA exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_customizing_dao = test_double_cust_dao.
      CATCH /usi/cx_bal_root INTO exception.
        /usi/cl_bal_aunit_exception=>abort_on_unexpected_exception( i_exception = exception
                                                                    i_quit      = if_aunit_constants=>method ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_fallback_for_no_cust.
    DATA expected_result TYPE /usi/bal_retention_parameters.

    expected_result = cut->get_fallback( ).
    assert_expected_result( i_log_object      = 'DUMMY'
                            i_expected_result = expected_result ).
  ENDMETHOD.

  METHOD test_no_match_for_log_object.
    DATA: expected_result               TYPE /usi/bal_retention_parameters,
          non_fallback_retention_params TYPE /usi/bal_retention_parameters.

    non_fallback_retention_params = get_non_fallback_retention_par( 10 ).

    test_double_cust_dao->insert_mock_data_line( i_log_object       = 'CUST_LOG_OBJECT'
                                                 i_retention_params = non_fallback_retention_params ).

    expected_result = cut->get_fallback( ).
    assert_expected_result( i_log_object      = 'NOT_IN_CUST'
                            i_expected_result = expected_result ).
  ENDMETHOD.

  METHOD test_no_match_for_sub_object.
    DATA: expected_result               TYPE /usi/bal_retention_parameters,
          non_fallback_retention_params TYPE /usi/bal_retention_parameters.

    non_fallback_retention_params = get_non_fallback_retention_par( 20 ).

    test_double_cust_dao->insert_mock_data_line( i_sub_object       = 'CUST_SUB_OBJECT'
                                                 i_retention_params = non_fallback_retention_params ).

    expected_result = cut->get_fallback( ).
    assert_expected_result( i_log_object      = space
                            i_sub_object      = 'NOT_IN_CUST'
                            i_expected_result = expected_result ).
  ENDMETHOD.

  METHOD test_cust_entry_priorities.
    DATA: non_fallback_retention_params TYPE /usi/bal_retention_parameters,
          cust_key_log_object           TYPE balobj_d,
          cust_key_sub_object           TYPE balsubobj.

    " Prio 4
    non_fallback_retention_params = get_non_fallback_retention_par( 4 ).
    test_double_cust_dao->insert_mock_data_line( i_log_object       = space
                                                 i_sub_object       = space
                                                 i_retention_params = non_fallback_retention_params ).

    assert_expected_result( i_log_object      = 'NOT_IN_CUST'
                            i_sub_object      = 'NOT_IN_CUST'
                            i_expected_result = non_fallback_retention_params ).

    " Prio 3
    non_fallback_retention_params = get_non_fallback_retention_par( 3 ).
    test_double_cust_dao->insert_mock_data_line( i_log_object       = space
                                                 i_sub_object       = 'CUST_SUB_OBJECT'
                                                 i_retention_params = non_fallback_retention_params ).

    assert_expected_result( i_log_object      = 'NOT_IN_CUST'
                            i_sub_object      = 'CUST_SUB_OBJECT'
                            i_expected_result = non_fallback_retention_params ).

    " Prio 2
    non_fallback_retention_params = get_non_fallback_retention_par( 2 ).
    test_double_cust_dao->insert_mock_data_line( i_log_object       = 'CUST_LOG_OBJECT'
                                                 i_sub_object       = space
                                                 i_retention_params = non_fallback_retention_params ).

    assert_expected_result( i_log_object      = 'CUST_LOG_OBJECT'
                            i_sub_object      = 'NOT_IN_CUST'
                            i_expected_result = non_fallback_retention_params ).

    " Prio 1
    non_fallback_retention_params = get_non_fallback_retention_par( 1 ).
    test_double_cust_dao->insert_mock_data_line( i_log_object       = 'CUST_LOG_OBJECT'
                                                 i_sub_object       = 'CUST_SUB_OBJECT'
                                                 i_retention_params = non_fallback_retention_params ).

    assert_expected_result( i_log_object      = 'CUST_LOG_OBJECT'
                            i_sub_object      = 'CUST_SUB_OBJECT'
                            i_expected_result = non_fallback_retention_params ).
  ENDMETHOD.

  METHOD get_non_fallback_retention_par.
    r_result = cut->get_fallback( ).

    TRY.
        r_result-retention_time = r_result-retention_time + i_offset_days.
      CATCH cx_sy_conversion_overflow.
        " Fix overflow - we need a number GT 0!
        r_result-retention_time = r_result-retention_time + 32769.
    ENDTRY.

    r_result-no_early_delete = boolc( r_result-no_early_delete EQ abap_false ).
  ENDMETHOD.

  METHOD assert_expected_result.
    DATA: actual_result TYPE /usi/bal_retention_parameters,
          message       TYPE string.

    reset_cut( ).

    actual_result = cut->/usi/if_bal_ce_retention~get_parameters( i_log_object = i_log_object
                                                                  i_sub_object = i_sub_object
                                                                  i_log_level  = i_log_level ).

    CONCATENATE `REQUEST:` i_log_object i_sub_object INTO message IN CHARACTER MODE SEPARATED BY space.
    cl_aunit_assert=>assert_equals( msg  = message
                                    act  = actual_result
                                    exp  = i_expected_result
                                    quit = if_aunit_constants=>no ).
  ENDMETHOD.
ENDCLASS.
