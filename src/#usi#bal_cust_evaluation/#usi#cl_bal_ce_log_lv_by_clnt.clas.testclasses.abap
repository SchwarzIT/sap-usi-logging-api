*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Test-Double for DAO (To inject customizing)
*--------------------------------------------------------------------*
CLASS lcl_test_double_cust_dao DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_log_lv_by_clnt.

    METHODS clear_mock_data.

    METHODS insert_mock_data_line
      IMPORTING
        i_log_object TYPE balobj_d  OPTIONAL
        i_sub_object TYPE balsubobj OPTIONAL
        i_log_level  TYPE REF TO /usi/cl_bal_enum_log_level
        i_auto_save  TYPE /usi/bal_auto_save_immediately DEFAULT abap_false.

  PRIVATE SECTION.
    DATA mock_data TYPE /usi/if_bal_cd_log_lv_by_clnt=>ty_records.

ENDCLASS.

CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD /usi/if_bal_cd_log_lv_by_clnt~get_records.
    DATA mock_data_line_dref TYPE REF TO /usi/if_bal_cd_log_lv_by_clnt~ty_record.

    LOOP AT mock_data REFERENCE INTO mock_data_line_dref
                      WHERE log_object  IN i_log_object_range
                        AND sub_object  IN i_sub_object_range.
      INSERT mock_data_line_dref->* INTO TABLE r_result.
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
    DATA mock_data_line TYPE /usi/if_bal_cd_log_lv_by_clnt~ty_record.

    mock_data_line-log_object = i_log_object.
    mock_data_line-sub_object = i_sub_object.
    mock_data_line-log_level  = i_log_level->value.
    mock_data_line-auto_save  = i_auto_save.
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
    TYPES ty_log_level_enums TYPE STANDARD TABLE OF REF TO /usi/cl_bal_enum_log_level
                                       WITH NON-UNIQUE DEFAULT KEY.

    METHODS setup.
    METHODS test_fallback_auto_save       FOR TESTING.
    METHODS test_fallback_log_level       FOR TESTING.

    METHODS test_log_object_beats_sub_obj FOR TESTING.
    METHODS test_log_object_beats_generic FOR TESTING.
    METHODS test_sub_object_beats_generic FOR TESTING.

    METHODS get_non_fallback_log_levels
      IMPORTING
        i_amount        TYPE int1
      RETURNING
        VALUE(r_result) TYPE ty_log_level_enums.

    DATA: cut                  TYPE REF TO /usi/cl_bal_ce_log_lv_by_clnt,
          test_double_cust_dao TYPE REF TO lcl_test_double_cust_dao.
ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    DATA unexpected_exception TYPE REF TO /usi/cx_bal_root.

    CREATE OBJECT test_double_cust_dao.
    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_customizing_dao = test_double_cust_dao.
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( i_exception = unexpected_exception
                                                                   i_quit      = if_aunit_constants=>class ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_fallback_auto_save.
    DATA: acutal_result   TYPE /usi/bal_auto_save_pckg_size,
          expected_result TYPE /usi/bal_auto_save_pckg_size.

    expected_result = cut->get_fallback_auto_save( ).
    acutal_result   = cut->/usi/if_bal_ce_log_lv_by_clnt~get_auto_save_package_size( i_log_object = 'NOT_IN'
                                                                                     i_sub_object = 'CUST' ).

    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = acutal_result ).
  ENDMETHOD.

  METHOD test_fallback_log_level.
    DATA: acutal_result   TYPE REF TO /usi/cl_bal_enum_log_level,
          expected_result TYPE REF TO /usi/cl_bal_enum_log_level.

    expected_result = cut->get_fallback_log_level( ).
    acutal_result   = cut->/usi/if_bal_ce_log_lv_by_clnt~get_log_level( i_log_object = 'NOT_IN'
                                                                        i_sub_object = 'CUST' ).

    cl_aunit_assert=>assert_bound( acutal_result ).

    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = acutal_result ).
  ENDMETHOD.

  METHOD test_log_object_beats_sub_obj.
    DATA: non_fallback_log_levels TYPE ty_log_level_enums,
          wrong_result            TYPE REF TO /usi/cl_bal_enum_log_level,
          actual_result           TYPE REF TO /usi/cl_bal_enum_log_level,
          expected_result         TYPE REF TO /usi/cl_bal_enum_log_level.

    non_fallback_log_levels = get_non_fallback_log_levels( 2 ).
    READ TABLE non_fallback_log_levels INTO expected_result INDEX 1.
    READ TABLE non_fallback_log_levels INTO wrong_result    INDEX 2.

    test_double_cust_dao->insert_mock_data_line( i_log_object = 'CUST_LOG_OBJECT'
                                                 i_log_level  = expected_result ).
    test_double_cust_dao->insert_mock_data_line( i_sub_object = 'CUST_SUB_OBJECT'
                                                 i_log_level  = wrong_result ).

    actual_result = cut->/usi/if_bal_ce_log_lv_by_clnt~get_log_level( i_log_object = 'CUST_LOG_OBJECT'
                                                                      i_sub_object = 'CUST_SUB_OBJECT' ).

    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = actual_result ).
  ENDMETHOD.

  METHOD test_log_object_beats_generic.
    DATA: non_fallback_log_levels TYPE ty_log_level_enums,
          wrong_result            TYPE REF TO /usi/cl_bal_enum_log_level,
          actual_result           TYPE REF TO /usi/cl_bal_enum_log_level,
          expected_result         TYPE REF TO /usi/cl_bal_enum_log_level.

    non_fallback_log_levels = get_non_fallback_log_levels( 2 ).
    READ TABLE non_fallback_log_levels INTO expected_result INDEX 1.
    READ TABLE non_fallback_log_levels INTO wrong_result    INDEX 2.

    test_double_cust_dao->insert_mock_data_line( i_log_object = 'CUST_LOG_OBJECT'
                                                 i_log_level  = expected_result ).
    test_double_cust_dao->insert_mock_data_line( wrong_result ).

    actual_result = cut->/usi/if_bal_ce_log_lv_by_clnt~get_log_level( i_log_object = 'CUST_LOG_OBJECT' ).

    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = actual_result ).
  ENDMETHOD.

  METHOD test_sub_object_beats_generic.
    DATA: non_fallback_log_levels TYPE ty_log_level_enums,
          wrong_result            TYPE REF TO /usi/cl_bal_enum_log_level,
          actual_result           TYPE REF TO /usi/cl_bal_enum_log_level,
          expected_result         TYPE REF TO /usi/cl_bal_enum_log_level.

    non_fallback_log_levels = get_non_fallback_log_levels( 2 ).
    READ TABLE non_fallback_log_levels INTO expected_result INDEX 1.
    READ TABLE non_fallback_log_levels INTO wrong_result    INDEX 2.

    test_double_cust_dao->insert_mock_data_line( i_sub_object = 'CUST_SUB_OBJECT'
                                                 i_log_level  = expected_result ).
    test_double_cust_dao->insert_mock_data_line( i_sub_object = space
                                                 i_log_level  = wrong_result ).

    actual_result = cut->/usi/if_bal_ce_log_lv_by_clnt~get_log_level( i_log_object = 'NOT_IN_CUST'
                                                                      i_sub_object = 'CUST_SUB_OBJECT' ).

    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = actual_result ).
  ENDMETHOD.

  METHOD get_non_fallback_log_levels.
    DATA: fallback_log_level TYPE REF TO /usi/cl_bal_enum_log_level,
          log_level          TYPE REF TO /usi/cl_bal_enum_log_level,
          log_level_value    TYPE /usi/bal_log_level.

    fallback_log_level = cut->get_fallback_log_level( ).

    log_level_value = /usi/cl_bal_enum_log_level=>nothing->value.
    WHILE lines( r_result ) LT i_amount.
      TRY.
          log_level = /usi/cl_bal_enum_log_level=>get_by_value( log_level_value ).
          IF log_level NE fallback_log_level.
            INSERT log_level INTO TABLE r_result.
          ENDIF.
          log_level_value = log_level_value + 1.
        CATCH /usi/cx_bal_root.
          cl_aunit_assert=>abort( msg  = 'Too many values requested'
                                  quit = if_aunit_constants=>method ).
      ENDTRY.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
