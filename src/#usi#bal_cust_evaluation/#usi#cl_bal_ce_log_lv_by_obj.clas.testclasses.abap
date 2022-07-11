*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Test-Double for DAO (To inject customizing)
*--------------------------------------------------------------------*
CLASS lcl_test_double_cust_dao DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES: /usi/if_bal_cd_log_lv_by_obj.

    METHODS clear_mock_data.

    METHODS insert_mock_data_line
      IMPORTING
        i_log_object             TYPE balobj_d
        i_sub_object             TYPE balsubobj
        i_log_level              TYPE /usi/bal_log_level
        i_auto_save_package_size TYPE /usi/bal_auto_save_pckg_size DEFAULT 0.

  PRIVATE SECTION.
    ALIASES: ty_records FOR /usi/if_bal_cd_log_lv_by_obj~ty_records,
             ty_record  FOR /usi/if_bal_cd_log_lv_by_obj~ty_record.

    DATA: mock_data TYPE ty_records.
ENDCLASS.

CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD /usi/if_bal_cd_log_lv_by_obj~get_records.
    DATA mock_data_line_dref TYPE REF TO ty_record.
    LOOP AT mock_data
      REFERENCE INTO mock_data_line_dref
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
    DATA mock_data_line TYPE ty_record.
    mock_data_line-log_object             = i_log_object.
    mock_data_line-sub_object             = i_sub_object.
    mock_data_line-log_level              = i_log_level.
    mock_data_line-auto_save_package_size = i_auto_save_package_size.
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
    TYPES: ty_non_default_log_levels TYPE STANDARD TABLE OF REF TO /usi/cl_bal_enum_log_level
                                              WITH NON-UNIQUE DEFAULT KEY.

    METHODS setup.
    METHODS test_fallback_auto_save FOR TESTING.
    METHODS test_fallback_log_level FOR TESTING.
    METHODS test_prio_log_object    FOR TESTING.
    METHODS test_prio_sub_object    FOR TESTING.

    METHODS get_non_fallback_log_levels
      IMPORTING
        i_amount        TYPE int1
      RETURNING
        VALUE(r_result) TYPE ty_non_default_log_levels.

    DATA: cut                  TYPE REF TO /usi/cl_bal_ce_log_lv_by_obj,
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
    acutal_result   = cut->/usi/if_bal_ce_log_lv_by_obj~get_auto_save_package_size( i_log_object = 'NOT'
                                                                                    i_sub_object = 'IN_CUST' ).

    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = acutal_result ).
  ENDMETHOD.

  METHOD test_fallback_log_level.
    DATA: acutal_result   TYPE REF TO /usi/cl_bal_enum_log_level,
          expected_result TYPE REF TO /usi/cl_bal_enum_log_level.

    expected_result = cut->get_fallback_log_level( ).
    acutal_result   = cut->/usi/if_bal_ce_log_lv_by_obj~get_log_level( i_log_object = 'NOT'
                                                                       i_sub_object = 'IN_CUST' ).

    cl_aunit_assert=>assert_bound( acutal_result ).
    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = acutal_result ).
  ENDMETHOD.

  METHOD test_prio_log_object.
    DATA: non_fallback_log_levels TYPE ty_non_default_log_levels,
          actual_result           TYPE REF TO /usi/cl_bal_enum_log_level,
          expected_result         TYPE REF TO /usi/cl_bal_enum_log_level,
          wrong_result            TYPE REF TO /usi/cl_bal_enum_log_level.

    non_fallback_log_levels = get_non_fallback_log_levels( 2 ).
    READ TABLE non_fallback_log_levels INTO expected_result INDEX 1.
    READ TABLE non_fallback_log_levels INTO wrong_result    INDEX 2.

    test_double_cust_dao->insert_mock_data_line( i_log_object = 'CUST_LOG_OBJECT'
                                                 i_sub_object = space
                                                 i_log_level  = expected_result->value ).
    test_double_cust_dao->insert_mock_data_line( i_log_object = space
                                                 i_sub_object = 'CUST_SUB_OBJECT'
                                                 i_log_level  = wrong_result->value ).

    actual_result = cut->/usi/if_bal_ce_log_lv_by_obj~get_log_level( i_log_object = 'CUST_LOG_OBJECT'
                                                                     i_sub_object = 'CUST_SUB_OBJECT' ).

    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = actual_result ).
  ENDMETHOD.

  METHOD test_prio_sub_object.
    DATA: non_fallback_log_levels TYPE ty_non_default_log_levels,
          actual_result           TYPE REF TO /usi/cl_bal_enum_log_level,
          expected_result         TYPE REF TO /usi/cl_bal_enum_log_level,
          wrong_result            TYPE REF TO /usi/cl_bal_enum_log_level.

    non_fallback_log_levels = get_non_fallback_log_levels( 2 ).
    READ TABLE non_fallback_log_levels INTO expected_result INDEX 1.
    READ TABLE non_fallback_log_levels INTO wrong_result    INDEX 2.

    test_double_cust_dao->insert_mock_data_line( i_log_object = 'CUST_LOG_OBJECT'
                                                 i_sub_object = space
                                                 i_log_level  = wrong_result->value ).
    test_double_cust_dao->insert_mock_data_line( i_log_object = space
                                                 i_sub_object = 'CUST_SUB_OBJECT'
                                                 i_log_level  = expected_result->value ).

    actual_result = cut->/usi/if_bal_ce_log_lv_by_obj~get_log_level( i_log_object = 'NOT_IN_CUST'
                                                                     i_sub_object = 'CUST_SUB_OBJECT' ).

    cl_aunit_assert=>assert_equals( exp = expected_result
                                    act = actual_result ).
  ENDMETHOD.

  METHOD get_non_fallback_log_levels.
    DATA: log_level       TYPE REF TO /usi/cl_bal_enum_log_level,
          log_level_value TYPE /usi/bal_log_level.

    log_level_value = /usi/cl_bal_enum_log_level=>nothing->value.
    WHILE lines( r_result ) LT i_amount.
      TRY.
          log_level = /usi/cl_bal_enum_log_level=>get_by_value( log_level_value ).
          IF log_level NE cut->get_fallback_log_level( ).
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
