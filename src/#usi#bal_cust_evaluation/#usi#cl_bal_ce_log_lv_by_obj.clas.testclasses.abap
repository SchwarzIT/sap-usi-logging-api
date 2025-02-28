*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Test-Double for DAO (To inject customizing)
" ---------------------------------------------------------------------
CLASS lcl_test_double_cust_dao DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_log_lv_by_obj.

    METHODS clear_mock_data.

    METHODS insert_mock_data_line
      IMPORTING
        i_log_object             TYPE balobj_d
        i_sub_object             TYPE balsubobj
        i_log_level              TYPE /usi/bal_log_level
        i_auto_save_package_size TYPE /usi/bal_auto_save_pckg_size DEFAULT 0.

  PRIVATE SECTION.
    DATA mock_data TYPE /usi/if_bal_cd_log_lv_by_obj~ty_records.

ENDCLASS.


CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD /usi/if_bal_cd_log_lv_by_obj~get_records.
    LOOP AT mock_data
         REFERENCE INTO DATA(mock_data_line_dref)
         WHERE     log_object IN i_log_object_range
               AND sub_object IN i_sub_object_range.
      INSERT mock_data_line_dref->* INTO TABLE r_result.
    ENDLOOP.

    IF r_result IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.

  METHOD clear_mock_data.
    CLEAR mock_data.
  ENDMETHOD.

  METHOD insert_mock_data_line.
    INSERT VALUE #( log_object             = i_log_object
                    sub_object             = i_sub_object
                    log_level              = i_log_level
                    auto_save_package_size = i_auto_save_package_size )
           INTO TABLE mock_data.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test
" ---------------------------------------------------------------------
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES ty_non_default_log_levels TYPE STANDARD TABLE OF REF TO /usi/cl_bal_enum_log_level WITH EMPTY KEY.

    METHODS setup.
    METHODS test_fallback_auto_save FOR TESTING.
    METHODS test_fallback_log_level FOR TESTING.
    METHODS test_prio_log_object    FOR TESTING RAISING /usi/cx_bal_root.
    METHODS test_prio_sub_object    FOR TESTING RAISING /usi/cx_bal_root.

    METHODS get_non_fallback_log_levels
      IMPORTING i_amount        TYPE int1
      RETURNING VALUE(r_result) TYPE ty_non_default_log_levels
      RAISING   /usi/cx_bal_root.

    DATA: cut                  TYPE REF TO /usi/cl_bal_ce_log_lv_by_obj,
          test_double_cust_dao TYPE REF TO lcl_test_double_cust_dao.
ENDCLASS.


CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    test_double_cust_dao = NEW #( ).
    cut = NEW #( i_customizing_dao = test_double_cust_dao ).
  ENDMETHOD.

  METHOD test_fallback_auto_save.
    DATA(expected_result) = cut->get_fallback_auto_save( ).
    DATA(acutal_result)   = cut->/usi/if_bal_ce_log_lv_by_obj~get_auto_save_package_size( i_log_object = 'NOT'
                                                                                          i_sub_object = 'IN_CUST' ).

    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = acutal_result ).
  ENDMETHOD.

  METHOD test_fallback_log_level.
    DATA(expected_result) = cut->get_fallback_log_level( ).
    DATA(acutal_result)   = cut->/usi/if_bal_ce_log_lv_by_obj~get_log_level( i_log_object = 'NOT'
                                                                             i_sub_object = 'IN_CUST' ).

    cl_abap_unit_assert=>assert_bound( acutal_result ).
    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = acutal_result ).
  ENDMETHOD.

  METHOD test_prio_log_object.
    DATA(non_fallback_log_levels) = get_non_fallback_log_levels( 2 ).
    DATA(expected_result)         = non_fallback_log_levels[ 1 ].
    DATA(wrong_result)            = non_fallback_log_levels[ 2 ].

    test_double_cust_dao->insert_mock_data_line( i_log_object = 'CUST_LOG_OBJECT'
                                                 i_sub_object = space
                                                 i_log_level  = expected_result->value ).
    test_double_cust_dao->insert_mock_data_line( i_log_object = space
                                                 i_sub_object = 'CUST_SUB_OBJECT'
                                                 i_log_level  = wrong_result->value ).

    DATA(actual_result) = cut->/usi/if_bal_ce_log_lv_by_obj~get_log_level( i_log_object = 'CUST_LOG_OBJECT'
                                                                           i_sub_object = 'CUST_SUB_OBJECT' ).

    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = actual_result ).
  ENDMETHOD.

  METHOD test_prio_sub_object.
    DATA(non_fallback_log_levels) = get_non_fallback_log_levels( 2 ).
    DATA(expected_result)         = non_fallback_log_levels[ 1 ].
    DATA(wrong_result)            = non_fallback_log_levels[ 2 ].

    test_double_cust_dao->insert_mock_data_line( i_log_object = 'CUST_LOG_OBJECT'
                                                 i_sub_object = space
                                                 i_log_level  = wrong_result->value ).
    test_double_cust_dao->insert_mock_data_line( i_log_object = space
                                                 i_sub_object = 'CUST_SUB_OBJECT'
                                                 i_log_level  = expected_result->value ).

    DATA(actual_result) = cut->/usi/if_bal_ce_log_lv_by_obj~get_log_level( i_log_object = 'NOT_IN_CUST'
                                                                           i_sub_object = 'CUST_SUB_OBJECT' ).

    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = actual_result ).
  ENDMETHOD.

  METHOD get_non_fallback_log_levels.
    DATA(fallback_log_level) = cut->get_fallback_log_level( ).

    DATA(log_level_value) = /usi/cl_bal_enum_log_level=>nothing->value.
    WHILE lines( r_result ) < i_amount.
      DATA(log_level) = /usi/cl_bal_enum_log_level=>get_by_value( log_level_value ).
      IF log_level <> fallback_log_level.
        INSERT log_level INTO TABLE r_result.
      ENDIF.
      log_level_value = log_level_value + 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
