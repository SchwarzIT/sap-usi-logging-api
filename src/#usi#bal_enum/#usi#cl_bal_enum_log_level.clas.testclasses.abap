*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Test all public static object references are bound & read-only
" ---------------------------------------------------------------------
CLASS lcl_unit_test_public_attribs DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut_description TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl.

    METHODS setup.
    METHODS assert_public_is_read_only  FOR TESTING.
    METHODS assert_public_statics_bound FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_public_attribs IMPLEMENTATION.
  METHOD setup.
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
  ENDMETHOD.

  METHOD assert_public_is_read_only.
    cut_description->assert_public_attrib_read_only( ).
  ENDMETHOD.

  METHOD assert_public_statics_bound.
    cut_description->assert_publ_static_orefs_bound( ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Check the various helper methods of the class
" ---------------------------------------------------------------------
CLASS lcl_unit_test_behavior DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_raise_on_invalid_value    FOR TESTING.
    METHODS test_get_by_value              FOR TESTING RAISING /usi/cx_bal_root.
    METHODS test_static_instances          FOR TESTING.
    METHODS test_is_higher                 FOR TESTING.
    METHODS test_is_problem_class_relevant FOR TESTING RAISING /usi/cx_bal_root.

    METHODS assert_get_by_value_returns
      IMPORTING i_value    TYPE /usi/bal_log_level
                i_expected TYPE REF TO /usi/cl_bal_enum_log_level
      RAISING   /usi/cx_bal_root.

    METHODS assert_value
      IMPORTING i_instance TYPE REF TO /usi/cl_bal_enum_log_level
                i_expected TYPE /usi/bal_log_level.

    METHODS assert_bound
      IMPORTING i_instance TYPE REF TO /usi/cl_bal_enum_log_level.

    METHODS assert_prob_class_relevant_for
      IMPORTING i_problem_class TYPE REF TO /usi/cl_bal_enum_problem_class
                i_relevant_from TYPE /usi/bal_log_level
                i_relevant_to   TYPE /usi/bal_log_level
      RAISING   /usi/cx_bal_root.
ENDCLASS.


CLASS lcl_unit_test_behavior IMPLEMENTATION.
  METHOD test_raise_on_invalid_value.
    TRY.
        /usi/cl_bal_enum_log_level=>get_by_value( 9 ).
        cl_abap_unit_assert=>fail( msg = 'Exception should have been raised, as input was invalid!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_by_value.
    assert_get_by_value_returns( i_value    = 0
                                 i_expected = /usi/cl_bal_enum_log_level=>nothing ).

    assert_get_by_value_returns( i_value    = 1
                                 i_expected = /usi/cl_bal_enum_log_level=>very_important ).

    assert_get_by_value_returns( i_value    = 2
                                 i_expected = /usi/cl_bal_enum_log_level=>important ).

    assert_get_by_value_returns( i_value    = 3
                                 i_expected = /usi/cl_bal_enum_log_level=>medium ).

    assert_get_by_value_returns( i_value    = 4
                                 i_expected = /usi/cl_bal_enum_log_level=>additional_info ).

    assert_get_by_value_returns( i_value    = 5
                                 i_expected = /usi/cl_bal_enum_log_level=>other ).

    assert_get_by_value_returns( i_value    = 6
                                 i_expected = /usi/cl_bal_enum_log_level=>everything ).
  ENDMETHOD.

  METHOD assert_get_by_value_returns.
    cl_abap_unit_assert=>assert_equals( exp = i_expected
                                        act = /usi/cl_bal_enum_log_level=>get_by_value( i_value )
                                        msg = `GET_BY_VALUE( ) returns wrong instance!` ).
  ENDMETHOD.

  METHOD test_static_instances.
    assert_value( i_instance = /usi/cl_bal_enum_log_level=>nothing
                  i_expected = '0' ).

    assert_value( i_instance = /usi/cl_bal_enum_log_level=>very_important
                  i_expected = '1' ).

    assert_value( i_instance = /usi/cl_bal_enum_log_level=>important
                  i_expected = '2' ).

    assert_value( i_instance = /usi/cl_bal_enum_log_level=>medium
                  i_expected = '3' ).

    assert_value( i_instance = /usi/cl_bal_enum_log_level=>additional_info
                  i_expected = '4' ).

    assert_value( i_instance = /usi/cl_bal_enum_log_level=>other
                  i_expected = '5' ).

    assert_value( i_instance = /usi/cl_bal_enum_log_level=>everything
                  i_expected = '6' ).
  ENDMETHOD.

  METHOD assert_value.
    assert_bound( i_instance ).

    cl_abap_unit_assert=>assert_equals( exp = i_expected
                                        act = i_instance->value ).
  ENDMETHOD.

  METHOD assert_bound.
    cl_abap_unit_assert=>assert_bound( act = i_instance ).
  ENDMETHOD.

  METHOD test_is_higher.
    DATA: actual_result     TYPE abap_bool,
          unbound_log_level TYPE REF TO /usi/cl_bal_enum_log_level.

    actual_result = /usi/cl_bal_enum_log_level=>everything->is_higher_than( /usi/cl_bal_enum_log_level=>nothing ).
    cl_abap_unit_assert=>assert_true( actual_result ).

    actual_result = /usi/cl_bal_enum_log_level=>nothing->is_higher_than( /usi/cl_bal_enum_log_level=>very_important ).
    cl_abap_unit_assert=>assert_false( actual_result ).

    actual_result = /usi/cl_bal_enum_log_level=>very_important->is_higher_than(
                        /usi/cl_bal_enum_log_level=>very_important ).
    cl_abap_unit_assert=>assert_false( actual_result ).

    actual_result = /usi/cl_bal_enum_log_level=>very_important->is_higher_than( unbound_log_level ).
    cl_abap_unit_assert=>assert_false( actual_result ).
  ENDMETHOD.

  METHOD test_is_problem_class_relevant.
    assert_prob_class_relevant_for( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                    i_relevant_from = /usi/cl_bal_enum_log_level=>very_important->value
                                    i_relevant_to   = /usi/cl_bal_enum_log_level=>everything->value ).

    assert_prob_class_relevant_for( i_problem_class = /usi/cl_bal_enum_problem_class=>important
                                    i_relevant_from = /usi/cl_bal_enum_log_level=>important->value
                                    i_relevant_to   = /usi/cl_bal_enum_log_level=>everything->value ).

    assert_prob_class_relevant_for( i_problem_class = /usi/cl_bal_enum_problem_class=>medium
                                    i_relevant_from = /usi/cl_bal_enum_log_level=>medium->value
                                    i_relevant_to   = /usi/cl_bal_enum_log_level=>everything->value ).

    assert_prob_class_relevant_for( i_problem_class = /usi/cl_bal_enum_problem_class=>additional_information
                                    i_relevant_from = /usi/cl_bal_enum_log_level=>additional_info->value
                                    i_relevant_to   = /usi/cl_bal_enum_log_level=>everything->value ).

    assert_prob_class_relevant_for( i_problem_class = /usi/cl_bal_enum_problem_class=>other
                                    i_relevant_from = /usi/cl_bal_enum_log_level=>other->value
                                    i_relevant_to   = /usi/cl_bal_enum_log_level=>everything->value ).
  ENDMETHOD.

  METHOD assert_prob_class_relevant_for.
    DATA: actual_result   TYPE abap_bool,
          cut             TYPE REF TO /usi/cl_bal_enum_log_level,
          expected_result TYPE abap_bool,
          log_level       TYPE /usi/bal_log_level.

    log_level = /usi/cl_bal_enum_log_level=>nothing->value.
    WHILE log_level <= /usi/cl_bal_enum_log_level=>everything->value.
      cut = /usi/cl_bal_enum_log_level=>get_by_value( log_level ).

      expected_result = boolc( log_level BETWEEN i_relevant_from AND i_relevant_to ).
      actual_result = cut->is_problem_class_relevant( i_problem_class ).

      cl_abap_unit_assert=>assert_equals( exp = expected_result
                                          act = actual_result
                                          msg = `Assignment of problem classes to log levels is broken!` ).

      log_level = log_level + 1.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
