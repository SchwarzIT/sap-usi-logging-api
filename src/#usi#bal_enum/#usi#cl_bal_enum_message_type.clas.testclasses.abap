*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Test all public static object references are bound & read-only
*--------------------------------------------------------------------*
CLASS lcl_unit_test_public_attribs DEFINITION FINAL FOR TESTING CREATE PUBLIC RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut_description TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl.

    METHODS setup.
    METHODS assert_public_is_read_only FOR TESTING.
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

*--------------------------------------------------------------------*
* Check the values of the public static attributes
*--------------------------------------------------------------------*
CLASS lcl_unit_test_values DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS verify_static_instances FOR TESTING.

    METHODS assert_value
      IMPORTING
        i_instance TYPE REF TO /usi/cl_bal_enum_message_type
        i_expected TYPE symsgty.

    METHODS assert_bound
      IMPORTING
        i_instance TYPE REF TO /usi/cl_bal_enum_message_type.
ENDCLASS.

CLASS lcl_unit_test_values IMPLEMENTATION.
  METHOD verify_static_instances.
    assert_value( i_instance = /usi/cl_bal_enum_message_type=>abend
                  i_expected = 'A' ).

    assert_value( i_instance = /usi/cl_bal_enum_message_type=>error
                  i_expected = 'E' ).

    assert_value( i_instance = /usi/cl_bal_enum_message_type=>exit
                  i_expected = 'X' ).

    assert_value( i_instance = /usi/cl_bal_enum_message_type=>information
                  i_expected = 'I' ).

    assert_value( i_instance = /usi/cl_bal_enum_message_type=>success
                  i_expected = 'S' ).

    assert_value( i_instance = /usi/cl_bal_enum_message_type=>warning
                  i_expected = 'W' ).
  ENDMETHOD.

  METHOD assert_value.
    assert_bound( i_instance ).

    cl_abap_unit_assert=>assert_equals( exp = i_expected
                                        act = i_instance->value ).
  ENDMETHOD.

  METHOD assert_bound.
    cl_abap_unit_assert=>assert_bound( act = i_instance ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Check the various helper methods of the class
*--------------------------------------------------------------------*
CLASS lcl_unit_test_behavior DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_raise_on_invalid_value     FOR TESTING.
    METHODS test_get_by_value               FOR TESTING.

    METHODS assert_get_by_value_returns
      IMPORTING
        i_value    TYPE symsgty
        i_expected TYPE REF TO /usi/cl_bal_enum_message_type.
ENDCLASS.

CLASS lcl_unit_test_behavior IMPLEMENTATION.
  METHOD test_raise_on_invalid_value.
    CONSTANTS invalid_message_type TYPE symsgty VALUE '#'.

    TRY.
        /usi/cl_bal_enum_message_type=>get_by_value( invalid_message_type ).
        cl_abap_unit_assert=>fail( msg = 'Exception should have been raised, as input was invalid!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_by_value.
    assert_get_by_value_returns( i_value    = /usi/cl_bal_enum_message_type=>abend->value
                                 i_expected = /usi/cl_bal_enum_message_type=>abend ).

    assert_get_by_value_returns( i_value    = /usi/cl_bal_enum_message_type=>error->value
                                 i_expected = /usi/cl_bal_enum_message_type=>error ).

    assert_get_by_value_returns( i_value    = /usi/cl_bal_enum_message_type=>exit->value
                                 i_expected = /usi/cl_bal_enum_message_type=>exit ).

    assert_get_by_value_returns( i_value    = /usi/cl_bal_enum_message_type=>information->value
                                 i_expected = /usi/cl_bal_enum_message_type=>information ).

    assert_get_by_value_returns( i_value    = /usi/cl_bal_enum_message_type=>success->value
                                 i_expected = /usi/cl_bal_enum_message_type=>success ).

    assert_get_by_value_returns( i_value    = /usi/cl_bal_enum_message_type=>warning->value
                                 i_expected = /usi/cl_bal_enum_message_type=>warning ).
  ENDMETHOD.

  METHOD assert_get_by_value_returns.
    DATA: actual_result        TYPE REF TO /usi/cl_bal_enum_message_type,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = /usi/cl_bal_enum_message_type=>get_by_value( i_value ).

        cl_abap_unit_assert=>assert_equals( exp = i_expected
                                            act = actual_result
                                            msg = `GET_BY_VALUE( ) returns wrong instance!` ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
