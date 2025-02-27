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
        i_instance TYPE REF TO /usi/cl_bal_enum_problem_class
        i_expected TYPE balprobcl.

    METHODS assert_bound
      IMPORTING
        i_instance TYPE REF TO /usi/cl_bal_enum_problem_class.
ENDCLASS.

CLASS lcl_unit_test_values IMPLEMENTATION.
  METHOD verify_static_instances.
    assert_value( i_instance = /usi/cl_bal_enum_problem_class=>very_important
                  i_expected = '1' ).

    assert_value( i_instance = /usi/cl_bal_enum_problem_class=>important
                  i_expected = '2' ).

    assert_value( i_instance = /usi/cl_bal_enum_problem_class=>medium
                  i_expected = '3' ).

    assert_value( i_instance = /usi/cl_bal_enum_problem_class=>additional_information
                  i_expected = '4' ).

    assert_value( i_instance = /usi/cl_bal_enum_problem_class=>other
                  i_expected = ' ' ).
  ENDMETHOD.

  METHOD assert_value.
    assert_bound( i_instance ).

    cl_abap_unit_assert=>assert_equals( exp = i_expected
                                        act = i_instance->value ).
  ENDMETHOD.

  METHOD assert_bound.
    cl_abap_unit_assert=>assert_bound( i_instance ).
  ENDMETHOD.
ENDCLASS.
