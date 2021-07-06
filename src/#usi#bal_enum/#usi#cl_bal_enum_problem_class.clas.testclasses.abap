*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Test all public static object references are bound & read-only
*--------------------------------------------------------------------*
CLASS lcl_unit_test_public_attribs DEFINITION FINAL FOR TESTING CREATE PUBLIC.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
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
CLASS lcl_unit_test_values DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS verify_static_instances FOR TESTING.

    METHODS assert_value
      IMPORTING
        i_instance TYPE REF TO /USI/CL_BAL_ENUM_PROBLEM_CLASS
        i_expected TYPE balprobcl.

    METHODS assert_bound
      IMPORTING
        i_instance TYPE REF TO /USI/CL_BAL_ENUM_PROBLEM_CLASS.
ENDCLASS.

CLASS lcl_unit_test_values IMPLEMENTATION.
  METHOD verify_static_instances.
    assert_value(
      i_instance = /USI/CL_BAL_ENUM_PROBLEM_CLASS=>very_important
      i_expected = '1'
    ).

    assert_value(
      i_instance = /USI/CL_BAL_ENUM_PROBLEM_CLASS=>important
      i_expected = '2'
    ).

    assert_value(
      i_instance = /USI/CL_BAL_ENUM_PROBLEM_CLASS=>medium
      i_expected = '3'
    ).

    assert_value(
      i_instance = /USI/CL_BAL_ENUM_PROBLEM_CLASS=>additional_information
      i_expected = '4'
    ).

    assert_value(
      i_instance = /USI/CL_BAL_ENUM_PROBLEM_CLASS=>other
      i_expected = ' '
    ).
  ENDMETHOD.

  METHOD assert_value.
    assert_bound( i_instance ).

    cl_aunit_assert=>assert_equals(
      act = i_instance->value
      exp = i_expected
    ).
  ENDMETHOD.

  METHOD assert_bound.
    cl_aunit_assert=>assert_bound( i_instance ).
  ENDMETHOD.
ENDCLASS.
