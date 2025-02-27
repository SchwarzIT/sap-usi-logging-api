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
" Check the values of the public static attributes
" ---------------------------------------------------------------------
CLASS lcl_unit_test_values DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS verify_static_instances FOR TESTING.

    METHODS assert_value
      IMPORTING
        i_instance TYPE REF TO /usi/cl_bal_enum_detail_level
        i_expected TYPE ballevel.

    METHODS assert_bound
      IMPORTING
        i_instance TYPE REF TO /usi/cl_bal_enum_detail_level.
ENDCLASS.


CLASS lcl_unit_test_values IMPLEMENTATION.
  METHOD verify_static_instances.
    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_1
                  i_expected = '1' ).

    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_2
                  i_expected = '2' ).

    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_3
                  i_expected = '3' ).

    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_4
                  i_expected = '4' ).

    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_5
                  i_expected = '5' ).

    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_6
                  i_expected = '6' ).

    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_7
                  i_expected = '7' ).

    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_8
                  i_expected = '8' ).

    assert_value( i_instance = /usi/cl_bal_enum_detail_level=>detail_level_9
                  i_expected = '9' ).
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
