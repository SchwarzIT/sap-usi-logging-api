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
    METHODS test_raise_on_invalid_value FOR TESTING.
    METHODS test_get_by_value           FOR TESTING RAISING /usi/cx_bal_root.
    METHODS test_static_instances       FOR TESTING.

    METHODS assert_get_by_value_returns
      IMPORTING i_value    TYPE /usi/bal_log_level
                i_expected TYPE REF TO /usi/cl_bal_enum_sub_log_behav
      RAISING   /usi/cx_bal_root.

    METHODS assert_value
      IMPORTING i_instance TYPE REF TO /usi/cl_bal_enum_sub_log_behav
                i_expected TYPE /usi/bal_log_level.

    METHODS assert_bound
      IMPORTING i_instance TYPE REF TO /usi/cl_bal_enum_sub_log_behav.
ENDCLASS.


CLASS lcl_unit_test_behavior IMPLEMENTATION.
  METHOD test_raise_on_invalid_value.
    TRY.
        /usi/cl_bal_enum_sub_log_behav=>get_by_value( 7 ).
        cl_abap_unit_assert=>fail( msg = 'Exception should have been raised, as input was invalid!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_by_value.
    assert_get_by_value_returns( i_value    = 0
                                 i_expected = /usi/cl_bal_enum_sub_log_behav=>create_new_logger ).

    assert_get_by_value_returns( i_value    = 3
                                 i_expected = /usi/cl_bal_enum_sub_log_behav=>reuse_if_sub_object_matches ).

    assert_get_by_value_returns( i_value    = 6
                                 i_expected = /usi/cl_bal_enum_sub_log_behav=>reuse_if_log_object_matches ).

    assert_get_by_value_returns( i_value    = 9
                                 i_expected = /usi/cl_bal_enum_sub_log_behav=>reuse_existing_logger ).
  ENDMETHOD.

  METHOD assert_get_by_value_returns.
    cl_abap_unit_assert=>assert_equals( exp = i_expected
                                        act = /usi/cl_bal_enum_sub_log_behav=>get_by_value( i_value )
                                        msg = `GET_BY_VALUE( ) returns wrong instance!` ).
  ENDMETHOD.

  METHOD test_static_instances.
    assert_value( i_instance = /usi/cl_bal_enum_sub_log_behav=>create_new_logger
                  i_expected = '0' ).

    assert_value( i_instance = /usi/cl_bal_enum_sub_log_behav=>reuse_if_sub_object_matches
                  i_expected = '3' ).

    assert_value( i_instance = /usi/cl_bal_enum_sub_log_behav=>reuse_if_log_object_matches
                  i_expected = '6' ).

    assert_value( i_instance = /usi/cl_bal_enum_sub_log_behav=>reuse_existing_logger
                  i_expected = '9' ).
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
