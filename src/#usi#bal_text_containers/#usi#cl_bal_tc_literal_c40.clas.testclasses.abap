*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Unit test: Classname
*--------------------------------------------------------------------*
CLASS lcl_unit_test_classname DEFINITION FINAL CREATE PUBLIC FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS assert_returns_right_classname FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_classname IMPLEMENTATION.
  METHOD assert_returns_right_classname.
    DATA: cut_description TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl,
          actual_result   TYPE classname.

    actual_result   = /usi/cl_bal_tc_literal_c40=>/usi/if_bal_text_container_c40~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
