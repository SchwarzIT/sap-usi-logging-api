*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Test all public attributes are read-only
*--------------------------------------------------------------------*
CLASS lcl_unit_test_read_only DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS assert_public_attrib_read_only FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_read_only IMPLEMENTATION.
  METHOD assert_public_attrib_read_only.
    DATA cut_description TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl.
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_public_attrib_read_only( ).
  ENDMETHOD.
ENDCLASS.
