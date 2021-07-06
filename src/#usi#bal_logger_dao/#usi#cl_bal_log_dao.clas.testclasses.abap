*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_test_public_attribs DEFINITION FINAL FOR TESTING CREATE PUBLIC.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS assert_public_is_read_only FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_public_attribs IMPLEMENTATION.
  METHOD assert_public_is_read_only.
    DATA cut_description TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl.
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_public_attrib_read_only( ).
  ENDMETHOD.
ENDCLASS.
