*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Unit test: Serialization
*--------------------------------------------------------------------*
CLASS lcl_unit_tests_serialization DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_html DEFINITION LOCAL FRIENDS lcl_unit_tests_serialization.

CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS test_deserialize_bad_xml   FOR TESTING.
    METHODS test_deserialize_valid_xml FOR TESTING.
ENDCLASS.

CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_xml.
    DATA cut TYPE REF TO /usi/cl_bal_dc_html.

    TRY.
        cut ?= /usi/cl_bal_dc_html=>/usi/if_bal_data_container~deserialize( `Garbage input - should fail.` ).
        cl_aunit_assert=>fail( 'Input was garbage! Exception expected!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_deserialize_valid_xml.
    DATA: cut                       TYPE REF TO /usi/cl_bal_dc_html,
          document                  TYPE string,
          title                     TYPE REF TO /usi/if_bal_text_container_c40,
          serialized_data_container TYPE /usi/bal_xml_string,
          serialized_title_in       TYPE /usi/bal_xml_string,
          serialized_title_out      TYPE /usi/bal_xml_string,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    " serialize
    document = `<html><head/><body><p>test document</p></body></html>`.

    CREATE OBJECT title TYPE /usi/cl_bal_tc_literal_c40
      EXPORTING
        i_text = 'Test document title'.

    CREATE OBJECT cut
      EXPORTING
        i_html_document  = document
        i_document_title = title.
    TRY.
        serialized_data_container = cut->/usi/if_bal_data_container~serialize( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
    CLEAR cut.

    " deserialize
    TRY.
        cut ?= /usi/cl_bal_dc_html=>/usi/if_bal_data_container~deserialize( serialized_data_container ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    " compare
    cl_aunit_assert=>assert_equals( act = cut->html_document
                                    exp = document ).

    serialized_title_in  = title->serialize( ).
    serialized_title_out = cut->document_title->serialize( ).
    cl_aunit_assert=>assert_equals( act = serialized_title_out
                                    exp = serialized_title_in ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Cardinality
*--------------------------------------------------------------------*
CLASS lcl_unit_test_cardinality DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS assert_is_multi_use FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_cardinality IMPLEMENTATION.
  METHOD assert_is_multi_use.
    DATA actual_result TYPE abap_bool.

    actual_result = /usi/cl_bal_dc_html=>/usi/if_bal_data_container~is_multiple_use_allowed( ).
    cl_aunit_assert=>assert_equals( act = actual_result
                                    exp = abap_true ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Classname
*--------------------------------------------------------------------*
CLASS lcl_unit_test_classname DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS assert_returns_right_classname FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_classname IMPLEMENTATION.
  METHOD assert_returns_right_classname.
    DATA: cut_description TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl,
          actual_result   TYPE classname.

    actual_result   = /usi/cl_bal_dc_html=>/usi/if_bal_data_container~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
