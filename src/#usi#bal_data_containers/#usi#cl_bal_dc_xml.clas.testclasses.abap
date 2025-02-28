*"* use this source file for your ABAP unit test classes
*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Unit test: Serialization
" ---------------------------------------------------------------------
CLASS lcl_unit_tests_serialization DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_xml DEFINITION LOCAL FRIENDS lcl_unit_tests_serialization.

CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_deserialize_bad_xml   FOR TESTING.
    METHODS test_deserialize_valid_xml FOR TESTING RAISING /usi/cx_bal_root.
ENDCLASS.


CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_xml.
    TRY.
        CAST /usi/cl_bal_dc_xml( /usi/cl_bal_dc_xml=>/usi/if_bal_data_container~deserialize(
                                     `Garbage input - should fail.` ) ).
        cl_abap_unit_assert=>fail( 'Input was garbage! Exception expected!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_deserialize_valid_xml.
    DATA(document) = `<html><head/><body><p>test document</p></body></html>`.
    DATA(title) = CAST /usi/if_bal_text_container_c40( NEW /usi/cl_bal_tc_literal_c40( 'Test document title' ) ).

    " serialize / deserialize
    DATA(serialized_data_container) = NEW /usi/cl_bal_dc_xml(
                                              i_xml_document   = document
                                              i_document_title = title )->/usi/if_bal_data_container~serialize( ).
    DATA(cut) = CAST /usi/cl_bal_dc_xml( /usi/cl_bal_dc_xml=>/usi/if_bal_data_container~deserialize(
                                             serialized_data_container ) ).

    " compare
    cl_abap_unit_assert=>assert_equals( exp = document
                                        act = cut->xml_document ).
    cl_abap_unit_assert=>assert_equals( exp = title->serialize( )
                                        act = cut->document_title->serialize( ) ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
CLASS lcl_unit_test_cardinality DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS assert_is_multi_use FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_cardinality IMPLEMENTATION.
  METHOD assert_is_multi_use.
    DATA actual_result TYPE abap_bool.

    actual_result = /usi/cl_bal_dc_xml=>/usi/if_bal_data_container~is_multiple_use_allowed( ).
    cl_abap_unit_assert=>assert_true( actual_result ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Classname
" ---------------------------------------------------------------------
CLASS lcl_unit_test_classname DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS assert_returns_right_classname FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_classname IMPLEMENTATION.
  METHOD assert_returns_right_classname.
    DATA: cut_description TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl,
          actual_result   TYPE classname.

    actual_result   = /usi/cl_bal_dc_xml=>/usi/if_bal_data_container~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
