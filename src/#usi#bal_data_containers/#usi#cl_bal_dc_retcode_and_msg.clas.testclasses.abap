*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Unit test: Serialization
" ---------------------------------------------------------------------
CLASS lcl_unit_tests_serialization DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_retcode_and_msg DEFINITION LOCAL FRIENDS lcl_unit_tests_serialization.

CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_deserialize_bad_xml   FOR TESTING.
    METHODS test_deserialize_valid_xml FOR TESTING RAISING /usi/cx_bal_root.
ENDCLASS.


CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_xml.
    TRY.
        CAST /usi/cl_bal_dc_retcode_and_msg( /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~deserialize(
                                                 `Invalid garbage` ) ).
        cl_abap_unit_assert=>fail( msg = 'Input was garbage! Exception expected!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_deserialize_valid_xml.
    DATA: BEGIN OF input,
            message     TYPE symsg,
            return_code TYPE sysubrc,
          END   OF input.

    input = VALUE #( return_code = 4
                     message     = VALUE #( msgty = 'E'
                                            msgid = '38'
                                            msgno = '000'
                                            msgv1 = 'Just a test' ) ).

    " Serialize / Deserialize
    DATA(serialized_data_container) = NEW /usi/cl_bal_dc_retcode_and_msg(
        i_message     = input-message
        i_return_code = input-return_code )->/usi/if_bal_data_container~serialize( ).
    DATA(cut) = CAST /usi/cl_bal_dc_retcode_and_msg(
                        /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~deserialize(
                            serialized_data_container ) ).

    " Compare
    cl_abap_unit_assert=>assert_equals( exp = input-message
                                        act = cut->message ).
    cl_abap_unit_assert=>assert_equals( exp = input-return_code
                                        act = cut->return_code ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
CLASS lcl_unit_test_cardinality DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS assert_is_single_use FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_cardinality IMPLEMENTATION.
  METHOD assert_is_single_use.
    DATA(actual_result) = /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~is_multiple_use_allowed( ).
    cl_abap_unit_assert=>assert_false( actual_result ).
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

    actual_result   = /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
