*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Unit test: Serialization
" ---------------------------------------------------------------------
CLASS lcl_unit_tests_serialization DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_callstack DEFINITION LOCAL FRIENDS lcl_unit_tests_serialization.

CLASS lcl_unit_tests_serialization DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_deserialize_bad_xml   FOR TESTING.
    METHODS test_deserialize_valid_xml FOR TESTING.

    METHODS get_callstack
      RETURNING
        VALUE(r_result) TYPE abap_callstack.
ENDCLASS.


CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_xml.
    TRY.
        CAST /usi/cl_bal_dc_callstack( /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~deserialize( `Garbage` ) ).
        cl_abap_unit_assert=>fail( `Input was garbage! Exception expected!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_deserialize_valid_xml.
    " serialize
    DATA(input) = get_callstack( ).
    TRY.
        DATA(serialized_data_container) = NEW /usi/cl_bal_dc_callstack( input )->/usi/if_bal_data_container~serialize( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    " deserialize
    TRY.
        DATA(cut) = CAST /usi/cl_bal_dc_callstack( /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~deserialize(
                                                       serialized_data_container ) ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    " compare
    cl_abap_unit_assert=>assert_equals( exp = input
                                        act = cut->callstack ).
  ENDMETHOD.

  METHOD get_callstack.
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING max_level = 0
      IMPORTING callstack = r_result.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
CLASS lcl_unit_test_cardinality DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS assert_is_single_use FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_cardinality IMPLEMENTATION.
  METHOD assert_is_single_use.
    DATA(actual_result) = /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~is_multiple_use_allowed( ).
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
    DATA(actual_result)   = /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( ).
    DATA(cut_description) = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
