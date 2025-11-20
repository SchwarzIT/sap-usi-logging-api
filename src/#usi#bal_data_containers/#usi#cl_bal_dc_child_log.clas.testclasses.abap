*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Helper class for serialization tests
" ---------------------------------------------------------------------
CLASS lcl_private_data_getter DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_child_log DEFINITION LOCAL FRIENDS lcl_private_data_getter.

CLASS lcl_private_data_getter DEFINITION FINAL FOR TESTING CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_data_container TYPE REF TO /usi/cl_bal_dc_child_log.

    METHODS get_log_handle
      RETURNING VALUE(r_result) TYPE balognr.

  PRIVATE SECTION.
    DATA data_container TYPE REF TO /usi/cl_bal_dc_child_log.

ENDCLASS.


CLASS lcl_private_data_getter IMPLEMENTATION.
  METHOD constructor.
    data_container = i_data_container.
  ENDMETHOD.

  METHOD get_log_handle.
    r_result = data_container->log_handle.
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Unit test: Serialization
" ---------------------------------------------------------------------
CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_deserialize_bad_json   FOR TESTING.
    METHODS test_deserialize_valid_json FOR TESTING RAISING /usi/cx_bal_root.
ENDCLASS.


CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_json.
    TRY.
        CAST /usi/cl_bal_dc_child_log( /usi/cl_bal_dc_child_log=>/usi/if_bal_data_container~deserialize(
                                                `Invalid garbage` ) ).
        cl_abap_unit_assert=>fail( 'Input was garbage! Exception expected!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_deserialize_valid_json.
    DATA(input) = CONV balloghndl( '123456' ).

    " serialize / deserialize
    DATA(serialized_data_container) = NEW /usi/cl_bal_dc_child_log( input )->/usi/if_bal_data_container~serialize( ).

    DATA(cut) = CAST /usi/cl_bal_dc_child_log(
                       /usi/cl_bal_dc_child_log=>/usi/if_bal_data_container~deserialize( serialized_data_container ) ).

    " compare
    cl_abap_unit_assert=>assert_equals( exp = input
                                        act = NEW lcl_private_data_getter( cut )->get_log_handle( ) ).
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
    DATA(actual_result) = /usi/cl_bal_dc_child_log=>/usi/if_bal_data_container~is_multiple_use_allowed( ).
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

    actual_result   = /usi/cl_bal_dc_child_log=>/usi/if_bal_data_container~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
