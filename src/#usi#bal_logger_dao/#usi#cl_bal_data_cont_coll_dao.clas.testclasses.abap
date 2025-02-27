*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Unit test: XML <-> DB conversion
" ---------------------------------------------------------------------
CLASS lcl_unit_tests_db_conversion DEFINITION DEFERRED.
CLASS /usi/cl_bal_data_cont_coll_dao DEFINITION LOCAL FRIENDS lcl_unit_tests_db_conversion.

CLASS lcl_unit_tests_db_conversion DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_conversion       FOR TESTING.
    METHODS test_empty_xml_string FOR TESTING.

    METHODS get_xml_test_string
      RETURNING VALUE(r_result) TYPE /usi/bal_xml_string.
ENDCLASS.


CLASS lcl_unit_tests_db_conversion IMPLEMENTATION.
  METHOD test_conversion.
    DATA: actual_result   TYPE /usi/bal_xml_string,
          cut             TYPE REF TO /usi/cl_bal_data_cont_coll_dao,
          db_records      TYPE /usi/cl_bal_data_cont_coll_dao=>ty_db_records,
          expected_result TYPE /usi/bal_xml_string.

    expected_result = get_xml_test_string( ).

    cut = NEW #( ).
    db_records    = cut->convert_xml_to_db( i_log_number                = '123456'
                                            i_message_number            = '1'
                                            i_serialized_data_cont_coll = expected_result ).
    actual_result = cut->convert_db_to_xml( db_records ).

    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = actual_result
                                        msg = `XML2DB-Cenversion is broken!` ).
  ENDMETHOD.

  METHOD get_xml_test_string.
    DATA callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING callstack = callstack.

    CALL TRANSFORMATION id
         SOURCE callstack = callstack
         RESULT XML r_result.
  ENDMETHOD.

  METHOD test_empty_xml_string.
    DATA: cut                  TYPE REF TO /usi/cl_bal_data_cont_coll_dao,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    cut = NEW #( ).

    TRY.
        cut->/usi/if_bal_data_cont_coll_dao~insert_collection_into_buffer( i_log_number                = '1'
                                                                           i_message_number            = 1
                                                                           i_serialized_data_cont_coll = `` ).

        cl_abap_unit_assert=>assert_initial( act = cut->db_records
                                             msg = `CUT should not create entries for initial input!` ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Duplicate message
" ---------------------------------------------------------------------
CLASS lcl_unit_test_duplicate_msg DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_duplicate_message FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_duplicate_msg IMPLEMENTATION.
  METHOD test_duplicate_message.
    DATA(cut) = CAST /usi/if_bal_data_cont_coll_dao( NEW /usi/cl_bal_data_cont_coll_dao( ) ).
    TRY.
        cut->insert_collection_into_buffer( i_log_number                = '1'
                                            i_message_number            = 1
                                            i_serialized_data_cont_coll = `<test>` ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    TRY.
        cut->insert_collection_into_buffer( i_log_number                = '1'
                                            i_message_number            = 1
                                            i_serialized_data_cont_coll = `<test>` ).

        cl_abap_unit_assert=>fail( `Call should have failed (Duplicate)!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
