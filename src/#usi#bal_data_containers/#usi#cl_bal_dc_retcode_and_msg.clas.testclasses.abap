*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Unit test: Serialization
*--------------------------------------------------------------------*
CLASS lcl_unit_tests_serialization DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_retcode_and_msg DEFINITION LOCAL FRIENDS lcl_unit_tests_serialization.

CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS test_deserialize_bad_xml   FOR TESTING.
    METHODS test_deserialize_valid_xml FOR TESTING.
ENDCLASS.

CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_xml.
    DATA cut TYPE REF TO /usi/cl_bal_dc_retcode_and_msg.

    TRY.
        cut ?= /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~deserialize( `Invalid garbage` ).
        cl_aunit_assert=>fail( msg = 'Input was garbage! Exception expected!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_deserialize_valid_xml.
    DATA: cut TYPE REF TO /usi/cl_bal_dc_retcode_and_msg,
          BEGIN OF input,
            message     TYPE symsg,
            return_code TYPE sysubrc,
          END   OF input,
          serialized_data_container TYPE /usi/bal_xml_string,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    input-return_code   = 4.
    input-message-msgty = 'E'.
    input-message-msgid = '38'.
    input-message-msgno = '000'.
    input-message-msgv1 = 'Just a test'.

    CREATE OBJECT cut
      EXPORTING
        i_message     = input-message
        i_return_code = input-return_code.
    TRY.
        serialized_data_container = cut->/usi/if_bal_data_container~serialize( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
    CLEAR cut.

    TRY.
        cut ?= /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~deserialize( serialized_data_container ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = cut->message
                                    exp = input-message ).
    cl_aunit_assert=>assert_equals( act = cut->return_code
                                    exp = input-return_code ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Cardinality
*--------------------------------------------------------------------*
CLASS lcl_unit_test_cardinality DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS assert_is_single_use FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_cardinality IMPLEMENTATION.
  METHOD assert_is_single_use.
    DATA actual_result TYPE abap_bool.

    actual_result = /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~is_multiple_use_allowed( ).
    cl_aunit_assert=>assert_equals( exp = abap_false
                                    act = actual_result ).
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

    actual_result   = /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
