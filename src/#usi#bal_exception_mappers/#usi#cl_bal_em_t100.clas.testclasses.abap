*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_test DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.

    METHODS verify_refuse_bad_exception FOR TESTING.
    METHODS verify_accept_good_exception FOR TESTING.
    METHODS verify_good_exception_output FOR TESTING.

    METHODS get_compatible_exception
      IMPORTING
        i_message       TYPE symsg OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cx_bal_root.

    METHODS get_incompatible_exception
      RETURNING
        VALUE(r_result) TYPE REF TO cx_root.

ENDCLASS.

CLASS lcl_unit_test IMPLEMENTATION.

  METHOD verify_refuse_bad_exception.
    DATA: cut        TYPE REF TO /usi/cl_bal_em_t100,
          test_input TYPE REF TO cx_root.

    test_input ?= get_incompatible_exception( ).
    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_exception = test_input.

        cl_aunit_assert=>fail( `Exceptions not implementing IF_T100_MESSAGE must not be accepted!` ).
      CATCH /usi/cx_bal_root.
        " Expected result - this is exactly, what we wanted!
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD verify_accept_good_exception.
    DATA: cut        TYPE REF TO /usi/cl_bal_em_t100,
          test_input TYPE REF TO cx_root.

    test_input ?= get_compatible_exception( ).
    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_exception = test_input.
      CATCH /usi/cx_bal_root.
        cl_aunit_assert=>fail( `Exceptions implementing IF_T100_MESSAGE should be accepted!` ).
    ENDTRY.
  ENDMETHOD.

  METHOD verify_good_exception_output.
    DATA: cut                  TYPE REF TO /usi/cl_bal_em_t100,
          input                TYPE symsg,
          exception            TYPE REF TO cx_root,
          output               TYPE symsg,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    input-msgty = 'E'.
    input-msgid = '38'.
    input-msgno = '000'.
    input-msgv1 = sy-abcde.
    input-msgv2 = sy-datum.
    input-msgv3 = sy-uzeit.
    input-msgv4 = sy-uname.
    exception  ?= get_compatible_exception( input ).

    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_exception = exception.
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    output = cut->/usi/if_bal_exception_mapper~get_t100_message( ).
    cl_aunit_assert=>assert_equals(
      act = input
      exp = output
    ).
  ENDMETHOD.

  METHOD get_incompatible_exception.
    DATA number TYPE int4.

    TRY.
        number = 'Not a number'.
      CATCH cx_sy_conversion_no_number INTO r_result.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD get_compatible_exception.
    DATA textid TYPE scx_t100key.

    IF i_message IS NOT INITIAL.
      textid-msgid = i_message-msgid.
      textid-msgno = i_message-msgno.
      textid-attr1 = 'PARAM1'.
      textid-attr2 = 'PARAM2'.
      textid-attr3 = 'PARAM3'.
      textid-attr4 = 'PARAM4'.
    ENDIF.

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found
          EXPORTING
            textid = textid
            param1 = i_message-msgv1
            param2 = i_message-msgv2
            param3 = i_message-msgv3
            param4 = i_message-msgv4.
      CATCH /usi/cx_bal_not_found INTO r_result.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
