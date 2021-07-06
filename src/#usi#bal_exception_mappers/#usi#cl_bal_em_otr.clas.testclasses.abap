*"* use this source file for your ABAP unit test classes
CLASS lcx_test DEFINITION FINAL INHERITING FROM cx_static_check FOR TESTING.
  PUBLIC SECTION.
    DATA text TYPE char200 READ-ONLY.

    METHODS constructor
      IMPORTING
        i_text TYPE char200.

    METHODS if_message~get_text REDEFINITION.
ENDCLASS.

CLASS lcx_test IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    text = i_text.
  ENDMETHOD.

  METHOD if_message~get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_unit_test DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.

    METHODS verify_t100_messages FOR TESTING.

    METHODS verify_t100_message
      IMPORTING
        i_text_length TYPE int4.

    METHODS get_exception
      IMPORTING
        i_text          TYPE char200
      RETURNING
        VALUE(r_result) TYPE REF TO lcx_test.

    METHODS get_message_text
      IMPORTING
        i_text_length   TYPE int4
      RETURNING
        VALUE(r_result) TYPE char200.
ENDCLASS.

CLASS lcl_unit_test IMPLEMENTATION.
  METHOD verify_t100_messages.
    verify_t100_message( 50 ).
    verify_t100_message( 51 ).
    verify_t100_message( 200 ).
  ENDMETHOD.

  METHOD verify_t100_message.
    DATA: cut       TYPE REF TO /usi/cl_bal_em_otr,
          text_in   TYPE char200,
          exception TYPE REF TO lcx_test,
          message   TYPE symsg,
          text_out  TYPE char200.

    text_in   = get_message_text( i_text_length ).
    exception = get_exception( text_in ).

    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_exception = exception.
      CATCH /usi/cx_bal_root.
        cl_aunit_assert=>fail( ).
    ENDTRY.

    message = cut->/usi/if_bal_exception_mapper~get_t100_message( ).
    MESSAGE ID     message-msgid
            TYPE   'S'
            NUMBER message-msgno
            WITH   message-msgv1
                   message-msgv2
                   message-msgv3
                   message-msgv4
            INTO   text_out.

    cl_aunit_assert=>assert_equals(
      act = text_out
      exp = text_in
    ).
  ENDMETHOD.

  METHOD get_exception.
    TRY.
        RAISE EXCEPTION TYPE lcx_test
          EXPORTING
            i_text = i_text.
      CATCH lcx_test INTO r_result.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD get_message_text.
    DATA needed_chars TYPE int4.

    needed_chars = i_text_length.

    WHILE needed_chars IS NOT INITIAL.
      IF needed_chars LE 26.
        CONCATENATE r_result
                    sy-abcde+0(needed_chars)
               INTO r_result
               IN CHARACTER MODE.
        CLEAR needed_chars.
      ELSE.
        CONCATENATE r_result
                    sy-abcde
               INTO r_result
               IN CHARACTER MODE.
        SUBTRACT 26 FROM needed_chars.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
