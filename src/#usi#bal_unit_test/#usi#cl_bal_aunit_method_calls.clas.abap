CLASS /usi/cl_bal_aunit_method_calls DEFINITION PUBLIC FINAL CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_method_call,
             method_name TYPE /usi/cl_bal_aunit_method_call=>ty_method_name,
             method_call TYPE REF TO /usi/cl_bal_aunit_method_call,
           END   OF ty_method_call,
           ty_method_calls TYPE SORTED TABLE OF ty_method_call WITH NON-UNIQUE KEY method_name.

    DATA method_calls TYPE ty_method_calls READ-ONLY.

    "! <h1>Makes the current test fail, if the method was not called n times</h1>
    "!
    "! @parameter i_method_name | Method name
    "! @parameter i_expected_number_of_calls | Expected number of calls
    METHODS assert_method_called_n_times
      IMPORTING
        i_method_name              TYPE /usi/cl_bal_aunit_method_call=>ty_method_name
        i_expected_number_of_calls TYPE int4.

    "! <h1>Makes the current test fail, if the method was not called</h1>
    "!
    "! @parameter i_method_name | Method name
    METHODS assert_method_was_called
      IMPORTING
        i_method_name TYPE /usi/cl_bal_aunit_method_call=>ty_method_name.

    "! <h1>Makes the current test fail, if the method has been called</h1>
    "!
    "! @parameter i_method_name | Method name
    METHODS assert_method_was_not_called
      IMPORTING
        i_method_name TYPE /usi/cl_bal_aunit_method_call=>ty_method_name.

    "! <h1>Returns calls of given method</h1>
    "!
    "! <p>Can be used to analyze indirect output by checking the data passed to the methods.</p>
    "!
    "! @parameter i_method_name | Method name
    "! @parameter r_result | Method calls (with parameters)
    METHODS get_method_calls
      IMPORTING
        i_method_name   TYPE /usi/cl_bal_aunit_method_call=>ty_method_name
      RETURNING
        VALUE(r_result) TYPE ty_method_calls.

    "! <h1>Insert method call</h1>
    "!
    "! <p>Can be used by test-doubles / spy-objects to log received method calls.</p>
    "!
    "! @parameter i_method_name | Method name
    "! @parameter r_result | Method call instance (Accepts parameters)
    METHODS insert_method_call
      IMPORTING
        i_method_name   TYPE /usi/cl_bal_aunit_method_call=>ty_method_name
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_aunit_method_call.

    "! <h1>Reset data</h1>
    METHODS reset_method_calls.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS was_method_called
      IMPORTING
        i_method_name   TYPE /usi/cl_bal_aunit_method_call=>ty_method_name
      RETURNING
        VALUE(r_result) TYPE abap_bool.

ENDCLASS.



CLASS /usi/cl_bal_aunit_method_calls IMPLEMENTATION.
  METHOD assert_method_called_n_times.
    DATA actual_number_of_calls TYPE int4.

    LOOP AT method_calls TRANSPORTING NO FIELDS WHERE method_name EQ i_method_name.
      actual_number_of_calls = actual_number_of_calls + 1.
    ENDLOOP.

    cl_aunit_assert=>assert_equals( exp = i_expected_number_of_calls
                                    act = actual_number_of_calls
                                    msg = `Unexpected number of method calls!` ).
  ENDMETHOD.


  METHOD assert_method_was_called.
    IF was_method_called( i_method_name ) EQ abap_false.
      cl_aunit_assert=>fail( msg    = `Method was not called!`
                             detail = i_method_name ).
    ENDIF.
  ENDMETHOD.


  METHOD assert_method_was_not_called.
    IF was_method_called( i_method_name ) EQ abap_true.
      cl_aunit_assert=>fail( msg    = `Method was called!`
                             detail = i_method_name ).
    ENDIF.
  ENDMETHOD.


  METHOD get_method_calls.
    FIELD-SYMBOLS <method_call> TYPE ty_method_call.

    LOOP AT method_calls ASSIGNING <method_call> WHERE method_name EQ i_method_name.
      INSERT <method_call> INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD insert_method_call.
    DATA method_call TYPE ty_method_call.

    CREATE OBJECT r_result
      EXPORTING
        i_method_name = i_method_name.

    method_call-method_name = r_result->method_name.
    method_call-method_call = r_result.
    INSERT method_call INTO TABLE method_calls.
  ENDMETHOD.


  METHOD reset_method_calls.
    CLEAR method_calls.
  ENDMETHOD.


  METHOD was_method_called.
    READ TABLE method_calls
      TRANSPORTING NO FIELDS
      WITH KEY method_name = i_method_name.
    IF sy-subrc EQ 0.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
