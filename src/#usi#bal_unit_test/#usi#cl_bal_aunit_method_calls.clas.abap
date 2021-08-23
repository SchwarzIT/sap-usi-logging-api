class /USI/CL_BAL_AUNIT_METHOD_CALLS definition
  public
  final
  create public
  for testing .

public section.
  class /USI/CL_BAL_AUNIT_METHOD_CALL definition load .

  types:
    BEGIN OF ty_method_call,
             method_name TYPE /usi/cl_bal_aunit_method_call=>ty_method_name,
             method_call TYPE REF TO /usi/cl_bal_aunit_method_call,
           END   OF ty_method_call .
  types:
    ty_method_calls TYPE SORTED TABLE OF ty_method_call WITH NON-UNIQUE KEY method_name .

  data METHOD_CALLS type TY_METHOD_CALLS read-only .

  methods ASSERT_METHOD_CALLED_N_TIMES
    importing
      !I_METHOD_NAME type /USI/CL_BAL_AUNIT_METHOD_CALL=>TY_METHOD_NAME
      !I_EXPECTED_NUMBER_OF_CALLS type INT4 .
  methods ASSERT_METHOD_WAS_CALLED
    importing
      !I_METHOD_NAME type /USI/CL_BAL_AUNIT_METHOD_CALL=>TY_METHOD_NAME .
  methods ASSERT_METHOD_WAS_NOT_CALLED
    importing
      !I_METHOD_NAME type /USI/CL_BAL_AUNIT_METHOD_CALL=>TY_METHOD_NAME .
  methods GET_METHOD_CALLS
    importing
      !I_METHOD_NAME type /USI/CL_BAL_AUNIT_METHOD_CALL=>TY_METHOD_NAME
    returning
      value(R_RESULT) type TY_METHOD_CALLS .
  methods INSERT_METHOD_CALL
    importing
      !I_METHOD_NAME type /USI/CL_BAL_AUNIT_METHOD_CALL=>TY_METHOD_NAME
    returning
      value(R_RESULT) type ref to /USI/CL_BAL_AUNIT_METHOD_CALL .
  methods RESET_METHOD_CALLS .
  PROTECTED SECTION.
private section.

  methods WAS_METHOD_CALLED
    importing
      !I_METHOD_NAME type /USI/CL_BAL_AUNIT_METHOD_CALL=>TY_METHOD_NAME
    returning
      value(R_RESULT) type ABAP_BOOL .
ENDCLASS.



CLASS /USI/CL_BAL_AUNIT_METHOD_CALLS IMPLEMENTATION.


  METHOD assert_method_called_n_times.
    DATA actual_number_of_calls TYPE int4.

    LOOP AT method_calls TRANSPORTING NO FIELDS WHERE method_name EQ i_method_name.
      ADD 1 TO actual_number_of_calls.
    ENDLOOP.

    cl_aunit_assert=>assert_equals(
      exp = i_expected_number_of_calls
      act = actual_number_of_calls
      msg = `Unexpected number of method calls!`
    ).
  ENDMETHOD.


  METHOD assert_method_was_called.
    IF was_method_called( i_method_name ) EQ abap_false.
      cl_aunit_assert=>fail(
        msg    = `Method was not called!`
        detail = i_method_name
      ).
    ENDIF.
  ENDMETHOD.


  METHOD assert_method_was_not_called.
    IF was_method_called( i_method_name ) EQ abap_true.
      cl_aunit_assert=>fail(
        msg    = `Method was called!`
        detail = i_method_name
      ).
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
