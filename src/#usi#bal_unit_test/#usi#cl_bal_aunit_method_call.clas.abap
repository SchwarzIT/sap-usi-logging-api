CLASS /usi/cl_bal_aunit_method_call DEFINITION PUBLIC FINAL CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    TYPES: ty_method_name    TYPE abap_methname,
           ty_parameter_name TYPE abap_parmname.

    TYPES: BEGIN OF ty_parameter,
             parameter_name  TYPE ty_parameter_name,
             parameter_value TYPE REF TO data,
           END   OF ty_parameter,
           ty_parameters TYPE SORTED TABLE OF ty_parameter WITH UNIQUE KEY parameter_name.

    DATA: method_name TYPE ty_method_name READ-ONLY,
          parameters  TYPE ty_parameters  READ-ONLY.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_method_name | Method name
    METHODS constructor
      IMPORTING i_method_name TYPE ty_method_name.

    "! <h1>Add parameter</h1>
    "!
    "! @parameter i_parameter_name  | Name
    "! @parameter i_parameter_value | Value
    METHODS add_parameter
      IMPORTING i_parameter_name  TYPE ty_parameter_name
                i_parameter_value TYPE any.

    "! <h1>Get parameter</h1>
    "!
    "! @parameter i_parameter_name  | Name
    "! @parameter e_parameter_value | Value
    METHODS get_parameter
      IMPORTING i_parameter_name         TYPE ty_parameter_name
      EXPORTING VALUE(e_parameter_value) TYPE any.

ENDCLASS.


CLASS /usi/cl_bal_aunit_method_call IMPLEMENTATION.
  METHOD constructor.
    method_name = i_method_name.
  ENDMETHOD.

  METHOD add_parameter.
    DATA parameter TYPE ty_parameter.
    FIELD-SYMBOLS <parameter_value> TYPE any.

    parameter-parameter_name = i_parameter_name.

    CREATE DATA parameter-parameter_value LIKE i_parameter_value.
    ASSIGN parameter-parameter_value->* TO <parameter_value>.
    <parameter_value> = i_parameter_value.

    INSERT parameter INTO TABLE parameters.
    IF sy-subrc <> 0.
      cl_aunit_assert=>fail( `Duplicate call! Test is broken!` ).
    ENDIF.
  ENDMETHOD.

  METHOD get_parameter.
    FIELD-SYMBOLS: <parameter>       TYPE ty_parameter,
                   <parameter_value> TYPE any.

    CLEAR e_parameter_value.

    ASSIGN parameters[ parameter_name = i_parameter_name ] TO <parameter>.
    IF sy-subrc <> 0.
      cl_aunit_assert=>fail( msg    = `Parameter not found!`
                             detail = i_parameter_name ).
    ENDIF.

    ASSIGN <parameter>-parameter_value->* TO <parameter_value>.
    e_parameter_value = <parameter_value>.
  ENDMETHOD.
ENDCLASS.
