CLASS /usi/cl_bal_enum_message_param DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA: log_number     TYPE REF TO /usi/cl_bal_enum_message_param READ-ONLY,
                message_number TYPE REF TO /usi/cl_bal_enum_message_param READ-ONLY.

    DATA value TYPE balpar READ-ONLY.

    "! <h1>Create static instances</h1>
    CLASS-METHODS class_constructor.

    "! <h1>Create instances</h1>
    METHODS constructor
      IMPORTING
        i_value TYPE balpar.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /usi/cl_bal_enum_message_param IMPLEMENTATION.
  METHOD class_constructor.
    log_number     = NEW #( i_value = '%LOGNUMBER' ).
    message_number = NEW #( i_value = 'MSG_NUMBER' ).
  ENDMETHOD.

  METHOD constructor.
    value = i_value.
  ENDMETHOD.
ENDCLASS.
