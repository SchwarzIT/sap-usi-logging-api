CLASS /usi/cl_bal_enum_message_param DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-DATA log_number TYPE REF TO /usi/cl_bal_enum_message_param READ-ONLY.
    CLASS-DATA message_number TYPE REF TO /usi/cl_bal_enum_message_param READ-ONLY.

    DATA value TYPE balpar READ-ONLY.

    CLASS-METHODS class_constructor .

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor
      IMPORTING
        !i_value TYPE balpar.

ENDCLASS.



CLASS /usi/cl_bal_enum_message_param IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT log_number
      EXPORTING
        i_value = '%LOGNUMBER'.

    CREATE OBJECT message_number
      EXPORTING
        i_value = 'MSG_NUMBER'.
  ENDMETHOD.

  METHOD constructor.
    value = i_value.
  ENDMETHOD.
ENDCLASS.
