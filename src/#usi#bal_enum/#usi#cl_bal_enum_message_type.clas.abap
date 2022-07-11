CLASS /usi/cl_bal_enum_message_type DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA: abend       TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY,
                error       TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY,
                exit        TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY,
                information TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY,
                success     TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY,
                warning     TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY.

    DATA value TYPE symsgty READ-ONLY.

    "! <h1>Create static instances</h1>
    CLASS-METHODS class_constructor.

    "! <h1>Get enumeration-instance by value</h1>
    "!
    "! <p>In some cases the value of an enum instance is known, but for an API call the instance itself is needed.</p>
    "!
    "! <p>This method provides a backwards search and returns the matching instance for the passed value.</p>
    "!
    "! @parameter i_value | The value to search for
    "! @parameter r_result | The corresponding instance
    "! @raising /usi/cx_bal_root | Unsupported value
    CLASS-METHODS get_by_value
      IMPORTING
        i_value         TYPE symsgty
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_message_type
      RAISING
        /usi/cx_bal_root.

    "! <h1>Create instances</h1>
    METHODS constructor
      IMPORTING
        i_msgty TYPE symsgty.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_buffered_instance,
             value    TYPE symsgty,
             instance TYPE REF TO /usi/cl_bal_enum_message_type,
           END   OF ty_buffered_instance,
           ty_buffered_instances TYPE HASHED TABLE OF ty_buffered_instance WITH UNIQUE KEY value.

    CLASS-DATA buffered_instances TYPE ty_buffered_instances.

ENDCLASS.



CLASS /usi/cl_bal_enum_message_type IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT abend
      EXPORTING
        i_msgty = 'A'.

    CREATE OBJECT error
      EXPORTING
        i_msgty = 'E'.

    CREATE OBJECT exit
      EXPORTING
        i_msgty = 'X'.

    CREATE OBJECT information
      EXPORTING
        i_msgty = 'I'.

    CREATE OBJECT success
      EXPORTING
        i_msgty = 'S'.

    CREATE OBJECT warning
      EXPORTING
        i_msgty = 'W'.
  ENDMETHOD.


  METHOD constructor.
    DATA buffered_instance TYPE ty_buffered_instance.

    value = i_msgty.

    buffered_instance-value    = me->value.
    buffered_instance-instance = me.
    INSERT buffered_instance INTO TABLE buffered_instances.
  ENDMETHOD.


  METHOD get_by_value.
    FIELD-SYMBOLS <buffered_instance> TYPE ty_buffered_instance.

    READ TABLE buffered_instances
      ASSIGNING <buffered_instance>
      WITH TABLE KEY value = i_value.

    IF sy-subrc EQ 0.
      r_result = <buffered_instance>-instance.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING
          textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
