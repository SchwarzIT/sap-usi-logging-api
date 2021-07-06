CLASS /usi/cl_bal_enum_message_type DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-DATA abend TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY .
    CLASS-DATA error TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY .
    CLASS-DATA exit TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY .
    CLASS-DATA information TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY .
    CLASS-DATA success TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY .
    CLASS-DATA warning TYPE REF TO /usi/cl_bal_enum_message_type READ-ONLY .
    DATA value TYPE symsgty READ-ONLY .

    CLASS-METHODS class_constructor .

    METHODS constructor
      IMPORTING
        !i_msgty TYPE symsgty .

    CLASS-METHODS get_by_value
      IMPORTING
        !i_value        TYPE symsgty
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_message_type
      RAISING
        /usi/cx_bal_root .

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance_map_entry,
        value    TYPE symsgty,
        instance TYPE REF TO /usi/cl_bal_enum_message_type,
      END   OF ty_instance_map_entry .
    TYPES:
      ty_instance_map_table TYPE HASHED TABLE OF ty_instance_map_entry WITH UNIQUE KEY value.

    CLASS-DATA instance_map_table TYPE ty_instance_map_table.
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
    DATA instance_map_entry TYPE ty_instance_map_entry.

    value = i_msgty.

    instance_map_entry-value    = me->value.
    instance_map_entry-instance = me.
    INSERT instance_map_entry INTO TABLE instance_map_table.
  ENDMETHOD.


  METHOD get_by_value.
    FIELD-SYMBOLS <instance_map_entry> TYPE ty_instance_map_entry.

    READ TABLE  instance_map_table
      ASSIGNING <instance_map_entry>
      WITH TABLE KEY value = i_value.

    IF sy-subrc EQ 0.
      r_result = <instance_map_entry>-instance.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING
          textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
