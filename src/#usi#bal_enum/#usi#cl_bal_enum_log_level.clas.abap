CLASS /usi/cl_bal_enum_log_level DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-DATA additional_info TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY .
    CLASS-DATA everything TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY .
    CLASS-DATA important TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY .
    CLASS-DATA medium TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY .
    CLASS-DATA nothing TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY .
    CLASS-DATA other TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY .
    CLASS-DATA very_important TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY .
    DATA value TYPE /usi/bal_log_level READ-ONLY .

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !i_value TYPE /usi/bal_log_level .
    CLASS-METHODS get_by_value
      IMPORTING
        !i_value        TYPE /usi/bal_log_level
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level
      RAISING
        /usi/cx_bal_root .
    METHODS is_higher_than
      IMPORTING
        !i_log_level    TYPE REF TO /usi/cl_bal_enum_log_level
      RETURNING
        VALUE(r_result) TYPE abap_bool .
    METHODS is_problem_class_relevant
      IMPORTING
        !i_problem_class TYPE REF TO /usi/cl_bal_enum_problem_class
      RETURNING
        VALUE(r_result)  TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_instance_map_entry,
        value    TYPE /usi/bal_log_level,
        instance TYPE REF TO /usi/cl_bal_enum_log_level,
      END   OF ty_instance_map_entry .
    TYPES:
      ty_instance_map_table TYPE HASHED TABLE OF ty_instance_map_entry WITH UNIQUE KEY value .

    CLASS-DATA instance_map_table TYPE ty_instance_map_table .

    METHODS get_min_loglevel_for_probclass
      IMPORTING
        !i_problem_class TYPE REF TO /usi/cl_bal_enum_problem_class
      RETURNING
        VALUE(r_result)  TYPE REF TO /usi/cl_bal_enum_log_level.
ENDCLASS.



CLASS /USI/CL_BAL_ENUM_LOG_LEVEL IMPLEMENTATION.


  METHOD class_constructor.
    CREATE OBJECT nothing
      EXPORTING
        i_value = 0.

    CREATE OBJECT very_important
      EXPORTING
        i_value = 1.

    CREATE OBJECT important
      EXPORTING
        i_value = 2.

    CREATE OBJECT medium
      EXPORTING
        i_value = 3.

    CREATE OBJECT additional_info
      EXPORTING
        i_value = 4.

    CREATE OBJECT other
      EXPORTING
        i_value = 5.

    CREATE OBJECT everything
      EXPORTING
        i_value = 6.
  ENDMETHOD.


  METHOD constructor.
    DATA instance_map_entry TYPE ty_instance_map_entry.

    value = i_value.

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


  METHOD get_min_loglevel_for_probclass.
    CASE i_problem_class.
      WHEN /usi/cl_bal_enum_problem_class=>very_important.
        r_result = very_important.
      WHEN /usi/cl_bal_enum_problem_class=>important.
        r_result = important.
      WHEN /usi/cl_bal_enum_problem_class=>medium.
        r_result = medium.
      WHEN /usi/cl_bal_enum_problem_class=>additional_information.
        r_result = additional_info.
      WHEN OTHERS.
        r_result = other.
    ENDCASE.
  ENDMETHOD.


  METHOD is_higher_than.
    IF i_log_level        IS BOUND AND
       i_log_level->value LT value.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_problem_class_relevant.
    DATA minimum_log_level TYPE REF TO /usi/cl_bal_enum_log_level.

    minimum_log_level = get_min_loglevel_for_probclass( i_problem_class ).

    IF minimum_log_level->is_higher_than( me ) EQ abap_false.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
