CLASS /usi/cl_bal_enum_log_level DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA: additional_info TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY,
                everything      TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY,
                important       TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY,
                medium          TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY,
                nothing         TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY,
                other           TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY,
                very_important  TYPE REF TO /usi/cl_bal_enum_log_level READ-ONLY.

    DATA value TYPE /usi/bal_log_level READ-ONLY.

    "! <h1>Create static instances</h1>
    CLASS-METHODS class_constructor.

    "! <h1>Get enumeration-instance by value</h1>
    "!
    "! <p>In some cases the value of an enum instance is known, but for an API call the instance itself is needed.</p>
    "!
    "! <p>This method provides a backwards search and returns the matching instance for the passed value.</p>
    "!
    "! @parameter i_value          | The value to search for
    "! @parameter r_result         | The corresponding instance
    "! @raising   /usi/cx_bal_root | Unsupported value
    CLASS-METHODS get_by_value
      IMPORTING i_value         TYPE /usi/bal_log_level
      RETURNING VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level
      RAISING   /usi/cx_bal_root.

    "! <h1>Create instances</h1>
    "!
    "! @parameter i_value |
    METHODS constructor
      IMPORTING i_value TYPE /usi/bal_log_level.

    "! <h1>Log-Level comparison</h1>
    "!
    "! <p>Will return abap_true, if this instance represents a higher log-level than the one passed for comparison.</p>
    "!
    "! @parameter i_log_level | The log-level to compare with
    "! @parameter r_result    | Flag: Is this log-level higher, than i_log_level?
    METHODS is_higher_than
      IMPORTING i_log_level     TYPE REF TO /usi/cl_bal_enum_log_level
      RETURNING VALUE(r_result) TYPE abap_bool.

    "! <h1>Relevance check for problem class</h1>
    "!
    "! <p>There is a strong correlation between the log-level used by this API and the problem class used by the
    "! SAP standard logging API.</p>
    "!
    "! <p>Problem classes represent the severity of a log message. Log-levels are used to filter log messages by
    "! severity thereby controlling the detail level of the log.</p>
    "!
    "! <p>This method indicates, whether a certain problem class is relevant for the current log level.
    "! The passed message will be logged or discarded based on the result.</p>
    "!
    "! @parameter i_problem_class | Problem class of the log message
    "! @parameter r_result        | Flag: Relevant for current log-level?
    METHODS is_problem_class_relevant
      IMPORTING i_problem_class TYPE REF TO /usi/cl_bal_enum_problem_class
      RETURNING VALUE(r_result) TYPE abap_bool.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_buffered_instance,
             value    TYPE /usi/bal_log_level,
             instance TYPE REF TO /usi/cl_bal_enum_log_level,
           END   OF ty_buffered_instance,
           ty_buffered_instances TYPE HASHED TABLE OF ty_buffered_instance WITH UNIQUE KEY value.

    CLASS-DATA buffered_instances TYPE ty_buffered_instances.

    METHODS get_min_loglevel_for_probclass
      IMPORTING i_problem_class TYPE REF TO /usi/cl_bal_enum_problem_class
      RETURNING VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level.

ENDCLASS.


CLASS /usi/cl_bal_enum_log_level IMPLEMENTATION.
  METHOD class_constructor.
    nothing         = NEW #( i_value = 0 ).
    very_important  = NEW #( i_value = 1 ).
    important       = NEW #( i_value = 2 ).
    medium          = NEW #( i_value = 3 ).
    additional_info = NEW #( i_value = 4 ).
    other           = NEW #( i_value = 5 ).
    everything      = NEW #( i_value = 6 ).
  ENDMETHOD.

  METHOD constructor.
    value = i_value.

    INSERT VALUE #( value    = value
                    instance = me )
           INTO TABLE buffered_instances.
  ENDMETHOD.

  METHOD get_by_value.
    FIELD-SYMBOLS <buffered_instance> TYPE ty_buffered_instance.

    ASSIGN buffered_instances[ value = i_value ] TO <buffered_instance>.

    IF sy-subrc = 0.
      r_result = <buffered_instance>-instance.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
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
    IF     i_log_level        IS BOUND
       AND i_log_level->value  < value.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_problem_class_relevant.
    DATA minimum_log_level TYPE REF TO /usi/cl_bal_enum_log_level.

    minimum_log_level = get_min_loglevel_for_probclass( i_problem_class ).

    IF minimum_log_level->is_higher_than( me ) = abap_false.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
