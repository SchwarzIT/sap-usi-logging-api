CLASS /usi/cl_bal_tc_report_text_c40 DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_text_container_c40.

    TYPES ty_text_key TYPE c LENGTH 3.

    "! <h1>Create instance for any program</h1>
    "!
    "! @parameter i_program | Program
    "! @parameter i_text_key | Text key
    "! @parameter i_text | Text (Dummy - ignored internally, passing the text will avoid ATC-Messages for unused texts)
    "! @parameter r_result | Text container instance
    CLASS-METHODS create_for_program
      IMPORTING
        i_program       TYPE programm
        i_text_key      TYPE ty_text_key
        i_text          TYPE csequence OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_tc_report_text_c40.

    "! <h1>Create instance for current program</h1>
    "!
    "! @parameter i_text_key | Text key
    "! @parameter i_text | Text (Dummy - ignored internally, passing the text will avoid ATC-Messages for unused texts)
    "! @parameter r_result | Text container instance
    CLASS-METHODS create
      IMPORTING
        i_text_key      TYPE ty_text_key
        i_text          TYPE csequence OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_tc_report_text_c40.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_program | Program name
    "! @parameter i_text_key | Text key
    METHODS constructor
      IMPORTING
        i_program  TYPE programm
        i_text_key TYPE ty_text_key.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: language_priority TYPE REF TO /usi/if_bal_language_priority,
          program           TYPE programm,
          text_key          TYPE ty_text_key,
          textpool_reader   TYPE REF TO /usi/if_bal_textpool_reader.

    CLASS-METHODS get_name_of_calling_program
      RETURNING
        VALUE(r_result) TYPE programm.

ENDCLASS.



CLASS /usi/cl_bal_tc_report_text_c40 IMPLEMENTATION.
  METHOD /usi/if_bal_text_container_c40~deserialize.
    DATA: exception TYPE REF TO cx_transformation_error,
          program   TYPE programm,
          text_key  TYPE ty_text_key.

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML i_serialized_text_container
          RESULT program  = program
                 text_key = text_key.

        r_result  = create_for_program( i_program  = program
                                        i_text_key = text_key ).
      CATCH cx_transformation_error INTO exception.
        RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
          EXPORTING
            textid   = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch
            previous = exception.
    ENDTRY.
  ENDMETHOD.


  METHOD /usi/if_bal_text_container_c40~get_classname.
    r_result = '/USI/CL_BAL_TC_REPORT_TEXT_C40'.
  ENDMETHOD.


  METHOD /usi/if_bal_text_container_c40~get_text.
    DATA languages TYPE language_priority->ty_languages.

    FIELD-SYMBOLS <language> TYPE language_priority->ty_language.

    languages = language_priority->get_languages( ).
    LOOP AT languages ASSIGNING <language>.
      TRY.
          r_result  = textpool_reader->get_text_symbol( i_program         = program
                                                        i_language        = <language>-language
                                                        i_text_symbol_key = text_key ).
          EXIT.
        CATCH /usi/cx_bal_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    ASSERT ID /usi/bal_log_writer
      FIELDS program
             text_key
      CONDITION r_result IS NOT INITIAL.
  ENDMETHOD.


  METHOD /usi/if_bal_text_container_c40~serialize.
    CALL TRANSFORMATION id
      SOURCE program  = program
             text_key = text_key
      RESULT XML r_result.
  ENDMETHOD.


  METHOD constructor.
    language_priority = /usi/cl_bal_language_priority=>get_instance( ).
    CREATE OBJECT textpool_reader TYPE /usi/cl_bal_textpool_reader.

    program   = i_program.
    text_key  = i_text_key.

    SET LOCALE LANGUAGE sy-langu.
    TRANSLATE text_key TO UPPER CASE.
  ENDMETHOD.


  METHOD create.
    DATA program TYPE programm.
    program   = get_name_of_calling_program( ).
    CREATE OBJECT r_result
      EXPORTING
        i_program  = program
        i_text_key = i_text_key.
  ENDMETHOD.


  METHOD create_for_program.
    CREATE OBJECT r_result
      EXPORTING
        i_program  = i_program
        i_text_key = i_text_key.
  ENDMETHOD.


  METHOD get_name_of_calling_program.
    DATA callstack TYPE abap_callstack.
    FIELD-SYMBOLS: <callstack_line> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = 0
      IMPORTING
        callstack = callstack.

    DELETE ADJACENT DUPLICATES FROM callstack COMPARING mainprogram.
    READ TABLE callstack ASSIGNING <callstack_line> INDEX 2.
    IF sy-subrc NE 0.
      READ TABLE callstack ASSIGNING <callstack_line> INDEX 1.
    ENDIF.

    r_result = <callstack_line>-mainprogram.
  ENDMETHOD.
ENDCLASS.
