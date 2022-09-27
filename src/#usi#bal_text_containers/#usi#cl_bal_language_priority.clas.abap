CLASS /usi/cl_bal_language_priority DEFINITION PUBLIC FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    INTERFACES /usi/if_bal_language_priority.

    "! <h1>Factory method (Singleton pattern)</h1>
    "!
    "! <p>Creates the singleton if needed. The language list will be created once during the object creation.</p>
    "!
    "! @parameter r_result | Singleton
    CLASS-METHODS get_instance
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_language_priority.

    "! <h1>Constructor</h1>
    METHODS constructor.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO /usi/if_bal_language_priority.

    DATA: languages_by_priority TYPE /usi/if_bal_language_priority~ty_languages,
          processed_languages   TYPE HASHED TABLE OF sylangu WITH UNIQUE KEY table_line.

    METHODS insert_language
      IMPORTING
        i_language TYPE sylangu.

    METHODS get_fallback_languages.

    METHODS get_fallback_language
      IMPORTING
        i_primary_language TYPE sylangu
      RETURNING
        VALUE(r_result)    TYPE sylangu
      RAISING
        /usi/cx_bal_root.

    METHODS get_other_languages.

ENDCLASS.



CLASS /usi/cl_bal_language_priority IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance TYPE /usi/cl_bal_language_priority.
    ENDIF.
    r_result = instance.
  ENDMETHOD.

  METHOD constructor.
    insert_language( sy-langu ).
    get_fallback_languages( ).
    get_other_languages( ).
  ENDMETHOD.

  METHOD insert_language.
    DATA language TYPE /usi/if_bal_language_priority~ty_language.

    INSERT i_language INTO TABLE processed_languages.
    IF sy-subrc EQ 0.
      language-priority = lines( languages_by_priority ) + 1.
      language-language = i_language.
      INSERT language INTO TABLE languages_by_priority.
    ENDIF.
  ENDMETHOD.

  METHOD get_fallback_languages.
    DATA fallback_language TYPE sylangu.

    TRY.
        fallback_language = get_fallback_language( sy-langu ).
        insert_language( fallback_language ).

        fallback_language = get_fallback_language( fallback_language ).
        insert_language( fallback_language ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD get_fallback_language.
    SELECT SINGLE lakett
      FROM t002c
      INTO r_result
      WHERE spras  EQ i_primary_language
        AND lakett NE space.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>generic_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD get_other_languages.
    TYPES: BEGIN OF ty_language,
             seqen TYPE seqen,
             langu TYPE langu,
           END   OF ty_language,
           ty_languages TYPE SORTED TABLE OF ty_language WITH UNIQUE KEY seqen.

    DATA languages TYPE ty_languages.
    FIELD-SYMBOLS <language> TYPE ty_language.

    SELECT seqen langu
      FROM t778l
      INTO CORRESPONDING FIELDS OF TABLE languages.

    LOOP AT languages ASSIGNING <language>.
      insert_language( <language>-langu ).
    ENDLOOP.
  ENDMETHOD.

  METHOD /usi/if_bal_language_priority~get_languages.
    r_result = languages_by_priority.
  ENDMETHOD.
ENDCLASS.
