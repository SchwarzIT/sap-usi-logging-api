CLASS /usi/cl_bal_language_priority DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    INTERFACES /usi/if_bal_language_priority.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_language_priority.

  PROTECTED SECTION.
  PRIVATE SECTION.
    ALIASES: ty_language  FOR /usi/if_bal_language_priority~ty_language,
             ty_languages FOR /usi/if_bal_language_priority~ty_languages.

    CLASS-DATA instance TYPE REF TO /usi/if_bal_language_priority.

    DATA: languages_by_priority TYPE ty_languages,
          processed_languages   TYPE HASHED TABLE OF sylangu WITH UNIQUE KEY table_line.

    METHODS constructor.

    METHODS insert_language
      IMPORTING
        i_language TYPE sylangu.

    METHODS get_fallback_languages.
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
    DATA language TYPE ty_language.

    INSERT i_language INTO TABLE processed_languages.
    IF sy-subrc EQ 0.
      language-priority = lines( languages_by_priority ) + 1.
      language-language = i_language.
      INSERT language INTO TABLE languages_by_priority.
    ENDIF.
  ENDMETHOD.

  METHOD get_fallback_languages.
    DATA: primary_language  TYPE sylangu,
          fallback_language TYPE sylangu.

    primary_language = sy-langu.
    DO 2 TIMES.
      SELECT  SINGLE lakett
        FROM  t002c
        INTO  fallback_language
        WHERE spras  EQ primary_language
        AND   lakett NE space.

      IF sy-subrc NE 0.
        EXIT.
      ELSE.
        insert_language( fallback_language ).
        primary_language = fallback_language.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD get_other_languages.
    DATA languages TYPE SORTED TABLE OF t778l WITH UNIQUE KEY seqen.
    FIELD-SYMBOLS <language> TYPE t778l.

    SELECT *
      FROM t778l
      INTO TABLE languages.

    LOOP AT languages ASSIGNING <language>.
      insert_language( <language>-langu ).
    ENDLOOP.
  ENDMETHOD.

  METHOD /usi/if_bal_language_priority~get_languages.
    r_result = languages_by_priority.
  ENDMETHOD.
ENDCLASS.
