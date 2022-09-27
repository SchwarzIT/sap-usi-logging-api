CLASS /usi/cl_bal_textpool_reader DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_textpool_reader.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF text_types,
                 selection_text TYPE textpoolid VALUE 'S',
                 text_symbol    TYPE textpoolid VALUE 'I',
               END   OF text_types.

    METHODS get_textpool_line
      IMPORTING
        i_program       TYPE syrepid
        i_language      TYPE sylangu
        i_id            TYPE textpoolid
        i_key           TYPE csequence
      RETURNING
        VALUE(r_result) TYPE textpooltx
      RAISING
        /usi/cx_bal_not_found.

ENDCLASS.



CLASS /usi/cl_bal_textpool_reader IMPLEMENTATION.
  METHOD /usi/if_bal_textpool_reader~get_selection_text.
    r_result  = get_textpool_line( i_program  = i_program
                                   i_language = i_language
                                   i_id       = text_types-selection_text
                                   i_key      = i_selection_text_key ).
  ENDMETHOD.

  METHOD /usi/if_bal_textpool_reader~get_text_symbol.
    r_result  = get_textpool_line( i_program  = i_program
                                   i_language = i_language
                                   i_id       = text_types-text_symbol
                                   i_key      = i_text_symbol_key ).
  ENDMETHOD.

  METHOD /usi/if_bal_textpool_reader~get_textpool.
    DATA: message_variable_1 TYPE symsgv,
          message_variable_2 TYPE symsgv.

    READ TEXTPOOL i_program INTO r_result LANGUAGE i_language.

    IF sy-subrc NE 0.
      message_variable_1 = i_program.
      message_variable_2 = i_language.

      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>textpool_not_found
          param1 = message_variable_1
          param2 = message_variable_2.
    ENDIF.
  ENDMETHOD.

  METHOD get_textpool_line.
    DATA: message_variable_1 TYPE symsgv,
          message_variable_2 TYPE symsgv,
          message_variable_3 TYPE symsgv,
          message_variable_4 TYPE symsgv,
          textpool           TYPE /usi/if_bal_textpool_reader=>ty_textpool_table.

    FIELD-SYMBOLS <textpool_line> TYPE textpool.

    textpool = /usi/if_bal_textpool_reader~get_textpool( i_program  = i_program
                                                         i_language = i_language ).

    READ TABLE textpool
      ASSIGNING <textpool_line>
      WITH KEY id  = i_id
               key = i_key.

    IF sy-subrc EQ 0.
      r_result = <textpool_line>-entry.
    ELSE.
      message_variable_1 = i_id.
      message_variable_2 = i_key.
      message_variable_3 = i_program.
      message_variable_4 = i_language.

      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>text_not_found_in_textpool
          param1 = message_variable_1
          param2 = message_variable_2
          param3 = message_variable_3
          param4 = message_variable_4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
