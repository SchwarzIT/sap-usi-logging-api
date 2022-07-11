INTERFACE /usi/if_bal_textpool_reader PUBLIC.

  TYPES: ty_text_symbol_key    TYPE c LENGTH 3,
         ty_text_symbol_text   TYPE c LENGTH 132,
         ty_selection_text_key TYPE c LENGTH 8,
         ty_selection_text     TYPE c LENGTH 30,
         ty_textpool_table     TYPE SORTED TABLE OF textpool WITH UNIQUE KEY id key.

  "! <h1>Read textpool of program</h1>
  "!
  "! @parameter i_program | Program name
  "! @parameter i_language | Language
  "! @parameter r_result | Textpool
  "! @raising /usi/cx_bal_not_found | Error
  METHODS get_textpool
    IMPORTING
      i_program       TYPE syrepid
      i_language      TYPE sylangu DEFAULT sy-langu
    RETURNING
      VALUE(r_result) TYPE ty_textpool_table
    RAISING
      /usi/cx_bal_not_found.

  "! <h1>Get text symbol from the textpool of a program</h1>
  "!
  "! @parameter i_program | Program name
  "! @parameter i_language | Language
  "! @parameter i_text_symbol_key | Text symbol key ('001' for TEXT-001)
  "! @parameter r_result | Text
  "! @raising /usi/cx_bal_not_found | Error
  METHODS get_text_symbol
    IMPORTING
      i_program         TYPE syrepid
      i_language        TYPE sylangu DEFAULT sy-langu
      i_text_symbol_key TYPE ty_text_symbol_key
    RETURNING
      VALUE(r_result)   TYPE ty_text_symbol_text
    RAISING
      /usi/cx_bal_not_found.

  "! <h1>Get selection text from the textpool of a program</h1>
  "!
  "! @parameter i_program | Program name
  "! @parameter i_language | Language
  "! @parameter i_selection_text_key | Name of parameter or select option
  "! @parameter r_result | Text
  "! @raising /usi/cx_bal_not_found | Error
  METHODS get_selection_text
    IMPORTING
      i_program            TYPE syrepid
      i_language           TYPE sylangu DEFAULT sy-langu
      i_selection_text_key TYPE ty_selection_text_key
    RETURNING
      VALUE(r_result)      TYPE ty_selection_text
    RAISING
      /usi/cx_bal_not_found.

ENDINTERFACE.
