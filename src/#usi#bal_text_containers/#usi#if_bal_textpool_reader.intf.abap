INTERFACE /usi/if_bal_textpool_reader
  PUBLIC .

  TYPES: ty_text_symbol_key    TYPE c LENGTH 3,
         ty_text_symbol_text   TYPE c LENGTH 132,
         ty_selection_text_key TYPE c LENGTH 8,
         ty_selection_text     TYPE c LENGTH 30.

  METHODS get_textpool
    IMPORTING
      i_program       TYPE syrepid
      i_language      TYPE sylangu DEFAULT sy-langu
    RETURNING
      VALUE(r_result) TYPE textpool_table
    RAISING
      /usi/cx_bal_not_found.

  METHODS get_text_symbol
    IMPORTING
      i_program         TYPE syrepid
      i_language        TYPE sylangu DEFAULT sy-langu
      i_text_symbol_key TYPE ty_text_symbol_key
    RETURNING
      VALUE(r_result)   TYPE ty_text_symbol_text
    RAISING
      /usi/cx_bal_not_found.

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
