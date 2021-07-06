INTERFACE /usi/if_bal_logger_bl_factory
  PUBLIC .

  METHODS get_exception_mapper
    IMPORTING
      i_exception     TYPE REF TO cx_root
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_exception_mapper.

  METHODS get_token
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_token.
ENDINTERFACE.
