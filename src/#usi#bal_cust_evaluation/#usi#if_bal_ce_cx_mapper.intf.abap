INTERFACE /usi/if_bal_ce_cx_mapper
  PUBLIC .


  METHODS get_exception_mapper_classname
    IMPORTING
      !i_exception    TYPE REF TO cx_root
    RETURNING
      VALUE(r_result) TYPE /usi/bal_exception_mapper.

  METHODS get_fallback_mapper_classname
    RETURNING
      VALUE(r_result) TYPE /usi/bal_exception_mapper.

ENDINTERFACE.
