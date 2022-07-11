INTERFACE /usi/if_bal_ce_cx_mapper PUBLIC.

  "! Get exception mapper class name for given exception
  "!
  "! @parameter i_exception | The to-be-mapped exception
  "! @parameter r_result | Mapper class name
  METHODS get_exception_mapper_classname
    IMPORTING
      i_exception     TYPE REF TO cx_root
    RETURNING
      VALUE(r_result) TYPE /usi/bal_exception_mapper.

  "! Get fallback mapper class name
  "!
  "! @parameter r_result | Fallback mapper class name
  METHODS get_fallback_mapper_classname
    RETURNING
      VALUE(r_result) TYPE /usi/bal_exception_mapper.

ENDINTERFACE.
