INTERFACE /usi/if_bal_cd_cx_mapper PUBLIC.

  TYPES: BEGIN OF ty_record,
           exception_class TYPE /usi/bal_exception_classname,
           mapper_class    TYPE /usi/bal_exception_mapper,
         END   OF ty_record,
         ty_records TYPE STANDARD TABLE OF ty_record WITH EMPTY KEY.

  "! Read table /usi/bal_cx_map (Exception Mapper Classes)
  "!
  "! @parameter r_result | Records
  "! @raising /usi/cx_bal_root | Nothing found
  METHODS get_records
    RETURNING
      VALUE(r_result) TYPE ty_records
    RAISING
      /usi/cx_bal_root .

ENDINTERFACE.
