INTERFACE /usi/if_bal_cd_retention PUBLIC.

  TYPES: BEGIN OF ty_record,
           log_level  TYPE /usi/bal_log_level,
           log_object TYPE balobj_d,
           sub_object TYPE balsubobj.
  INCLUDE TYPE /usi/bal_retention_parameters AS retention_parameters.
  TYPES END OF ty_record.

  TYPES ty_records TYPE STANDARD TABLE OF ty_record WITH EMPTY KEY.

  "! <h1>Read table /usi/bal_lv_rtim (Retention time per Log-Level)</h1>
  "!
  "! @parameter r_result | Records
  "! @raising /usi/cx_bal_root | Nothing found
  METHODS get_records
    IMPORTING
      i_log_level        TYPE /usi/bal_log_level
      i_log_object_range TYPE /usi/bal_log_object_range OPTIONAL
      i_sub_object_range TYPE /usi/bal_sub_object_range OPTIONAL
    RETURNING
      VALUE(r_result)    TYPE ty_records
    RAISING
      /usi/cx_bal_root.

ENDINTERFACE.
