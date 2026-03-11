INTERFACE /usi/if_bal_cd_sub_log_behav PUBLIC.

  TYPES: BEGIN OF ty_record,
           log_object TYPE balobj_d,
           sub_object TYPE balsubobj,
           behavior   TYPE /usi/bal_sub_log_behavior,
         END   OF ty_record,
         ty_records TYPE STANDARD TABLE OF ty_record WITH EMPTY KEY.

  "! Read table /usi/bal_sl_beha (Sub-Log Behavior)
  "!
  "! @parameter i_log_object_range | Log-Object Range
  "! @parameter i_sub_object_range | Subobject Range
  "! @parameter r_result         | Records
  "! @raising   /usi/cx_bal_root | Nothing found
  METHODS get_records
    IMPORTING i_log_object_range TYPE /usi/bal_log_object_range OPTIONAL
              i_sub_object_range TYPE /usi/bal_sub_object_range OPTIONAL
    RETURNING VALUE(r_result)    TYPE ty_records
    RAISING   /usi/cx_bal_root.

ENDINTERFACE.
