INTERFACE /usi/if_bal_cd_log_lv_by_user
  PUBLIC .


  TYPES:
    BEGIN OF ty_record,
      uname      TYPE xubname,
      endda      TYPE endda,
      log_object TYPE balobj_d,
      sub_object TYPE balsubobj,
      log_level  TYPE /usi/bal_log_level,
      auto_save  TYPE /usi/bal_auto_save_immediately,
    END   OF ty_record .
  TYPES:
    ty_records TYPE STANDARD TABLE OF ty_record WITH NON-UNIQUE DEFAULT KEY.

  METHODS get_records
    IMPORTING
      !i_user_name        TYPE xubname
      !i_endda_range      TYPE /usi/bal_date_range OPTIONAL
      !i_log_object_range TYPE /usi/bal_log_object_range OPTIONAL
      !i_sub_object_range TYPE /usi/bal_sub_object_range OPTIONAL
    RETURNING
      VALUE(r_result)     TYPE ty_records
    RAISING
      /usi/cx_bal_root .
ENDINTERFACE.
