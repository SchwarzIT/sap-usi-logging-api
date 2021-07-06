INTERFACE /usi/if_bal_cd_data_containers
  PUBLIC .


  TYPES:
    BEGIN OF ty_record,
      classname     TYPE /usi/bal_data_cont_classname,
      log_object    TYPE balobj_d,
      sub_object    TYPE balsubobj,
      min_log_level TYPE /usi/bal_log_level,
    END   OF ty_record .
  TYPES:
    ty_records TYPE STANDARD TABLE OF ty_record WITH NON-UNIQUE DEFAULT KEY.

  METHODS get_records
    IMPORTING
      !i_log_object_range TYPE /usi/bal_log_object_range OPTIONAL
      !i_sub_object_range TYPE /usi/bal_sub_object_range OPTIONAL
    RETURNING
      VALUE(r_result)     TYPE ty_records
    RAISING
      /usi/cx_bal_root .

ENDINTERFACE.
