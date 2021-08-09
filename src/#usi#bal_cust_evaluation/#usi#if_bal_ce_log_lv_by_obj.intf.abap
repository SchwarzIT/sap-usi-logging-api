INTERFACE /usi/if_bal_ce_log_lv_by_obj
  PUBLIC .


  METHODS get_auto_save_package_size
    IMPORTING
      !i_log_object   TYPE balobj_d
      !i_sub_object   TYPE balsubobj
    RETURNING
      VALUE(r_result) TYPE /usi/bal_auto_save_pckg_size .

  METHODS get_log_level
    IMPORTING
      !i_log_object   TYPE balobj_d
      !i_sub_object   TYPE balsubobj
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level .
ENDINTERFACE.
