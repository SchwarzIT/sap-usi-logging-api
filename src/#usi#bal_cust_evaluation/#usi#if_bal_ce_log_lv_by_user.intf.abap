INTERFACE /usi/if_bal_ce_log_lv_by_user
  PUBLIC .


  METHODS get_log_level
    IMPORTING
      !i_user_name    TYPE xubname DEFAULT sy-uname
      !i_log_object   TYPE balobj_d OPTIONAL
      !i_sub_object   TYPE balsubobj OPTIONAL
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level .
ENDINTERFACE.
