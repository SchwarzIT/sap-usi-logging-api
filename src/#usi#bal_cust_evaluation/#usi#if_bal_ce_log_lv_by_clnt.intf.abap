INTERFACE /usi/if_bal_ce_log_lv_by_clnt PUBLIC.

  "! Get package size for auto-save
  "!
  "! @parameter i_log_object | Log object
  "! @parameter i_sub_object | Sub object
  "! @parameter r_result | Auto-Save-Package-Size (0 = No auto-save)
  METHODS get_auto_save_package_size
    IMPORTING
      i_log_object   TYPE balobj_d OPTIONAL
      i_sub_object   TYPE balsubobj OPTIONAL
    RETURNING
      VALUE(r_result) TYPE /usi/bal_auto_save_pckg_size.

  "! Get Log Level
  "!
  "! @parameter i_log_object | Log object
  "! @parameter i_sub_object | Sub object
  "! @parameter r_result | Log Level
  METHODS get_log_level
    IMPORTING
      i_log_object    TYPE balobj_d OPTIONAL
      i_sub_object    TYPE balsubobj OPTIONAL
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level.

ENDINTERFACE.
