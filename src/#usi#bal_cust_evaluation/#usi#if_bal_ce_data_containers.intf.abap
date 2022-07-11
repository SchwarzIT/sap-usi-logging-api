INTERFACE /usi/if_bal_ce_data_containers PUBLIC.

  "! Get class names of relevant data container classes
  "!
  "! @parameter i_log_object | Log object
  "! @parameter i_sub_object | Sub object
  "! @parameter i_log_level | Current log level
  "! @parameter r_result | Class names
  METHODS get_relevant_plugin_classnames
    IMPORTING
      i_log_object    TYPE balobj_d
      i_sub_object    TYPE balsubobj OPTIONAL
      i_log_level     TYPE REF TO /usi/cl_bal_enum_log_level
    RETURNING
      VALUE(r_result) TYPE /usi/bal_data_cont_classnames.

ENDINTERFACE.
