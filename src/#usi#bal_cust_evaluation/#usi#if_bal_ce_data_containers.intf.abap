INTERFACE /usi/if_bal_ce_data_containers
  PUBLIC .
  METHODS get_relevant_plugin_classnames
    IMPORTING
      !i_log_object   TYPE balobj_d
      !i_sub_object   TYPE balsubobj OPTIONAL
      !i_log_level    TYPE REF TO /usi/cl_bal_enum_log_level
    RETURNING
      VALUE(r_result) TYPE /usi/bal_data_cont_classnames.
ENDINTERFACE.
