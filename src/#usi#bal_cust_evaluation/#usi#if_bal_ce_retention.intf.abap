INTERFACE /usi/if_bal_ce_retention
  PUBLIC .


  METHODS get_parameters
    IMPORTING
      !i_log_level    TYPE REF TO /usi/cl_bal_enum_log_level
      !i_log_object   TYPE balobj_d  OPTIONAL
      !i_sub_object   TYPE balsubobj OPTIONAL
    RETURNING
      VALUE(r_result) TYPE /usi/bal_retention_parameters .
ENDINTERFACE.
