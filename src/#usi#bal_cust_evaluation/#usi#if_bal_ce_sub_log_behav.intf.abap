INTERFACE /usi/if_bal_ce_sub_log_behav PUBLIC.

  "! Get Sub-Log Behavior
  "!
  "! @parameter i_log_object | Log object
  "! @parameter i_sub_object | Sub object
  "! @parameter r_result     | Sub-Log Behavior
  METHODS get_sub_log_behavior
    IMPORTING i_log_object    TYPE balobj_d
              i_sub_object    TYPE balsubobj
    RETURNING VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_sub_log_behav.

ENDINTERFACE.
