INTERFACE /usi/if_bal_cust_eval_factory
  PUBLIC .

  METHODS get_data_containers
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_data_containers.

  METHODS get_exception_mapper
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_cx_mapper.

  METHODS get_log_level_by_log_object
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_log_lv_by_obj.

  METHODS get_log_level_by_user
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_log_lv_by_user.

  METHODS get_retention_parameters
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_retention.

ENDINTERFACE.
