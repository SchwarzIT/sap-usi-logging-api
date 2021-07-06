INTERFACE /usi/if_bal_cust_dao_factory
  PUBLIC .


  METHODS get_exception_mapper
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_cx_mapper .
  METHODS get_log_level_by_user
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_log_lv_by_user .
  METHODS get_data_containers
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_data_containers .
  METHODS get_log_level_by_log_object
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_log_lv_by_obj .
  METHODS get_retention_parameters
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_retention .
ENDINTERFACE.
