INTERFACE /usi/if_bal_cust_dao_factory PUBLIC.

  "! Get DAO-Object for Customizing: Exception-Mappers
  "!
  "! @parameter r_result | DAO-Object
  METHODS get_exception_mapper
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_cx_mapper.

  "! Get DAO-Object for Customizing: Increased Log-Level by Client
  "!
  "! @parameter r_result | DAO-Object
  METHODS get_log_level_by_client
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_log_lv_by_clnt.

  "! Get DAO-Object for Customizing: Increased Log-Level by User
  "!
  "! @parameter r_result | DAO-Object
  METHODS get_log_level_by_user
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_log_lv_by_user.

  "! Get DAO-Object for Customizing: Data-Containers per Log-Level
  "!
  "! @parameter r_result | DAO-Object
  METHODS get_data_containers
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_data_containers.

  "! Get DAO-Object for Customizing: Product-Specific (regular) Log-Level
  "!
  "! @parameter r_result | DAO-Object
  METHODS get_log_level_by_log_object
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_log_lv_by_obj.

  "! Get DAO-Object for Customizing: Retention-Time per Log-Level
  "!
  "! @parameter r_result | DAO-Object
  METHODS get_retention_parameters
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_cd_retention.

ENDINTERFACE.
