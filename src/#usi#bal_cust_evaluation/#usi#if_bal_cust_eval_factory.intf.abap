INTERFACE /usi/if_bal_cust_eval_factory PUBLIC.

  "! Get Customizing-Evaluator-Object for Data-Containers per Log-Level.
  "!
  "! The DAO-Objects might deliver multiple database records.
  "! The customizing-evaluator will find the most relevant one.
  "!
  "! @parameter r_result | Customizing-Evaluator-Object
  METHODS get_data_containers
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_data_containers.

  "! Get Customizing-Evaluator-Object for Exception-Mappers.
  "!
  "! The DAO-Objects might deliver multiple database records.
  "! The customizing-evaluator will find the most relevant one.
  "!
  "! @parameter r_result | Customizing-Evaluator-Object
  METHODS get_exception_mapper
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_cx_mapper.

  "! Get Customizing-Evaluator-Object for Product-Specific (regular) Log-Level.
  "!
  "! The DAO-Objects might deliver multiple database records.
  "! The customizing-evaluator will find the most relevant one.
  "!
  "! @parameter r_result | Customizing-Evaluator-Object
  METHODS get_log_level_by_log_object
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_log_lv_by_obj.

  "! Get Customizing-Evaluator-Object for Increased Log-Level by Client
  "!
  "! The DAO-Objects might deliver multiple database records.
  "! The customizing-evaluator will find the most relevant one.
  "!
  "! @parameter r_result | Customizing-Evaluator-Object
  METHODS get_log_level_by_client
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_log_lv_by_clnt.

  "! Get Customizing-Evaluator-Object for Increased Log-Level by User
  "!
  "! The DAO-Objects might deliver multiple database records.
  "! The customizing-evaluator will find the most relevant one.
  "!
  "! @parameter r_result | Customizing-Evaluator-Object
  METHODS get_log_level_by_user
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_log_lv_by_user.

  "! Get Customizing-Evaluator-Object for Retention-Time per Log-Level
  "!
  "! The DAO-Objects might deliver multiple database records.
  "! The customizing-evaluator will find the most relevant one.
  "!
  "! @parameter r_result | Customizing-Evaluator-Object
  METHODS get_retention_parameters
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_ce_retention.

ENDINTERFACE.
