INTERFACE /usi/if_bal_logger_dao_factory PUBLIC.

  "! <h1>Get persistency handler for log messages</h1>
  "!
  "! @parameter i_log_object | Log Object (Defined in SLG0)
  "! @parameter i_sub_object | Sub Object (Defined in SLG0)
  "! @parameter i_external_id | External ID of the log (Filter in SLG1 - use ID of processed object if feasible)
  "! @parameter i_retention_parameters | Retention parameters
  "! @parameter i_context | Context structure
  "! @parameter i_params | Parameters for standard API
  "! @parameter r_result | Persistency handler
  "! @raising /usi/cx_bal_root | Error in standard API (Wrong parameter combination?)
  METHODS get_log
    IMPORTING
      i_log_object           TYPE balobj_d
      i_sub_object           TYPE balsubobj OPTIONAL
      i_external_id          TYPE balnrext OPTIONAL
      i_retention_parameters TYPE /usi/bal_retention_parameters
      i_context              TYPE bal_s_cont OPTIONAL
      i_params               TYPE bal_s_parm OPTIONAL
    RETURNING
      VALUE(r_result)        TYPE REF TO /usi/if_bal_log_dao
    RAISING
      /usi/cx_bal_root.

  "! <h1>Get persistency handler for data container collections</h1>
  "!
  "! @parameter r_result | Persistency handler
  METHODS get_data_container_collection
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_data_cont_coll_dao.

ENDINTERFACE.
