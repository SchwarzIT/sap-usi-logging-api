INTERFACE /usi/if_bal_logger_dao_factory
  PUBLIC .
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

  METHODS get_data_container_collection
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_data_cont_coll_dao.
ENDINTERFACE.
