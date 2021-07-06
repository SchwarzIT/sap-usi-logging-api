INTERFACE /usi/if_bal_factory
  PUBLIC .


  METHODS create_new_logger
    IMPORTING
      i_log_object    TYPE balobj_d
      i_sub_object    TYPE balsubobj OPTIONAL
      i_external_id   TYPE balnrext  OPTIONAL
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_logger.

  METHODS get_existing_logger
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_logger.

ENDINTERFACE.
