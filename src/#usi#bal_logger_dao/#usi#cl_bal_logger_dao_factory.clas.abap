CLASS /usi/cl_bal_logger_dao_factory DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_logger_dao_factory.

    "! <h1>Factory Method (Singleton pattern)</h1>
    "!
    "! @parameter r_result | Factory instance
    CLASS-METHODS get_instance
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_logger_dao_factory.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO /usi/if_bal_logger_dao_factory.

ENDCLASS.


CLASS /usi/cl_bal_logger_dao_factory IMPLEMENTATION.
  METHOD /usi/if_bal_logger_dao_factory~get_data_container_collection.
    r_result = NEW /usi/cl_bal_data_cont_coll_dao( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_dao_factory~get_log.
    r_result = NEW /usi/cl_bal_log_dao( i_log_object           = i_log_object
                                        i_sub_object           = i_sub_object
                                        i_external_id          = i_external_id
                                        i_retention_parameters = i_retention_parameters
                                        i_context              = i_context
                                        i_params               = i_params ).
  ENDMETHOD.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      instance = NEW /usi/cl_bal_logger_dao_factory( ).
    ENDIF.
    r_result = instance.
  ENDMETHOD.
ENDCLASS.
