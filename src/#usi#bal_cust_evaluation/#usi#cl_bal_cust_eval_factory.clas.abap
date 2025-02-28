CLASS /usi/cl_bal_cust_eval_factory DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cust_eval_factory.

    "! Factory-Method
    "!
    "! @parameter r_result | Instance
    CLASS-METHODS get_instance
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_cust_eval_factory.

    "! Constructor
    "!
    "! @parameter i_dao_factory | DAO-Factory
    METHODS constructor
      IMPORTING i_dao_factory TYPE REF TO /usi/if_bal_cust_dao_factory.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO /usi/if_bal_cust_eval_factory.

    DATA: dao_factory      TYPE REF TO /usi/if_bal_cust_dao_factory,
          exception_mapper TYPE REF TO /usi/if_bal_ce_cx_mapper.

ENDCLASS.


CLASS /usi/cl_bal_cust_eval_factory IMPLEMENTATION.
  METHOD /usi/if_bal_cust_eval_factory~get_data_containers.
    DATA(dao) = dao_factory->get_data_containers( ).
    r_result = NEW /usi/cl_bal_ce_data_containers( dao ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_eval_factory~get_exception_mapper.
    IF exception_mapper IS NOT BOUND.
      DATA(dao) = dao_factory->get_exception_mapper( ).
      exception_mapper = NEW /usi/cl_bal_ce_cx_mapper( i_customizing_dao = dao ).
    ENDIF.

    r_result = exception_mapper.
  ENDMETHOD.

  METHOD /usi/if_bal_cust_eval_factory~get_log_level_by_client.
    DATA(dao) = dao_factory->get_log_level_by_client( ).
    r_result = NEW /usi/cl_bal_ce_log_lv_by_clnt( i_customizing_dao = dao ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_eval_factory~get_log_level_by_log_object.
    DATA(dao) = dao_factory->get_log_level_by_log_object( ).
    r_result = NEW /usi/cl_bal_ce_log_lv_by_obj( i_customizing_dao = dao ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_eval_factory~get_log_level_by_user.
    DATA(dao) = dao_factory->get_log_level_by_user( ).
    r_result = NEW /usi/cl_bal_ce_log_lv_by_user( i_customizing_dao = dao ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_eval_factory~get_retention_parameters.
    DATA(dao) = dao_factory->get_retention_parameters( ).
    r_result = NEW /usi/cl_bal_ce_retention( i_customizing_dao = dao ).
  ENDMETHOD.

  METHOD constructor.
    dao_factory = i_dao_factory.
  ENDMETHOD.

  METHOD get_instance.
    IF instance IS NOT BOUND.
      DATA(dao_factory) = NEW /usi/cl_bal_cust_dao_factory( ).
      instance = NEW /usi/cl_bal_cust_eval_factory( i_dao_factory = dao_factory ).
    ENDIF.

    r_result = instance.
  ENDMETHOD.
ENDCLASS.
