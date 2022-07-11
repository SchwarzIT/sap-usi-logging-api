CLASS /usi/cl_bal_cust_eval_factory DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cust_eval_factory.

    "! Factory-Method
    "!
    "! @parameter r_result | Instance
    CLASS-METHODS get_instance
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_cust_eval_factory.

    "! Constructor
    "!
    "! @parameter i_dao_factory | DAO-Factory
    METHODS constructor
      IMPORTING
        i_dao_factory TYPE REF TO /usi/if_bal_cust_dao_factory.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA instance TYPE REF TO /usi/if_bal_cust_eval_factory.

    DATA: dao_factory      TYPE REF TO /usi/if_bal_cust_dao_factory,
          exception_mapper TYPE REF TO /usi/if_bal_ce_cx_mapper.

ENDCLASS.



CLASS /usi/cl_bal_cust_eval_factory IMPLEMENTATION.
  METHOD /usi/if_bal_cust_eval_factory~get_data_containers.
    DATA dao TYPE REF TO /usi/if_bal_cd_data_containers.

    dao = dao_factory->get_data_containers( ).
    CREATE OBJECT r_result TYPE /usi/cl_bal_ce_data_containers
      EXPORTING
        i_customizing_dao = dao.
  ENDMETHOD.


  METHOD /usi/if_bal_cust_eval_factory~get_exception_mapper.
    DATA dao TYPE REF TO /usi/if_bal_cd_cx_mapper.

    IF exception_mapper IS NOT BOUND.
      dao = dao_factory->get_exception_mapper( ).
      CREATE OBJECT exception_mapper TYPE /usi/cl_bal_ce_cx_mapper
        EXPORTING
          i_customizing_dao = dao.
    ENDIF.

    r_result = exception_mapper.
  ENDMETHOD.


  METHOD /usi/if_bal_cust_eval_factory~get_log_level_by_client.
    DATA dao TYPE REF TO /usi/if_bal_cd_log_lv_by_clnt.

    dao = dao_factory->get_log_level_by_client( ).
    CREATE OBJECT r_result TYPE /usi/cl_bal_ce_log_lv_by_clnt
      EXPORTING
        i_customizing_dao = dao.
  ENDMETHOD.


  METHOD /usi/if_bal_cust_eval_factory~get_log_level_by_log_object.
    DATA dao TYPE REF TO /usi/if_bal_cd_log_lv_by_obj.

    dao = dao_factory->get_log_level_by_log_object( ).
    CREATE OBJECT r_result TYPE /usi/cl_bal_ce_log_lv_by_obj
      EXPORTING
        i_customizing_dao = dao.
  ENDMETHOD.


  METHOD /usi/if_bal_cust_eval_factory~get_log_level_by_user.
    DATA dao TYPE REF TO /usi/if_bal_cd_log_lv_by_user.

    dao = dao_factory->get_log_level_by_user( ).
    CREATE OBJECT r_result TYPE /usi/cl_bal_ce_log_lv_by_user
      EXPORTING
        i_customizing_dao = dao.
  ENDMETHOD.


  METHOD /usi/if_bal_cust_eval_factory~get_retention_parameters.
    DATA dao TYPE REF TO /usi/if_bal_cd_retention.

    dao = dao_factory->get_retention_parameters( ).
    CREATE OBJECT r_result TYPE /usi/cl_bal_ce_retention
      EXPORTING
        i_customizing_dao = dao.
  ENDMETHOD.


  METHOD constructor.
    dao_factory = i_dao_factory.
  ENDMETHOD.


  METHOD get_instance.
    DATA dao_factory TYPE REF TO /usi/if_bal_cust_dao_factory.

    IF instance IS NOT BOUND.
      CREATE OBJECT dao_factory TYPE /usi/cl_bal_cust_dao_factory.
      CREATE OBJECT instance TYPE /usi/cl_bal_cust_eval_factory
        EXPORTING
          i_dao_factory = dao_factory.
    ENDIF.

    r_result = instance.
  ENDMETHOD.
ENDCLASS.
