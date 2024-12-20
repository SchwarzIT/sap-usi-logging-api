CLASS /usi/cl_bal_cust_dao_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cust_dao_factory.

ENDCLASS.


CLASS /usi/cl_bal_cust_dao_factory IMPLEMENTATION.
  METHOD /usi/if_bal_cust_dao_factory~get_data_containers.
    r_result = NEW /usi/cl_bal_cd_data_containers( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_exception_mapper.
    r_result = NEW /usi/cl_bal_cd_cx_mapper( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_log_level_by_log_object.
    r_result = NEW /usi/cl_bal_cd_log_lv_by_obj( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_log_level_by_client.
    r_result = NEW /usi/cl_bal_cd_log_lv_by_clnt( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_log_level_by_user.
    r_result = NEW /usi/cl_bal_cd_log_lv_by_user( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_retention_parameters.
    r_result = NEW /usi/cl_bal_cd_retention( ).
  ENDMETHOD.
ENDCLASS.
