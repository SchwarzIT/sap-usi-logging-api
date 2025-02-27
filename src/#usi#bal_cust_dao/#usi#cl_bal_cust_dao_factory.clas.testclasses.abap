*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/cl_bal_cust_dao_factory.

    METHODS setup.

    METHODS test_exception_mapper_dao     FOR TESTING.
    METHODS test_regular_log_level_dao    FOR TESTING.
    METHODS test_increased_log_level_dao  FOR TESTING.
    METHODS test_message_context_data_dao FOR TESTING.
    METHODS test_retention_parameter_dao  FOR TESTING.

ENDCLASS.


CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_exception_mapper_dao.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_exception_mapper( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_regular_log_level_dao.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_log_level_by_log_object( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_increased_log_level_dao.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_log_level_by_user( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_message_context_data_dao.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_data_containers( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_retention_parameter_dao.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_retention_parameters( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.
ENDCLASS.
