*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/cl_bal_cust_dao_factory.

    METHODS setup.

    METHODS test_get_data_containers      FOR TESTING.
    METHODS test_get_exception_mapper     FOR TESTING.
    METHODS test_get_log_level_by_client  FOR TESTING.
    METHODS test_get_log_level_by_log_obj FOR TESTING.
    METHODS test_get_log_level_by_user    FOR TESTING.
    METHODS test_get_retention_parameters FOR TESTING.
    METHODS test_get_sub_log_behavior     FOR TESTING.

ENDCLASS.


CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD test_get_data_containers.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_data_containers( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_exception_mapper.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_exception_mapper( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_log_level_by_client.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_log_level_by_client( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_log_level_by_log_obj.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_log_level_by_log_object( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_log_level_by_user.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_log_level_by_user( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_retention_parameters.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_retention_parameters( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_sub_log_behavior.
    DATA(actual_result) = cut->/usi/if_bal_cust_dao_factory~get_sub_log_behavior( ).
    cl_abap_unit_assert=>assert_bound( actual_result ).
  ENDMETHOD.
ENDCLASS.
