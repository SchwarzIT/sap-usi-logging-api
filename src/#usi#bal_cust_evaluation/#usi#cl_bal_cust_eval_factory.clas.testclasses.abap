*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_cust_eval_factory.

    METHODS setup.
    METHODS test_get_data_containers        FOR TESTING.
    METHODS test_get_exception_mapper       FOR TESTING.
    METHODS test_get_log_level_by_client    FOR TESTING.
    METHODS test_get_log_level_by_log_obj   FOR TESTING.
    METHODS test_get_log_level_by_user      FOR TESTING.
    METHODS test_get_retention_parameters   FOR TESTING.
ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    cut = /usi/cl_bal_cust_eval_factory=>get_instance( ).
  ENDMETHOD.

  METHOD test_get_data_containers.
    cl_abap_unit_assert=>assert_bound( cut->get_data_containers( ) ).
  ENDMETHOD.

  METHOD test_get_exception_mapper.
    cl_abap_unit_assert=>assert_bound( cut->get_exception_mapper( ) ).
  ENDMETHOD.

  METHOD test_get_log_level_by_client.
    cl_abap_unit_assert=>assert_bound( cut->get_log_level_by_client( ) ).
  ENDMETHOD.

  METHOD test_get_log_level_by_log_obj.
    cl_abap_unit_assert=>assert_bound( cut->get_log_level_by_log_object( ) ).
  ENDMETHOD.

  METHOD test_get_log_level_by_user.
    cl_abap_unit_assert=>assert_bound( cut->get_log_level_by_user( ) ).
  ENDMETHOD.

  METHOD test_get_retention_parameters.
    cl_abap_unit_assert=>assert_bound( cut->get_retention_parameters( ) ).
  ENDMETHOD.
ENDCLASS.
