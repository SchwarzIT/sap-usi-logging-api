*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
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
    DATA exception TYPE REF TO /usi/cx_bal_root.
    TRY.
        cut = /usi/cl_bal_cust_eval_factory=>get_instance( ).
      CATCH /usi/cx_bal_root INTO exception.
        /usi/cl_bal_aunit_exception=>abort_on_unexpected_exception( i_exception = exception
                                                                    i_quit      = if_aunit_constants=>class ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_data_containers.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_data_containers,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_data_containers( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_exception_mapper.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_cx_mapper,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_exception_mapper( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_log_level_by_client.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_log_lv_by_clnt,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_log_level_by_client( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_log_level_by_log_obj.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_log_lv_by_obj,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_log_level_by_log_object( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_log_level_by_user.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_log_lv_by_user,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_log_level_by_user( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_get_retention_parameters.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_retention,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_retention_parameters( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.
ENDCLASS.
