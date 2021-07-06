*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_cust_eval_factory.

    METHODS setup.
    METHODS test_exception_mapper_eval      FOR TESTING.
    METHODS test_increased_log_level_eval   FOR TESTING.
    METHODS test_message_context_data_eval  FOR TESTING.
    METHODS test_regular_log_level_eval     FOR TESTING.
    METHODS test_retention_parameter_eval   FOR TESTING.
ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    DATA exception TYPE REF TO /usi/cx_bal_root.
    TRY.
        cut = /usi/cl_bal_cust_eval_factory=>get_instance( ).
      CATCH /usi/cx_bal_root INTO exception.
        /usi/cl_bal_aunit_exception=>abort_on_unexpected_exception(
          i_exception = exception
          i_quit      = if_aunit_constants=>class
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_exception_mapper_eval.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_cx_mapper,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_exception_mapper( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_increased_log_level_eval.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_log_lv_by_user,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_log_level_by_user( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_message_context_data_eval.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_data_containers,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_data_containers( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_regular_log_level_eval.
    DATA: actual_result        TYPE REF TO /usi/if_bal_ce_log_lv_by_obj,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        actual_result = cut->get_log_level_by_log_object( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_bound( actual_result ).
  ENDMETHOD.

  METHOD test_retention_parameter_eval.
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
