*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING.
  "#AU Duration   Short
  "#AU Risk_Level Harmless
  PRIVATE SECTION.

    METHODS setup.
    METHODS verify_throws_on_no_cust FOR TESTING.

    DATA cut TYPE REF TO /usi/if_bal_cd_log_lv_by_obj.
    DATA exclude_all_log_objects_range TYPE /usi/bal_log_object_range.

ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.

  METHOD verify_throws_on_no_cust.
    TRY.
        cut->get_records( i_log_object_range = exclude_all_log_objects_range ).
        cl_aunit_assert=>fail( `Should throw exception on no data!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD setup.
    DATA range_line TYPE /usi/bal_log_object_range_line.

    CREATE OBJECT cut TYPE /usi/cl_bal_cd_log_lv_by_obj.

    range_line-sign   = 'E'.
    range_line-option = 'CP'.
    range_line-low    = '*'.
    INSERT range_line INTO TABLE exclude_all_log_objects_range.
  ENDMETHOD.

ENDCLASS.
