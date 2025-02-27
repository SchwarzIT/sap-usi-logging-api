*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS setup.
    METHODS verify_throws_on_no_cust FOR TESTING.

    DATA cut TYPE REF TO /usi/if_bal_cd_log_lv_by_user.

ENDCLASS.


CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD verify_throws_on_no_cust.
    TRY.
        cut->get_records( i_user_name        = sy-uname
                          i_log_object_range = VALUE #( ( sign   = 'E'
                                                          option = 'CP'
                                                          low    = '*' ) ) ).
        cl_abap_unit_assert=>fail( `Should throw exception on no data!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD setup.
    cut = NEW /usi/cl_bal_cd_log_lv_by_user( ).
  ENDMETHOD.
ENDCLASS.
