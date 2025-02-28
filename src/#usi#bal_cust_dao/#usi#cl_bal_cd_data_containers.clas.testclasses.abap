*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS setup.
    METHODS test_throws_on_no_cust FOR TESTING.

    DATA cut TYPE REF TO /usi/if_bal_cd_data_containers.

ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD test_throws_on_no_cust.
    TRY.
        cut->get_records( i_log_object_range = VALUE #( ( sign   = 'E'
                                                          option = 'CP'
                                                          low    = '*' ) ) ).
        cl_abap_unit_assert=>fail( `Should throw exception on no data!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD setup.
    cut = NEW /usi/cl_bal_cd_data_containers( ).
  ENDMETHOD.
ENDCLASS.
