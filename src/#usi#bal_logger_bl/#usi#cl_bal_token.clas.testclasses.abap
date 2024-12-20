*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short

  PRIVATE SECTION.
    METHODS assert_equals_same_instance FOR TESTING.
    METHODS assert_not_equals_others    FOR TESTING.

ENDCLASS.


CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD assert_equals_same_instance.
    DATA(cut) = NEW /usi/cl_bal_token( ).

    DATA(actual_result) = cut->/usi/if_bal_token~is_equal( cut ).

    cl_aunit_assert=>assert_equals( exp = abap_true
                                    act = actual_result
                                    msg = 'Instance does not equal itself!' ).
  ENDMETHOD.

  METHOD assert_not_equals_others.
    DATA(cut)            = NEW /usi/cl_bal_token( ).
    DATA(other_instance) = NEW /usi/cl_bal_token( ).

    DATA(actual_result) = cut->/usi/if_bal_token~is_equal( other_instance ).

    cl_aunit_assert=>assert_equals( exp = abap_false
                                    act = actual_result
                                    msg = 'Two independet instances are considered equal!' ).
  ENDMETHOD.
ENDCLASS.
