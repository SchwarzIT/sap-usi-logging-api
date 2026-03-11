*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Test-Double for DAO (To inject customizing)
" ---------------------------------------------------------------------
CLASS lcl_test_double_cust_dao DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_sub_log_behav.

    METHODS clear_mock_data.

    METHODS insert_mock_data_line
      IMPORTING i_log_object TYPE balobj_d
                i_sub_object TYPE balsubobj
                i_behavior   TYPE /usi/bal_sub_log_behavior.

  PRIVATE SECTION.
    DATA mock_data TYPE /usi/if_bal_cd_sub_log_behav~ty_records.

ENDCLASS.


CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD /usi/if_bal_cd_sub_log_behav~get_records.
    LOOP AT mock_data
         REFERENCE INTO DATA(mock_data_line_dref)
         WHERE     log_object IN i_log_object_range
               AND sub_object IN i_sub_object_range.
      INSERT mock_data_line_dref->* INTO TABLE r_result.
    ENDLOOP.

    IF r_result IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.

  METHOD clear_mock_data.
    CLEAR mock_data.
  ENDMETHOD.

  METHOD insert_mock_data_line.
    INSERT VALUE #( log_object = i_log_object
                    sub_object = i_sub_object
                    behavior   = i_behavior )
           INTO TABLE mock_data.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test
" ---------------------------------------------------------------------
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES ty_non_default_behaviors TYPE STANDARD TABLE OF REF TO /usi/cl_bal_enum_sub_log_behav WITH EMPTY KEY.

    METHODS setup.
    METHODS test_fallback_behavior FOR TESTING.
    METHODS test_prio_log_object   FOR TESTING RAISING /usi/cx_bal_root.
    METHODS test_prio_sub_object   FOR TESTING RAISING /usi/cx_bal_root.

    METHODS get_non_fallback_behaviors
      IMPORTING i_amount        TYPE int1
      RETURNING VALUE(r_result) TYPE ty_non_default_behaviors
      RAISING   /usi/cx_bal_root.

    DATA: cut                  TYPE REF TO /usi/cl_bal_ce_sub_log_behav,
          test_double_cust_dao TYPE REF TO lcl_test_double_cust_dao.
ENDCLASS.


CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    test_double_cust_dao = NEW #( ).
    cut = NEW #( i_customizing_dao = test_double_cust_dao ).
  ENDMETHOD.

  METHOD test_fallback_behavior.
    DATA(expected_result) = cut->get_fallback_behavior( ).
    DATA(acutal_result)   = cut->/usi/if_bal_ce_sub_log_behav~get_sub_log_behavior( i_log_object = 'NOT'
                                                                                    i_sub_object = 'IN_CUST' ).

    cl_abap_unit_assert=>assert_bound( acutal_result ).
    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = acutal_result ).
  ENDMETHOD.

  METHOD test_prio_log_object.
    DATA(non_fallback_behaviors) = get_non_fallback_behaviors( 2 ).
    DATA(expected_result)        = non_fallback_behaviors[ 1 ].
    DATA(wrong_result)           = non_fallback_behaviors[ 2 ].

    test_double_cust_dao->insert_mock_data_line( i_log_object = 'CUST_LOG_OBJECT'
                                                 i_sub_object = space
                                                 i_behavior   = expected_result->value ).
    test_double_cust_dao->insert_mock_data_line( i_log_object = space
                                                 i_sub_object = 'CUST_SUB_OBJECT'
                                                 i_behavior   = wrong_result->value ).

    DATA(actual_result) = cut->/usi/if_bal_ce_sub_log_behav~get_sub_log_behavior( i_log_object = 'CUST_LOG_OBJECT'
                                                                                  i_sub_object = 'CUST_SUB_OBJECT' ).

    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = actual_result ).
  ENDMETHOD.

  METHOD test_prio_sub_object.
    DATA(non_fallback_behaviors) = get_non_fallback_behaviors( 2 ).
    DATA(expected_result)        = non_fallback_behaviors[ 1 ].
    DATA(wrong_result)           = non_fallback_behaviors[ 2 ].

    test_double_cust_dao->insert_mock_data_line( i_log_object = 'CUST_LOG_OBJECT'
                                                 i_sub_object = space
                                                 i_behavior   = wrong_result->value ).
    test_double_cust_dao->insert_mock_data_line( i_log_object = space
                                                 i_sub_object = 'CUST_SUB_OBJECT'
                                                 i_behavior   = expected_result->value ).

    DATA(actual_result) = cut->/usi/if_bal_ce_sub_log_behav~get_sub_log_behavior( i_log_object = 'NOT_IN_CUST'
                                                                                  i_sub_object = 'CUST_SUB_OBJECT' ).

    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = actual_result ).
  ENDMETHOD.

  METHOD get_non_fallback_behaviors.
    DATA(fallback_behavior) = cut->get_fallback_behavior( ).

    DATA(value) = CONV /usi/bal_sub_log_behavior( 0 ).
    WHILE lines( r_result ) < i_amount.
      TRY.
          DATA(behavior) = /usi/cl_bal_enum_sub_log_behav=>get_by_value( value ).
        CATCH /usi/cx_bal_root.
          CLEAR behavior.
      ENDTRY.

      IF     behavior IS BOUND
         AND behavior <> fallback_behavior.
        INSERT behavior INTO TABLE r_result.
      ENDIF.

      value = value + 1.
      IF value = 0.
        EXIT.
      ENDIF.
    ENDWHILE.

    IF lines( r_result ) < i_amount.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>generic_not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
