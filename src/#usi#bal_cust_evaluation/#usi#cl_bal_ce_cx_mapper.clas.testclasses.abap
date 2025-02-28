*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Test-Double for DAO (To inject customizing)
" ---------------------------------------------------------------------
CLASS lcl_test_double_cust_dao DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_cx_mapper.

    METHODS set_mock_data
      IMPORTING
        i_mock_data TYPE /usi/if_bal_cd_cx_mapper=>ty_records.

  PRIVATE SECTION.
    DATA mock_data TYPE /usi/if_bal_cd_cx_mapper=>ty_records.
ENDCLASS.


CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD /usi/if_bal_cd_cx_mapper~get_records.
    r_result = mock_data.
    IF r_result IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.

  METHOD set_mock_data.
    mock_data = i_mock_data.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test
" ---------------------------------------------------------------------
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_request,
        log_object TYPE balobj_d,
        sub_object TYPE balsubobj,
        log_level  TYPE REF TO /usi/cl_bal_enum_log_level,
      END   OF ty_request.

    DATA: test_double_cust_dao TYPE REF TO lcl_test_double_cust_dao,
          cut                  TYPE REF TO /usi/if_bal_ce_cx_mapper.

    METHODS setup.
    METHODS test_ignore_invalid_mappers FOR TESTING.
    METHODS test_match_class            FOR TESTING.
    METHODS test_match_new_interface    FOR TESTING.
    METHODS test_match_superclass       FOR TESTING.
    METHODS test_validate_fallback      FOR TESTING RAISING /usi/cx_bal_root.

    METHODS assert_expected_result
      IMPORTING
        i_customizing     TYPE /usi/if_bal_cd_cx_mapper=>ty_records
        i_exception       TYPE REF TO cx_root
        i_expected_result TYPE /usi/bal_exception_mapper.
ENDCLASS.


CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    test_double_cust_dao = NEW #( ).
    cut = NEW /usi/cl_bal_ce_cx_mapper( i_customizing_dao = test_double_cust_dao ).
  ENDMETHOD.

  METHOD test_ignore_invalid_mappers.
    DATA: test_exception      TYPE REF TO /usi/cx_bal_root,
          customizing_records TYPE /usi/if_bal_cd_cx_mapper=>ty_records,
          expected_result     TYPE /usi/bal_exception_mapper.

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
      CATCH /usi/cx_bal_root INTO test_exception.
    ENDTRY.

    customizing_records = VALUE #( ( exception_class = '/USI/CX_BAL_NOT_FOUND'
                                     mapper_class    = 'CL_GUI_ALV_GRID' )
                                   ( exception_class = '/USI/CX_BAL_ROOT'
                                     mapper_class    = '/USI/CL_BAL_EM_BASE' )
                                   ( exception_class = '/USI/CX_EXCEPTION'
                                     mapper_class    = '/USI/CL_BAL_UNKNOWN_CLASS' ) ).

    expected_result = cut->get_fallback_mapper_classname( ).

    assert_expected_result( i_customizing     = customizing_records
                            i_exception       = test_exception
                            i_expected_result = expected_result ).
  ENDMETHOD.

  METHOD test_match_class.
    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
      CATCH /usi/cx_bal_root INTO DATA(test_exception).
    ENDTRY.
    assert_expected_result( i_customizing     = VALUE #( ( exception_class = '/USI/CX_BAL_NOT_FOUND'
                                                           mapper_class    = '/USI/CL_BAL_EM_BASE' ) )
                            i_exception       = test_exception
                            i_expected_result = '/USI/CL_BAL_EM_BASE' ).
  ENDMETHOD.

  METHOD test_match_new_interface.
    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
      CATCH /usi/cx_bal_root INTO DATA(test_exception).
    ENDTRY.
    assert_expected_result( i_customizing     = VALUE #( ( exception_class = 'IF_T100_MESSAGE'
                                                           mapper_class    = '/USI/CL_BAL_EM_BASE' ) )
                            i_exception       = test_exception
                            i_expected_result = '/USI/CL_BAL_EM_BASE' ).
  ENDMETHOD.

  METHOD test_match_superclass.
    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
      CATCH /usi/cx_bal_root INTO DATA(test_exception).
    ENDTRY.
    assert_expected_result( i_customizing     = VALUE #( ( exception_class = '/USI/CX_BAL_ROOT'
                                                           mapper_class    = '/USI/CL_BAL_EM_BASE' ) )
                            i_exception       = test_exception
                            i_expected_result = '/USI/CL_BAL_EM_BASE' ).
  ENDMETHOD.

  METHOD assert_expected_result.
    test_double_cust_dao->set_mock_data( i_customizing ).

    cl_abap_unit_assert=>assert_equals( exp = i_expected_result
                                        act = cut->get_exception_mapper_classname( i_exception ) ).
  ENDMETHOD.

  METHOD test_validate_fallback.
    CONSTANTS mapper_interface_name TYPE seoclsname VALUE '/USI/IF_BAL_EXCEPTION_MAPPER'.

    DATA(object_description) = NEW /usi/cl_bal_object_descr( cut->get_fallback_mapper_classname( ) ).
    IF    object_description->is_implementing( mapper_interface_name ) = abap_false
       OR object_description->is_instantiatable( ) = abap_false.
      cl_abap_unit_assert=>fail( 'Invalid fallback for mapper class!' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
