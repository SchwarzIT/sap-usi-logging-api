*"* use this source file for your ABAP unit test classes
CLASS lcl_test_double_cust_dao DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_data_containers.

    METHODS clear_mock_data.

    METHODS insert_mock_data_line
      IMPORTING
        i_log_object    TYPE balobj_d  OPTIONAL
        i_sub_object    TYPE balsubobj OPTIONAL
        i_classname     TYPE /usi/bal_data_cont_classname
        i_min_log_level TYPE REF TO /usi/cl_bal_enum_log_level.

  PRIVATE SECTION.
    DATA mock_data TYPE /usi/if_bal_cd_data_containers=>ty_records.

ENDCLASS.

CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD insert_mock_data_line.
    INSERT VALUE #( log_object    = i_log_object
                    sub_object    = i_sub_object
                    classname     = i_classname
                    min_log_level = i_min_log_level->value )
           INTO TABLE mock_data.
  ENDMETHOD.

  METHOD clear_mock_data.
    CLEAR mock_data.
  ENDMETHOD.

  METHOD /usi/if_bal_cd_data_containers~get_records.
    LOOP AT mock_data
         REFERENCE INTO DATA(mock_data_line_dref)
         WHERE     log_object IN i_log_object_range
               AND sub_object IN i_sub_object_range.
      INSERT mock_data_line_dref->* INTO TABLE r_result.
    ENDLOOP.

    IF r_result IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test
*--------------------------------------------------------------------*
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_request,
             log_object TYPE balobj_d,
             sub_object TYPE balsubobj,
             log_level  TYPE REF TO /usi/cl_bal_enum_log_level,
           END   OF ty_request.

    DATA: test_double_cust_dao TYPE REF TO lcl_test_double_cust_dao,
          cut                  TYPE REF TO /usi/if_bal_ce_data_containers.

    METHODS setup.

    METHODS test_return_empty_on_no_cust FOR TESTING.
    METHODS test_ignore_invalid_classes  FOR TESTING.
    METHODS test_filter_by_log_level     FOR TESTING.
    METHODS test_most_specific_rule_wins FOR TESTING.
    METHODS test_generic_sub_object      FOR TESTING.
    METHODS test_generic_log_object      FOR TESTING.

    METHODS assert_expected_result
      IMPORTING
        i_request         TYPE ty_request
        i_expected_result TYPE /usi/bal_data_cont_classnames.

ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    test_double_cust_dao = NEW #( ).
    cut = NEW /usi/cl_bal_ce_data_containers( test_double_cust_dao ).
  ENDMETHOD.

  METHOD test_return_empty_on_no_cust.
    " given
    test_double_cust_dao->clear_mock_data( ).

    " When / Then
    assert_expected_result( i_request         = VALUE #( log_object = 'DUMMY'
                                                         sub_object = 'VALUE'
                                                         log_level  = /usi/cl_bal_enum_log_level=>everything )
                            i_expected_result = VALUE #( ) ).
  ENDMETHOD.

  METHOD test_ignore_invalid_classes.
    DATA(log_level) = /usi/cl_bal_enum_log_level=>very_important.

    " given
    test_double_cust_dao->insert_mock_data_line( i_classname     = '/USI/IF_BAL_MSG_CDAT'
                                                 i_min_log_level = log_level ).
    test_double_cust_dao->insert_mock_data_line( i_classname     = 'CL_GUI_ALV_GRID'
                                                 i_min_log_level = log_level ).
    test_double_cust_dao->insert_mock_data_line( i_classname     = 'SFLIGHT'
                                                 i_min_log_level = log_level ).

    " When / Then
    assert_expected_result( i_request         = VALUE #( log_object = 'DUMMY'
                                                         sub_object = 'VALUE'
                                                         log_level  = log_level )
                            i_expected_result = VALUE #( ) ).
  ENDMETHOD.

  METHOD test_filter_by_log_level.
    " Given
    test_double_cust_dao->insert_mock_data_line( i_log_object    = space
                                                 i_sub_object    = space
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>very_important ).
    test_double_cust_dao->insert_mock_data_line( i_log_object    = space
                                                 i_sub_object    = space
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CX'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>everything ).

    " When / Then
    assert_expected_result( i_request         = VALUE #( log_object = 'DUMMY'
                                                         sub_object = 'VALUE'
                                                         log_level  = /usi/cl_bal_enum_log_level=>important )
                            i_expected_result = VALUE #( ( CONV #( '/USI/CL_BAL_DC_SRC_POS_CALLER' ) ) ) ).
  ENDMETHOD.

  METHOD test_most_specific_rule_wins.
    " Given
    test_double_cust_dao->insert_mock_data_line( i_log_object    = space
                                                 i_sub_object    = space
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>very_important ).
    test_double_cust_dao->insert_mock_data_line( i_log_object    = 'OBJECT'
                                                 i_sub_object    = space
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>important ).
    test_double_cust_dao->insert_mock_data_line( i_log_object    = 'OBJECT'
                                                 i_sub_object    = 'SUB_OBJECT'
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>medium ).

    " When / Then
    assert_expected_result( i_request         = VALUE #( log_object = 'OBJECT'
                                                         sub_object = 'SUB_OBJECT'
                                                         log_level  = /usi/cl_bal_enum_log_level=>important )
                            i_expected_result = VALUE #( ) ).

    " When / Then
    assert_expected_result( i_request         = VALUE #( log_object = 'OBJECT'
                                                         sub_object = 'SUB_OBJECT'
                                                         log_level  = /usi/cl_bal_enum_log_level=>medium )
                            i_expected_result = VALUE #( ( CONV #( '/USI/CL_BAL_DC_SRC_POS_CALLER' ) ) ) ).
  ENDMETHOD.

  METHOD test_generic_sub_object.
    " Given
    test_double_cust_dao->insert_mock_data_line( i_log_object    = 'CUST_OBJECT'
                                                 i_sub_object    = space
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CX'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>important ).
    test_double_cust_dao->insert_mock_data_line( i_log_object    = 'CUST_OBJECT'
                                                 i_sub_object    = 'CUST_SUB_OBJECT'
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>medium ).

    " When / Then
    assert_expected_result( i_request         = VALUE #( log_object = 'CUST_OBJECT'
                                                         sub_object = 'UNKNOWN_SUB_OBJECT'
                                                         log_level  = /usi/cl_bal_enum_log_level=>everything )
                            i_expected_result = VALUE #( ( CONV #( '/USI/CL_BAL_DC_SRC_POS_CX' ) ) ) ).
  ENDMETHOD.

  METHOD test_generic_log_object.
    " Given
    test_double_cust_dao->insert_mock_data_line( i_log_object    = space
                                                 i_sub_object    = 'CUST_SUB_OBJECT'
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CX'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>important ).
    test_double_cust_dao->insert_mock_data_line( i_log_object    = 'CUST_OBJECT'
                                                 i_sub_object    = 'CUST_SUB_OBJECT'
                                                 i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
                                                 i_min_log_level = /usi/cl_bal_enum_log_level=>medium ).

    " When / Then
    assert_expected_result( i_request         = VALUE #( log_object = 'UNKNOWN_OBJECT'
                                                         sub_object = 'CUST_SUB_OBJECT'
                                                         log_level  = /usi/cl_bal_enum_log_level=>everything )
                            i_expected_result = VALUE #( ( CONV #( '/USI/CL_BAL_DC_SRC_POS_CX' ) ) ) ).
  ENDMETHOD.

  METHOD assert_expected_result.
    DATA actual_result TYPE /usi/bal_data_cont_classnames.

    actual_result = cut->get_relevant_plugin_classnames( i_log_object = i_request-log_object
                                                         i_sub_object = i_request-sub_object
                                                         i_log_level  = i_request-log_level ).

    cl_abap_unit_assert=>assert_equals( exp = i_expected_result
                                        act = actual_result ).
  ENDMETHOD.
ENDCLASS.
