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
    ALIASES: ty_records FOR /usi/if_bal_cd_data_containers~ty_records,
             ty_record  FOR /usi/if_bal_cd_data_containers~ty_record.

    DATA mock_data TYPE /usi/if_bal_cd_data_containers=>ty_records.
ENDCLASS.

CLASS lcl_test_double_cust_dao IMPLEMENTATION.
  METHOD insert_mock_data_line.
    DATA mock_data_line TYPE ty_record.
    mock_data_line-log_object     = i_log_object.
    mock_data_line-sub_object     = i_sub_object.
    mock_data_line-classname      = i_classname.
    mock_data_line-min_log_level  = i_min_log_level->value.
    INSERT mock_data_line INTO TABLE mock_data.
  ENDMETHOD.

  METHOD clear_mock_data.
    CLEAR mock_data.
  ENDMETHOD.

  METHOD /usi/if_bal_cd_data_containers~get_records.
    DATA mock_data_line_dref TYPE REF TO ty_record.
    LOOP AT mock_data
      REFERENCE INTO mock_data_line_dref
      WHERE log_object    IN i_log_object_range
        AND sub_object    IN i_sub_object_range.
      INSERT mock_data_line_dref->* INTO TABLE r_result.
    ENDLOOP.

    IF r_result IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test
*--------------------------------------------------------------------*
CLASS lcl_unit_tests DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_request,
        log_object TYPE balobj_d,
        sub_object TYPE balsubobj,
        log_level  TYPE REF TO /usi/cl_bal_enum_log_level,
      END   OF ty_request.

    DATA: test_double_cust_dao TYPE REF TO lcl_test_double_cust_dao,
          cut                  TYPE REF TO /usi/if_bal_ce_data_containers.

    METHODS setup.

    METHODS test_return_empty_on_no_cust  FOR TESTING.
    METHODS test_ignore_invalid_classes   FOR TESTING.
    METHODS test_filter_by_log_level      FOR TESTING.
    METHODS test_most_specific_rule_wins  FOR TESTING.
    METHODS test_generic_sub_object       FOR TESTING.
    METHODS test_generic_log_object       FOR TESTING.

    METHODS assert_expected_result
      IMPORTING
        i_request         TYPE ty_request
        i_expected_result TYPE /usi/bal_data_cont_classnames.

    METHODS reset_cut.
ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT test_double_cust_dao.
    reset_cut( ).
  ENDMETHOD.

  METHOD reset_cut.
    DATA exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        CREATE OBJECT cut TYPE /usi/cl_bal_ce_data_containers
          EXPORTING
            i_customizing_dao = test_double_cust_dao.
      CATCH /usi/cx_bal_root INTO exception.
        /usi/cl_bal_aunit_exception=>abort_on_unexpected_exception( exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_return_empty_on_no_cust.
    DATA: request      TYPE ty_request,
          empty_result TYPE /usi/bal_data_cont_classnames.

    " given
    test_double_cust_dao->clear_mock_data( ).

    " when
    request-log_object = 'DUMMY'.
    request-sub_object = 'VALUE'.
    request-log_level  = /usi/cl_bal_enum_log_level=>everything.

    " then
    assert_expected_result(
      i_request         = request
      i_expected_result = empty_result
    ).
  ENDMETHOD.

  METHOD test_ignore_invalid_classes.
    DATA: log_level    TYPE REF TO /usi/cl_bal_enum_log_level,
          request      TYPE ty_request,
          empty_result TYPE /usi/bal_data_cont_classnames.

    log_level = /usi/cl_bal_enum_log_level=>very_important.

    " given
    test_double_cust_dao->insert_mock_data_line(
      i_classname     = '/USI/IF_BAL_MSG_CDAT'
      i_min_log_level = log_level
    ).
    test_double_cust_dao->insert_mock_data_line(
      i_classname     = 'CL_GUI_ALV_GRID'
      i_min_log_level = log_level
    ).
    test_double_cust_dao->insert_mock_data_line(
      i_classname     = 'SFLIGHT'
      i_min_log_level = log_level
    ).

    " when
    request-log_object = 'DUMMY'.
    request-sub_object = 'VALUE'.
    request-log_level  = log_level.

    " then
    assert_expected_result(
      i_request         = request
      i_expected_result = empty_result
    ).
  ENDMETHOD.

  METHOD test_filter_by_log_level.
    DATA: data_container_classname TYPE /usi/bal_data_cont_classname,
          expected_result          TYPE /usi/bal_data_cont_classnames,
          log_level                TYPE REF TO /usi/cl_bal_enum_log_level,
          request                  TYPE ty_request.

    " Given
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = space
      i_sub_object    = space
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
      i_min_log_level = /usi/cl_bal_enum_log_level=>very_important
    ).
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = space
      i_sub_object    = space
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CX'
      i_min_log_level = /usi/cl_bal_enum_log_level=>everything
    ).

    " When
    request-log_object = 'DUMMY'.
    request-sub_object = 'VALUE'.
    request-log_level  = /usi/cl_bal_enum_log_level=>important.

    " Then
    CLEAR expected_result.
    data_container_classname = '/USI/CL_BAL_DC_SRC_POS_CALLER'.
    INSERT data_container_classname INTO TABLE expected_result.
    assert_expected_result(
      i_request         = request
      i_expected_result = expected_result
    ).
  ENDMETHOD.

  METHOD test_most_specific_rule_wins.
    DATA: data_container_classname TYPE /usi/bal_data_cont_classname,
          expected_result          TYPE /usi/bal_data_cont_classnames,
          request                  TYPE ty_request.

    " Given
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = space
      i_sub_object    = space
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
      i_min_log_level = /usi/cl_bal_enum_log_level=>very_important
    ).
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = 'OBJECT'
      i_sub_object    = space
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
      i_min_log_level = /usi/cl_bal_enum_log_level=>important
    ).
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = 'OBJECT'
      i_sub_object    = 'SUB_OBJECT'
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
      i_min_log_level = /usi/cl_bal_enum_log_level=>medium
    ).

    " When
    request-log_object = 'OBJECT'.
    request-sub_object = 'SUB_OBJECT'.
    request-log_level  = /usi/cl_bal_enum_log_level=>important.

    " Then
    CLEAR expected_result.
    assert_expected_result(
      i_request         = request
      i_expected_result = expected_result
    ).

    " When
    request-log_object = 'OBJECT'.
    request-sub_object = 'SUB_OBJECT'.
    request-log_level  = /usi/cl_bal_enum_log_level=>medium.

    " Then
    CLEAR expected_result.
    data_container_classname = '/USI/CL_BAL_DC_SRC_POS_CALLER'.
    INSERT data_container_classname INTO TABLE expected_result.
    assert_expected_result(
      i_request         = request
      i_expected_result = expected_result
    ).
  ENDMETHOD.

  METHOD test_generic_sub_object.
    DATA: data_container_classname TYPE /usi/bal_data_cont_classname,
          expected_result          TYPE /usi/bal_data_cont_classnames,
          request                  TYPE ty_request.

    " Given
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = 'CUST_OBJECT'
      i_sub_object    = space
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CX'
      i_min_log_level = /usi/cl_bal_enum_log_level=>important
    ).
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = 'CUST_OBJECT'
      i_sub_object    = 'CUST_SUB_OBJECT'
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
      i_min_log_level = /usi/cl_bal_enum_log_level=>medium
    ).

    " When
    request-log_object = 'CUST_OBJECT'.
    request-sub_object = 'UNKNOWN_SUB_OBJECT'.
    request-log_level  = /usi/cl_bal_enum_log_level=>everything.

    " Then
    CLEAR expected_result.
    data_container_classname = '/USI/CL_BAL_DC_SRC_POS_CX'.
    INSERT data_container_classname INTO TABLE expected_result.
    assert_expected_result(
      i_request         = request
      i_expected_result = expected_result
    ).
  ENDMETHOD.

  METHOD test_generic_log_object.
    DATA: data_container_classname TYPE /usi/bal_data_cont_classname,
          expected_result          TYPE /usi/bal_data_cont_classnames,
          request                  TYPE ty_request.

    " Given
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = space
      i_sub_object    = 'CUST_SUB_OBJECT'
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CX'
      i_min_log_level = /usi/cl_bal_enum_log_level=>important
    ).
    test_double_cust_dao->insert_mock_data_line(
      i_log_object    = 'CUST_OBJECT'
      i_sub_object    = 'CUST_SUB_OBJECT'
      i_classname     = '/USI/CL_BAL_DC_SRC_POS_CALLER'
      i_min_log_level = /usi/cl_bal_enum_log_level=>medium
    ).

    " When
    request-log_object = 'UNKNOWN_OBJECT'.
    request-sub_object = 'CUST_SUB_OBJECT'.
    request-log_level  = /usi/cl_bal_enum_log_level=>everything.

    " Then
    CLEAR expected_result.
    data_container_classname = '/USI/CL_BAL_DC_SRC_POS_CX'.
    INSERT data_container_classname INTO TABLE expected_result.
    assert_expected_result(
      i_request         = request
      i_expected_result = expected_result
    ).
  ENDMETHOD.

  METHOD assert_expected_result.
    DATA actual_result TYPE /usi/bal_data_cont_classnames.

    actual_result = cut->get_relevant_plugin_classnames(
                      i_log_object = i_request-log_object
                      i_sub_object = i_request-sub_object
                      i_log_level  = i_request-log_level
                    ).

    cl_aunit_assert=>assert_equals(
      act = actual_result
      exp = i_expected_result
    ).
  ENDMETHOD.

ENDCLASS.
