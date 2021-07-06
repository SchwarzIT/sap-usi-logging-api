*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Test-Double for DAO (To inject customizing)
*--------------------------------------------------------------------*
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
        EXPORTING
          textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.

  METHOD set_mock_data.
    mock_data = i_mock_data.
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
          cut                  TYPE REF TO /usi/if_bal_ce_cx_mapper.

    METHODS setup.
    METHODS test_ignore_invalid_mappers    FOR TESTING.
    METHODS test_match_class               FOR TESTING.
    METHODS test_match_new_interface       FOR TESTING.
    METHODS test_match_superclass          FOR TESTING.
    METHODS test_validate_fallback         FOR TESTING.

    METHODS reset_cut
      IMPORTING
        i_quit TYPE aunit_flowctrl DEFAULT cl_aunit_assert=>method.

    METHODS assert_expected_result
      IMPORTING
        i_customizing     TYPE /usi/if_bal_cd_cx_mapper=>ty_records
        i_exception       TYPE REF TO cx_root
        i_expected_result TYPE /usi/bal_exception_mapper.
ENDCLASS.

CLASS lcl_unit_tests IMPLEMENTATION.
  METHOD setup.
    CREATE OBJECT test_double_cust_dao.
    reset_cut( cl_aunit_assert=>class ).
  ENDMETHOD.

  METHOD reset_cut.
    DATA exception TYPE REF TO /usi/cx_bal_root.
    TRY.
        CREATE OBJECT cut TYPE /usi/cl_bal_ce_cx_mapper
          EXPORTING
            i_customizing_dao = test_double_cust_dao.
      CATCH /usi/cx_bal_root INTO exception.
        /usi/cl_bal_aunit_exception=>abort_on_unexpected_exception(
          i_exception = exception
          i_quit      = i_quit
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_ignore_invalid_mappers.
    DATA: test_exception      TYPE REF TO /usi/cx_bal_root,
          customizing_records TYPE /usi/if_bal_cd_cx_mapper=>ty_records,
          customizing_record  TYPE /usi/if_bal_cd_cx_mapper=>ty_record,
          expected_result     TYPE /usi/bal_exception_mapper.

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
      CATCH /usi/cx_bal_root INTO test_exception.
    ENDTRY.

    customizing_record-exception_class = '/USI/CX_BAL_NOT_FOUND'.
    customizing_record-mapper_class    = 'CL_GUI_ALV_GRID'.
    INSERT customizing_record INTO TABLE customizing_records.
    customizing_record-exception_class = '/USI/CX_BAL_ROOT'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_BASE'.
    INSERT customizing_record INTO TABLE customizing_records.
    customizing_record-exception_class = '/USI/CX_EXCEPTION'.
    customizing_record-mapper_class    = '/USI/CL_BAL_UNKNOWN_CLASS'.
    INSERT customizing_record INTO TABLE customizing_records.

    expected_result = cut->get_fallback_mapper_classname( ).

    assert_expected_result(
      i_customizing     = customizing_records
      i_exception       = test_exception
      i_expected_result = expected_result
    ).
  ENDMETHOD.

  METHOD test_match_class.
    DATA: test_exception      TYPE REF TO /usi/cx_bal_root,
          customizing_records TYPE /usi/if_bal_cd_cx_mapper=>ty_records,
          customizing_record  TYPE /usi/if_bal_cd_cx_mapper=>ty_record.

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
      CATCH /usi/cx_bal_root INTO test_exception.
    ENDTRY.

    CLEAR customizing_records.
    customizing_record-exception_class = '/USI/CX_BAL_NOT_FOUND'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_T100'.
    INSERT customizing_record INTO TABLE customizing_records.
    assert_expected_result(
      i_customizing     = customizing_records
      i_exception       = test_exception
      i_expected_result = '/USI/CL_BAL_EM_T100'
    ).

    CLEAR customizing_records.
    customizing_record-exception_class = '/USI/CX_BAL_NOT_FOUND'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_OTR'.
    INSERT customizing_record INTO TABLE customizing_records.
    assert_expected_result(
      i_customizing     = customizing_records
      i_exception       = test_exception
      i_expected_result = '/USI/CL_BAL_EM_OTR'
    ).


    CLEAR customizing_records.
    customizing_record-exception_class = '/USI/CX_EXCEPTION'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_T100'.
    INSERT customizing_record INTO TABLE customizing_records.
    customizing_record-exception_class = 'IF_T100_MESSAGE'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_OTR'.
    INSERT customizing_record INTO TABLE customizing_records.

    assert_expected_result(
      i_customizing     = customizing_records
      i_exception       = test_exception
      i_expected_result = '/USI/CL_BAL_EM_T100'
    ).
  ENDMETHOD.

  METHOD test_match_new_interface.
    DATA: test_exception      TYPE REF TO /usi/cx_bal_root,
          customizing_records TYPE /usi/if_bal_cd_cx_mapper=>ty_records,
          customizing_record  TYPE /usi/if_bal_cd_cx_mapper=>ty_record.

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
      CATCH /usi/cx_bal_root INTO test_exception.
    ENDTRY.

    CLEAR customizing_records.
    customizing_record-exception_class = 'CX_STATIC_CHECK'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_OTR'.
    INSERT customizing_record INTO TABLE customizing_records.
    customizing_record-exception_class = 'IF_T100_MESSAGE'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_T100'.
    INSERT customizing_record INTO TABLE customizing_records.
    assert_expected_result(
      i_customizing     = customizing_records
      i_exception       = test_exception
      i_expected_result = '/USI/CL_BAL_EM_T100'
    ).
  ENDMETHOD.

  METHOD test_match_superclass.
    DATA: test_exception      TYPE REF TO /usi/cx_bal_root,
          customizing_records TYPE /usi/if_bal_cd_cx_mapper=>ty_records,
          customizing_record  TYPE /usi/if_bal_cd_cx_mapper=>ty_record.

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
      CATCH /usi/cx_bal_root INTO test_exception.
    ENDTRY.

    CLEAR customizing_records.
    customizing_record-exception_class = '/USI/CX_BAL_ROOT'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_T100'.
    INSERT customizing_record INTO TABLE customizing_records.
    customizing_record-exception_class = '/USI/CX_EXCEPTION'.
    customizing_record-mapper_class    = '/USI/CL_BAL_EM_OTR'.
    INSERT customizing_record INTO TABLE customizing_records.
    assert_expected_result(
      i_customizing     = customizing_records
      i_exception       = test_exception
      i_expected_result = '/USI/CL_BAL_EM_T100'
    ).
  ENDMETHOD.

  METHOD assert_expected_result.
    DATA: mapper_class_name    TYPE /usi/bal_exception_mapper,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    test_double_cust_dao->set_mock_data( i_customizing ).
    reset_cut( ).

    TRY.
        mapper_class_name = cut->get_exception_mapper_classname( i_exception ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_aunit_assert=>assert_equals(
      act = mapper_class_name
      exp = i_expected_result
    ).
  ENDMETHOD.

  METHOD test_validate_fallback.
    CONSTANTS mapper_interface_name TYPE seoclsname VALUE '/USI/IF_BAL_EXCEPTION_MAPPER'.

    DATA: fallback             TYPE /usi/bal_exception_mapper,
          object_description   TYPE REF TO /usi/cl_bal_object_descr,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    fallback = cut->get_fallback_mapper_classname( ).

    TRY.
        CREATE OBJECT object_description
          EXPORTING
            i_object_type_name = fallback.
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    IF object_description->is_implementing( mapper_interface_name ) NE abap_true OR
       object_description->is_instantiatable( ) NE abap_true.
      cl_aunit_assert=>fail( 'Invalid fallback for mapper class!' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
