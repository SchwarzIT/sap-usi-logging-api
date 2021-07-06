*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* DAO-Mock: Log data (Does nothing)
*--------------------------------------------------------------------*
CLASS lcl_log_writer_dao_mock DEFINITION FINAL FOR TESTING CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_log_dao.
ENDCLASS.

CLASS lcl_log_writer_dao_mock IMPLEMENTATION.
  METHOD /usi/if_bal_log_dao~add_message.
    RETURN.
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~free.
    RETURN.
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~save.
    RETURN.
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~get_log_number.
    r_result = '123456'.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* DAO-Mock: Data container collection (Does nothing)
*--------------------------------------------------------------------*
CLASS lcl_data_cont_coll_dao_mock DEFINITION FINAL FOR TESTING CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_cont_coll_dao.
ENDCLASS.

CLASS lcl_data_cont_coll_dao_mock IMPLEMENTATION.
  METHOD /usi/if_bal_data_cont_coll_dao~delete_collections.
    RETURN.
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~get_collection.
    RETURN.
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~insert_collection_into_buffer.
    RETURN.
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~save_buffer_to_db.
    RETURN.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* State-Double
*--------------------------------------------------------------------*
CLASS lcl_log_writer_state_double DEFINITION DEFERRED.
CLASS /usi/cl_bal_logger DEFINITION LOCAL FRIENDS lcl_log_writer_state_double.

CLASS lcl_log_writer_state_double DEFINITION FINAL FOR TESTING CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_logger_state.

    DATA: BEGIN OF called_methods READ-ONLY,
            add_exception   TYPE abap_bool,
            add_free_text   TYPE abap_bool,
            add_message     TYPE abap_bool,
            free            TYPE abap_bool,
            claim_ownership TYPE abap_bool,
            save            TYPE abap_bool,
          END   OF called_methods.

    CLASS-METHODS inject_into
      IMPORTING
        i_log_writer    TYPE REF TO /usi/if_bal_logger
      RETURNING
        VALUE(r_result) TYPE REF TO lcl_log_writer_state_double.

    METHODS make_next_call_fail.

    METHODS set_token
      IMPORTING
        i_token TYPE REF TO /usi/if_bal_token.

  PRIVATE SECTION.
    DATA: is_failure_requested TYPE abap_bool,
          token                TYPE REF TO /usi/if_bal_token.

    METHODS fail_if_requested
      RAISING
        /usi/cx_bal_root.
ENDCLASS.

CLASS lcl_log_writer_state_double IMPLEMENTATION.
  METHOD inject_into.
    DATA log_writer TYPE REF TO /usi/cl_bal_logger.
    log_writer ?= i_log_writer.
    CREATE OBJECT r_result.
    log_writer->state = r_result.
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~add_exception.
    called_methods-add_exception = abap_true.
    fail_if_requested( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~add_free_text.
    called_methods-add_free_text = abap_true.
    fail_if_requested( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~add_message.
    called_methods-add_message = abap_true.
    fail_if_requested( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~free.
    called_methods-free = abap_true.
    fail_if_requested( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~claim_ownership.
    called_methods-claim_ownership = abap_true.
    fail_if_requested( ).
    r_result = token.
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~save.
    called_methods-save = abap_true.
    fail_if_requested( ).
  ENDMETHOD.

  METHOD make_next_call_fail.
    is_failure_requested = abap_true.
  ENDMETHOD.

  METHOD fail_if_requested.
    IF is_failure_requested EQ abap_true.
      CLEAR is_failure_requested.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
        EXPORTING
          textid = /usi/cx_bal_not_allowed=>wrong_logger_state.
    ENDIF.
  ENDMETHOD.

  METHOD set_token.
    token = i_token.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Delegation
*--------------------------------------------------------------------*
CLASS lcl_unit_test_delegation DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut              TYPE REF TO /usi/if_bal_logger,
          dao_mock         TYPE REF TO lcl_log_writer_dao_mock,
          dc_coll_dao_mock TYPE REF TO lcl_data_cont_coll_dao_mock,
          state_double     TYPE REF TO lcl_log_writer_state_double.

    METHODS setup.
    METHODS test_add_exception   FOR TESTING.
    METHODS test_add_free_text   FOR TESTING.
    METHODS test_add_message     FOR TESTING.
    METHODS test_free            FOR TESTING.
    METHODS test_claim_ownership FOR TESTING.
    METHODS test_save            FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_delegation IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames.

    CREATE OBJECT dao_mock.
    CREATE OBJECT dc_coll_dao_mock.
    cust_eval_factory   = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).

    CREATE OBJECT cut TYPE /usi/cl_bal_logger
      EXPORTING
        i_factory                  = logger_bl_factory
        i_relevant_data_containers = relevant_data_containers
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_log_dao                  = dao_mock
        i_data_cont_coll_dao       = dc_coll_dao_mock.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
  ENDMETHOD.

  METHOD test_add_exception.
    DATA exception TYPE REF TO cx_root.

    cut->add_exception( exception ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = state_double->called_methods-add_exception
      msg = 'STATE->ADD_EXCEPTION( ) not called!'
    ).
  ENDMETHOD.

  METHOD test_add_free_text.
    cut->add_free_text( `Just a test` ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = state_double->called_methods-add_free_text
      msg = 'STATE->ADD_FREE_TEXT( ) not called!'
    ).
  ENDMETHOD.

  METHOD test_add_message.
    cut->add_message(
      i_message_class   = '38'
      i_message_number  = '000'
    ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = state_double->called_methods-add_message
      msg = 'STATE->ADD_MESSAGE( ) not called!'
    ).
  ENDMETHOD.

  METHOD test_free.
    DATA token TYPE REF TO /usi/if_bal_token.

    cut->free( token ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = state_double->called_methods-free
      msg = 'STATE->FREE( ) not called!'
    ).
  ENDMETHOD.

  METHOD test_claim_ownership.
    cut->claim_ownership( ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = state_double->called_methods-claim_ownership
      msg = 'STATE->CLAIM_OWNERSHIP( ) not called!'
    ).
  ENDMETHOD.

  METHOD test_save.
    DATA token TYPE REF TO /usi/if_bal_token.

    cut->save( token ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = state_double->called_methods-save
      msg = 'STATE->SAVE( ) not called!'
    ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Events
*--------------------------------------------------------------------*
CLASS lcl_unit_test_events DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut          TYPE REF TO /usi/if_bal_logger,
          event_raised TYPE abap_bool,
          state_double TYPE REF TO lcl_log_writer_state_double,
          token        TYPE REF TO /usi/if_bal_token.

    METHODS test_raises_on_free_ok        FOR TESTING.
    METHODS test_dont_raise_on_free_error FOR TESTING.

    METHODS setup.
    METHODS on_free FOR EVENT instance_invalidated OF /usi/if_bal_logger.
ENDCLASS.

CLASS lcl_unit_test_events IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          dao_mock                 TYPE REF TO lcl_log_writer_dao_mock,
          dc_coll_dao_mock         TYPE REF TO lcl_data_cont_coll_dao_mock,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames.

    CREATE OBJECT dao_mock.
    CREATE OBJECT dc_coll_dao_mock.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).

    CREATE OBJECT cut TYPE /usi/cl_bal_logger
      EXPORTING
        i_factory                  = logger_bl_factory
        i_relevant_data_containers = relevant_data_containers
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_log_dao                  = dao_mock
        i_data_cont_coll_dao       = dc_coll_dao_mock.
    SET HANDLER on_free FOR cut.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
  ENDMETHOD.

  METHOD test_raises_on_free_ok.
    cut->free( token ).

    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = event_raised
      msg = 'No event raised on free( )!'
    ).
  ENDMETHOD.

  METHOD test_dont_raise_on_free_error.
    state_double->make_next_call_fail( ).

    cut->free( token ).

    cl_aunit_assert=>assert_equals(
      exp = abap_false
      act = event_raised
      msg = 'Event raised, though free( ) ran on errors!'
    ).
  ENDMETHOD.

  METHOD on_free.
    event_raised = abap_true.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: State transitions
*--------------------------------------------------------------------*
CLASS lcl_unit_test_state_transition DEFINITION DEFERRED.
CLASS /usi/cl_bal_logger DEFINITION LOCAL FRIENDS lcl_unit_test_state_transition.

CLASS lcl_unit_test_state_transition DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_logger.

    METHODS test_initial_state              FOR TESTING.

    METHODS test_state_trans_add_exception  FOR TESTING.
    METHODS test_state_trans_add_free_text  FOR TESTING.
    METHODS test_state_trans_add_message    FOR TESTING.
    METHODS test_state_trans_free           FOR TESTING.
    METHODS test_claim_ownership            FOR TESTING.
    METHODS test_state_trans_save           FOR TESTING.

    METHODS test_state_trans_add_cx_error   FOR TESTING.
    METHODS test_state_trans_add_txt_error  FOR TESTING.
    METHODS test_state_trans_add_msg_error  FOR TESTING.
    METHODS test_state_trans_free_error     FOR TESTING.
    METHODS test_state_trans_token_error    FOR TESTING.
    METHODS test_state_trans_save_error     FOR TESTING.

    METHODS setup.
    METHODS assert_state_is_test_double.
    METHODS assert_state_is_not_claimed.
    METHODS assert_state_is_active.
    METHODS assert_state_is_invalidated.
    METHODS get_current_state
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_logger_state.
ENDCLASS.

CLASS lcl_unit_test_state_transition IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          dao_mock                 TYPE REF TO lcl_log_writer_dao_mock,
          dc_coll_dao_mock         TYPE REF TO lcl_data_cont_coll_dao_mock,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames.

    CREATE OBJECT dao_mock.
    CREATE OBJECT dc_coll_dao_mock.
    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).

    CREATE OBJECT cut TYPE /usi/cl_bal_logger
      EXPORTING
        i_factory                  = logger_bl_factory
        i_relevant_data_containers = relevant_data_containers
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_log_dao                  = dao_mock
        i_data_cont_coll_dao       = dc_coll_dao_mock.
  ENDMETHOD.

  METHOD test_initial_state.
    assert_state_is_not_claimed( ).
  ENDMETHOD.

  METHOD test_state_trans_add_exception.
    DATA dummy_exception TYPE REF TO cx_root.

    lcl_log_writer_state_double=>inject_into( cut ).
    cut->add_exception( dummy_exception ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_free_text.
    lcl_log_writer_state_double=>inject_into( cut ).
    cut->add_free_text( `Should not change state.` ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_message.
    lcl_log_writer_state_double=>inject_into( cut ).
    cut->add_message(
      i_message_class   = '38'
      i_message_number  = '000'
    ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_free.
    DATA dummy_token TYPE REF TO /usi/if_bal_token.

    lcl_log_writer_state_double=>inject_into( cut ).
    cut->free( dummy_token ).
    assert_state_is_invalidated( ).
  ENDMETHOD.

  METHOD test_claim_ownership.
    lcl_log_writer_state_double=>inject_into( cut ).
    cut->claim_ownership( ).
    assert_state_is_active( ).
  ENDMETHOD.

  METHOD test_state_trans_save.
    DATA dummy_token TYPE REF TO /usi/if_bal_token.

    lcl_log_writer_state_double=>inject_into( cut ).
    cut->save( dummy_token ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_cx_error.
    DATA: dummy_exception TYPE REF TO cx_root,
          state_double    TYPE REF TO lcl_log_writer_state_double.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
    state_double->make_next_call_fail( ).
    cut->add_exception( dummy_exception ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_txt_error.
    DATA state_double TYPE REF TO lcl_log_writer_state_double.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
    state_double->make_next_call_fail( ).
    cut->add_free_text( `Should not change state.` ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_msg_error.
    DATA state_double TYPE REF TO lcl_log_writer_state_double.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
    state_double->make_next_call_fail( ).
    cut->add_message(
      i_message_class   = '38'
      i_message_number  = '000'
    ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_free_error.
    DATA: dummy_token  TYPE REF TO /usi/if_bal_token,
          state_double TYPE REF TO lcl_log_writer_state_double.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
    state_double->make_next_call_fail( ).
    cut->free( dummy_token ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_token_error.
    DATA state_double TYPE REF TO lcl_log_writer_state_double.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
    state_double->make_next_call_fail( ).
    cut->claim_ownership( ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_save_error.
    DATA: dummy_token  TYPE REF TO /usi/if_bal_token,
          state_double TYPE REF TO lcl_log_writer_state_double.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
    state_double->make_next_call_fail( ).
    cut->save( dummy_token ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD assert_state_is_test_double.
    DATA: requested_state      TYPE REF TO lcl_log_writer_state_double,
          unexpected_exception TYPE REF TO cx_sy_move_cast_error.

    TRY.
        requested_state ?= get_current_state( ).
      CATCH cx_sy_move_cast_error INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_state_is_not_claimed.
    DATA: requested_state      TYPE REF TO /usi/cl_bal_lstate_not_claimed,
          unexpected_exception TYPE REF TO cx_sy_move_cast_error.

    TRY.
        requested_state ?= get_current_state( ).
      CATCH cx_sy_move_cast_error INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_state_is_active.
    DATA: requested_state      TYPE REF TO /usi/cl_bal_lstate_active,
          unexpected_exception TYPE REF TO cx_sy_move_cast_error.

    TRY.
        requested_state ?= get_current_state( ).
      CATCH cx_sy_move_cast_error INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_state_is_invalidated.
    DATA: requested_state      TYPE REF TO /usi/cl_bal_lstate_invalidated,
          unexpected_exception TYPE REF TO cx_sy_move_cast_error.

    TRY.
        requested_state ?= get_current_state( ).
      CATCH cx_sy_move_cast_error INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_current_state.
    DATA log_writer TYPE REF TO /usi/cl_bal_logger.

    log_writer ?= cut.
    r_result = log_writer->state.

    cl_aunit_assert=>assert_bound(
      act = r_result
      msg = `State is not bound!`
    ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Tokens
*--------------------------------------------------------------------*
CLASS lcl_unit_test_token DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut               TYPE REF TO /usi/if_bal_logger,
          logger_bl_factory TYPE REF TO /usi/if_bal_logger_bl_factory,
          state_double      TYPE REF TO lcl_log_writer_state_double.

    METHODS setup.

    METHODS test_token_on_state_ok        FOR TESTING.
    METHODS test_token_on_state_exception FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_token IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          dao_mock                 TYPE REF TO lcl_log_writer_dao_mock,
          dc_coll_dao_mock         TYPE REF TO lcl_data_cont_coll_dao_mock,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    CREATE OBJECT dao_mock.
    CREATE OBJECT dc_coll_dao_mock.

    CREATE OBJECT cut TYPE /usi/cl_bal_logger
      EXPORTING
        i_factory                  = logger_bl_factory
        i_relevant_data_containers = relevant_data_containers
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_log_dao                  = dao_mock
        i_data_cont_coll_dao       = dc_coll_dao_mock.

    state_double = lcl_log_writer_state_double=>inject_into( cut ).
  ENDMETHOD.

  METHOD test_token_on_state_exception.
    DATA: secret_token  TYPE REF TO /usi/if_bal_token,
          actual_result TYPE REF TO /usi/if_bal_token.

    secret_token = logger_bl_factory->get_token( ).
    state_double->set_token( secret_token ).
    state_double->make_next_call_fail( ).

    actual_result = cut->claim_ownership( ).

    cl_aunit_assert=>assert_bound(
      act = actual_result
      msg = `Should return dummy token!`
    ).
    IF secret_token EQ actual_result.
      cl_aunit_assert=>fail( `Returns real token on failure!!!` ).
    ENDIF.
  ENDMETHOD.

  METHOD test_token_on_state_ok.
    DATA: expected TYPE REF TO /usi/if_bal_token,
          actual   TYPE REF TO /usi/if_bal_token.

    expected = logger_bl_factory->get_token( ).
    state_double->set_token( expected ).
    actual = cut->claim_ownership( ).

    cl_aunit_assert=>assert_equals(
      exp = expected
      act = actual
      msg = `Token should come from state in this case!`
    ).
  ENDMETHOD.
ENDCLASS.
