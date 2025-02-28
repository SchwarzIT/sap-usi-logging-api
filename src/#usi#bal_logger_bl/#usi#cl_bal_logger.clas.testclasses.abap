*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" DAO-Mock: Log data (Does nothing)
" ---------------------------------------------------------------------
CLASS lcl_log_writer_dao_mock DEFINITION FINAL CREATE PUBLIC FOR TESTING.
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


" ---------------------------------------------------------------------
" DAO-Mock: Data container collection (Does nothing)
" ---------------------------------------------------------------------
CLASS lcl_data_cont_coll_dao_mock DEFINITION FINAL CREATE PUBLIC FOR TESTING.
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


" ---------------------------------------------------------------------
" State-Double
" ---------------------------------------------------------------------
CLASS lcl_log_writer_state_double DEFINITION FINAL CREATE PUBLIC FOR TESTING.
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

    METHODS make_next_call_fail.

    METHODS set_token
      IMPORTING i_token TYPE REF TO /usi/if_bal_token.

  PRIVATE SECTION.
    DATA: is_failure_requested TYPE abap_bool,
          token                TYPE REF TO /usi/if_bal_token.

    METHODS fail_if_requested
      RAISING /usi/cx_bal_root.
ENDCLASS.


CLASS lcl_log_writer_state_double IMPLEMENTATION.
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

  METHOD /usi/if_bal_logger_state~display.
    cl_abap_unit_assert=>fail( msg = `Method not implemented!` ).
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
    IF is_failure_requested = abap_true.
      CLEAR is_failure_requested.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
        EXPORTING textid = /usi/cx_bal_not_allowed=>wrong_logger_state.
    ENDIF.
  ENDMETHOD.

  METHOD set_token.
    token = i_token.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Helper class to access private data
" ---------------------------------------------------------------------
CLASS lcl_private_data DEFINITION DEFERRED.
CLASS /usi/cl_bal_logger DEFINITION LOCAL FRIENDS lcl_private_data.

CLASS lcl_private_data DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_logger TYPE REF TO /usi/if_bal_logger.

    METHODS set_state
      IMPORTING i_state TYPE REF TO /usi/if_bal_logger_state.

    METHODS get_state
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_logger_state.

  PRIVATE SECTION.
    DATA logger TYPE REF TO /usi/cl_bal_logger.

ENDCLASS.


CLASS lcl_private_data IMPLEMENTATION.
  METHOD constructor.
    logger = CAST #( i_logger ).
  ENDMETHOD.

  METHOD set_state.
    logger->state = i_state.
  ENDMETHOD.

  METHOD get_state.
    r_result = logger->state.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Delegation
" ---------------------------------------------------------------------
CLASS lcl_unit_test_delegation DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut          TYPE REF TO /usi/if_bal_logger,
          state_double TYPE REF TO lcl_log_writer_state_double.

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
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).
    cut = NEW /usi/cl_bal_logger( i_factory                  = bl_factory
                                  i_relevant_data_containers = VALUE #( )
                                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                  i_auto_save_pckg_size      = 0
                                  i_log_dao                  = NEW lcl_log_writer_dao_mock( )
                                  i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_mock( ) ).

    state_double = NEW #( ).
    NEW lcl_private_data( cut )->set_state( state_double ).
  ENDMETHOD.

  METHOD test_add_exception.
    cut->add_exception( VALUE #( ) ).
    cl_abap_unit_assert=>assert_true( act = state_double->called_methods-add_exception
                                      msg = 'STATE->ADD_EXCEPTION( ) not called!' ).
  ENDMETHOD.

  METHOD test_add_free_text.
    cut->add_free_text( `Just a test` ).
    cl_abap_unit_assert=>assert_true( act = state_double->called_methods-add_free_text
                                      msg = 'STATE->ADD_FREE_TEXT( ) not called!' ).
  ENDMETHOD.

  METHOD test_add_message.
    cut->add_message( i_message_class  = '38'
                      i_message_number = '000' ).
    cl_abap_unit_assert=>assert_true( act = state_double->called_methods-add_message
                                      msg = 'STATE->ADD_MESSAGE( ) not called!' ).
  ENDMETHOD.

  METHOD test_free.
    cut->free( VALUE #( ) ).
    cl_abap_unit_assert=>assert_true( act = state_double->called_methods-free
                                      msg = 'STATE->FREE( ) not called!' ).
  ENDMETHOD.

  METHOD test_claim_ownership.
    cut->claim_ownership( ).
    cl_abap_unit_assert=>assert_true( act = state_double->called_methods-claim_ownership
                                      msg = 'STATE->CLAIM_OWNERSHIP( ) not called!' ).
  ENDMETHOD.

  METHOD test_save.
    cut->save( VALUE #( ) ).
    cl_abap_unit_assert=>assert_true( act = state_double->called_methods-save
                                      msg = 'STATE->SAVE( ) not called!' ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Events
" ---------------------------------------------------------------------
CLASS lcl_unit_test_events DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
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

    dao_mock = NEW #( ).
    dc_coll_dao_mock = NEW #( ).

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).

    cut = NEW /usi/cl_bal_logger( i_factory                  = logger_bl_factory
                                  i_relevant_data_containers = relevant_data_containers
                                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                  i_auto_save_pckg_size      = 0
                                  i_log_dao                  = dao_mock
                                  i_data_cont_coll_dao       = dc_coll_dao_mock ).
    SET HANDLER on_free FOR cut.

    state_double = NEW #( ).
    NEW lcl_private_data( cut )->set_state( state_double ).
  ENDMETHOD.

  METHOD test_raises_on_free_ok.
    cut->free( token ).
    cl_abap_unit_assert=>assert_true( act = event_raised
                                      msg = 'No event raised on free( )!' ).
  ENDMETHOD.

  METHOD test_dont_raise_on_free_error.
    state_double->make_next_call_fail( ).

    cut->free( token ).

    cl_abap_unit_assert=>assert_false( act = event_raised
                                       msg = 'Event raised, though free( ) ran on errors!' ).
  ENDMETHOD.

  METHOD on_free.
    event_raised = abap_true.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: State transitions
" ---------------------------------------------------------------------
CLASS lcl_unit_test_state_transition DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_logger.

    METHODS test_initial_state             FOR TESTING.

    METHODS test_state_trans_add_exception FOR TESTING.
    METHODS test_state_trans_add_free_text FOR TESTING.
    METHODS test_state_trans_add_message   FOR TESTING.
    METHODS test_state_trans_free          FOR TESTING.
    METHODS test_claim_ownership           FOR TESTING.
    METHODS test_state_trans_save          FOR TESTING.

    METHODS test_state_trans_add_cx_error  FOR TESTING.
    METHODS test_state_trans_add_txt_error FOR TESTING.
    METHODS test_state_trans_add_msg_error FOR TESTING.
    METHODS test_state_trans_free_error    FOR TESTING.
    METHODS test_state_trans_token_error   FOR TESTING.
    METHODS test_state_trans_save_error    FOR TESTING.

    METHODS setup.
    METHODS assert_state_is_test_double.
    METHODS assert_state_is_not_claimed.
    METHODS assert_state_is_active.
    METHODS assert_state_is_invalidated.

    METHODS get_current_state
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_logger_state.
ENDCLASS.


CLASS lcl_unit_test_state_transition IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          dao_mock                 TYPE REF TO lcl_log_writer_dao_mock,
          dc_coll_dao_mock         TYPE REF TO lcl_data_cont_coll_dao_mock,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames.

    dao_mock = NEW #( ).
    dc_coll_dao_mock = NEW #( ).
    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).

    cut = NEW /usi/cl_bal_logger( i_factory                  = logger_bl_factory
                                  i_relevant_data_containers = relevant_data_containers
                                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                  i_auto_save_pckg_size      = 0
                                  i_log_dao                  = dao_mock
                                  i_data_cont_coll_dao       = dc_coll_dao_mock ).
  ENDMETHOD.

  METHOD test_initial_state.
    assert_state_is_not_claimed( ).
  ENDMETHOD.

  METHOD test_state_trans_add_exception.
    DATA dummy_exception TYPE REF TO cx_root.

    NEW lcl_private_data( cut )->set_state( NEW lcl_log_writer_state_double( ) ).
    cut->add_exception( dummy_exception ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_free_text.
    NEW lcl_private_data( cut )->set_state( NEW lcl_log_writer_state_double( ) ).
    cut->add_free_text( `Should not change state.` ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_message.
    NEW lcl_private_data( cut )->set_state( NEW lcl_log_writer_state_double( ) ).
    cut->add_message( i_message_class  = '38'
                      i_message_number = '000' ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_free.
    DATA dummy_token TYPE REF TO /usi/if_bal_token.

    NEW lcl_private_data( cut )->set_state( NEW lcl_log_writer_state_double( ) ).
    cut->free( dummy_token ).
    assert_state_is_invalidated( ).
  ENDMETHOD.

  METHOD test_claim_ownership.
    NEW lcl_private_data( cut )->set_state( NEW lcl_log_writer_state_double( ) ).
    cut->claim_ownership( ).
    assert_state_is_active( ).
  ENDMETHOD.

  METHOD test_state_trans_save.
    DATA dummy_token TYPE REF TO /usi/if_bal_token.

    NEW lcl_private_data( cut )->set_state( NEW lcl_log_writer_state_double( ) ).
    cut->save( dummy_token ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_cx_error.
    DATA: dummy_exception TYPE REF TO cx_root,
          state_double    TYPE REF TO lcl_log_writer_state_double.

    state_double = NEW #( ).
    NEW lcl_private_data( cut )->set_state( state_double ).
    state_double->make_next_call_fail( ).
    cut->add_exception( dummy_exception ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_txt_error.
    DATA(state_double) = NEW lcl_log_writer_state_double( ).
    NEW lcl_private_data( cut )->set_state( state_double ).
    state_double->make_next_call_fail( ).
    cut->add_free_text( `Should not change state.` ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_add_msg_error.
    DATA(state_double) = NEW lcl_log_writer_state_double( ).
    NEW lcl_private_data( cut )->set_state( state_double ).

    state_double->make_next_call_fail( ).
    cut->add_message( i_message_class  = '38'
                      i_message_number = '000' ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_free_error.
    DATA dummy_token TYPE REF TO /usi/if_bal_token.

    DATA(state_double) = NEW lcl_log_writer_state_double( ).
    NEW lcl_private_data( cut )->set_state( state_double ).
    state_double->make_next_call_fail( ).
    cut->free( dummy_token ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_token_error.
    DATA(state_double) = NEW lcl_log_writer_state_double( ).
    NEW lcl_private_data( cut )->set_state( state_double ).
    state_double->make_next_call_fail( ).
    cut->claim_ownership( ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD test_state_trans_save_error.
    DATA dummy_token TYPE REF TO /usi/if_bal_token.

    DATA(state_double) = NEW lcl_log_writer_state_double( ).
    NEW lcl_private_data( cut )->set_state( state_double ).
    state_double->make_next_call_fail( ).
    cut->save( dummy_token ).
    assert_state_is_test_double( ).
  ENDMETHOD.

  METHOD assert_state_is_test_double.
    CAST lcl_log_writer_state_double( get_current_state( ) ).
  ENDMETHOD.

  METHOD assert_state_is_not_claimed.
    CAST /usi/cl_bal_lstate_not_claimed( get_current_state( ) ).
  ENDMETHOD.

  METHOD assert_state_is_active.
    CAST /usi/cl_bal_lstate_active( get_current_state( ) ).
  ENDMETHOD.

  METHOD assert_state_is_invalidated.
    CAST /usi/cl_bal_lstate_invalidated( get_current_state( ) ).
  ENDMETHOD.

  METHOD get_current_state.
    r_result = NEW lcl_private_data( cut )->get_state( ).
    cl_abap_unit_assert=>assert_bound( act = r_result
                                       msg = `State is not bound!` ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Tokens
" ---------------------------------------------------------------------
CLASS lcl_unit_test_token DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
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
    dao_mock = NEW #( ).
    dc_coll_dao_mock = NEW #( ).

    cut = NEW /usi/cl_bal_logger( i_factory                  = logger_bl_factory
                                  i_relevant_data_containers = relevant_data_containers
                                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                  i_auto_save_pckg_size      = 0
                                  i_log_dao                  = dao_mock
                                  i_data_cont_coll_dao       = dc_coll_dao_mock ).

    state_double = NEW #( ).
    NEW lcl_private_data( cut )->set_state( state_double ).
  ENDMETHOD.

  METHOD test_token_on_state_exception.
    DATA: secret_token  TYPE REF TO /usi/if_bal_token,
          actual_result TYPE REF TO /usi/if_bal_token.

    secret_token = logger_bl_factory->get_token( ).
    state_double->set_token( secret_token ).
    state_double->make_next_call_fail( ).

    actual_result = cut->claim_ownership( ).

    cl_abap_unit_assert=>assert_bound( act = actual_result
                                       msg = `Should return dummy token!` ).
    IF secret_token = actual_result.
      cl_abap_unit_assert=>fail( `Returns real token on failure!!!` ).
    ENDIF.
  ENDMETHOD.

  METHOD test_token_on_state_ok.
    DATA: expected TYPE REF TO /usi/if_bal_token,
          actual   TYPE REF TO /usi/if_bal_token.

    expected = logger_bl_factory->get_token( ).
    state_double->set_token( expected ).
    actual = cut->claim_ownership( ).

    cl_abap_unit_assert=>assert_equals( exp = expected
                                        act = actual
                                        msg = `Token should come from state in this case!` ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Type conversions
" ---------------------------------------------------------------------
CLASS lcl_unit_test_type_conversions DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut   TYPE REF TO /usi/if_bal_logger,
          token TYPE REF TO /usi/if_bal_token.

    METHODS setup.
    METHODS teardown.

    METHODS test_add_free_text FOR TESTING.
    METHODS test_add_message   FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_type_conversions IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          dao_mock                 TYPE REF TO lcl_log_writer_dao_mock,
          dc_coll_dao_mock         TYPE REF TO lcl_data_cont_coll_dao_mock,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    dao_mock = NEW #( ).
    dc_coll_dao_mock = NEW #( ).

    cut = NEW /usi/cl_bal_logger( i_factory                  = logger_bl_factory
                                  i_relevant_data_containers = relevant_data_containers
                                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                  i_auto_save_pckg_size      = 0
                                  i_log_dao                  = dao_mock
                                  i_data_cont_coll_dao       = dc_coll_dao_mock ).

    token = cut->claim_ownership( ).
  ENDMETHOD.

  METHOD teardown.
    cut->free( token ).
  ENDMETHOD.

  METHOD test_add_free_text.
    CONSTANTS the_text TYPE c LENGTH 50 VALUE 'TEST'.

    " Logger uses type CSEQUENCE, the state uses CHAR200.
    "   => Explicit conversion needed!
    cut->add_free_text( the_text ).
  ENDMETHOD.

  METHOD test_add_message.
    CONSTANTS: message_variable_1 TYPE i      VALUE 42,
               message_variable_2 TYPE char10 VALUE 'Test',
               message_variable_3 TYPE numc4  VALUE 7,
               message_variable_4 TYPE string VALUE `Another test`.

    " Logger uses type SIMPLE, the state uses SYMSGV.
    "   => Explicit conversion needed!
    cut->add_message( i_message_class      = '/USI/BAL'
                      i_message_number     = '000'
                      i_message_variable_1 = message_variable_1
                      i_message_variable_2 = message_variable_2
                      i_message_variable_3 = message_variable_3
                      i_message_variable_4 = message_variable_4 ).
  ENDMETHOD.
ENDCLASS.
