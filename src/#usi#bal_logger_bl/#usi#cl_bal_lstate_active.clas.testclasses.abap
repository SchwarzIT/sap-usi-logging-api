*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Test-Double for Log-DAO
" ---------------------------------------------------------------------
CLASS lcl_dao_spy DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_log_dao.

    CONSTANTS: BEGIN OF method_names,
                 add_message    TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'ADD_MESSAGE',
                 free           TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'FREE',
                 get_log_number TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'GET_LOG_NUMBER',
                 save           TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'SAVE',
               END   OF method_names.

    DATA method_calls TYPE REF TO /usi/cl_bal_aunit_method_calls READ-ONLY.

    METHODS constructor.

    METHODS get_message
      IMPORTING i_nth_call      TYPE int4
      RETURNING VALUE(r_result) TYPE bal_s_msg.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_parameter_names,
                 i_message TYPE /usi/cl_bal_aunit_method_call=>ty_parameter_name VALUE 'I_MESSAGE',
               END   OF c_parameter_names.

ENDCLASS.


CLASS lcl_dao_spy IMPLEMENTATION.
  METHOD constructor.
    method_calls = NEW #( ).
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~add_message.
    method_calls->insert_method_call( method_names-add_message )->add_parameter(
                                                                   i_parameter_name  = c_parameter_names-i_message
                                                                   i_parameter_value = i_message ).
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~free.
    method_calls->insert_method_call( method_names-free ).
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~save.
    method_calls->insert_method_call( method_names-save ).
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~get_log_number.
    method_calls->insert_method_call( method_names-get_log_number ).
    r_result = '123456'.
  ENDMETHOD.

  METHOD get_message.
    DATA(all_method_calls) = method_calls->get_method_calls( method_names-add_message ).
    TRY.
        DATA(relevant_method_call) = all_method_calls[ i_nth_call ].
      CATCH cx_sy_itab_line_not_found.
        cl_abap_unit_assert=>fail( msg = |Method call { method_names-add_message }[{ i_nth_call }] was not traced!| ).
    ENDTRY.
    relevant_method_call-method_call->get_parameter( EXPORTING i_parameter_name  = c_parameter_names-i_message
                                                     IMPORTING e_parameter_value = r_result ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Test-Double for Data-container-collection-DAO
" ---------------------------------------------------------------------
CLASS lcl_data_cont_coll_dao_spy DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_cont_coll_dao.

    CONSTANTS:
      BEGIN OF method_names,
        delete_collection  TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'DELETE_COLLECTION',
        get_collection     TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'GET_COLLECTION',
        insert_into_buffer TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'INSERT_COLLECTION_INTO_BUFFER',
        save_buffer_to_db  TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'SAVE_BUFFER_TO_DB',
      END   OF method_names.

    CONSTANTS:
      BEGIN OF parameter_names,
        log_number            TYPE /usi/cl_bal_aunit_method_call=>ty_parameter_name VALUE 'I_LOG_NUMBER',
        message_number        TYPE /usi/cl_bal_aunit_method_call=>ty_parameter_name VALUE 'I_MESSAGE_NUMBER',
        serialized_collection TYPE /usi/cl_bal_aunit_method_call=>ty_parameter_name VALUE 'I_SERIALIZED_COLLECTION',
      END   OF parameter_names.

    DATA method_calls TYPE REF TO /usi/cl_bal_aunit_method_calls READ-ONLY.

    METHODS constructor.
ENDCLASS.


CLASS lcl_data_cont_coll_dao_spy IMPLEMENTATION.
  METHOD constructor.
    method_calls = NEW #( ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~delete_collections.
    method_calls->insert_method_call( method_names-delete_collection )->add_parameter(
                                                                         i_parameter_name  = 'I_LOG_NUMBER'
                                                                         i_parameter_value = i_log_numbers ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~get_collection.
    DATA(method_call) = method_calls->insert_method_call( method_names-get_collection ).
    method_call->add_parameter( i_parameter_name  = 'I_LOG_NUMBER'
                                i_parameter_value = i_log_number ).
    method_call->add_parameter( i_parameter_name  = 'I_MESSAGE_NUMBER'
                                i_parameter_value = i_message_number ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~insert_collection_into_buffer.
    DATA(method_call) = method_calls->insert_method_call( method_names-insert_into_buffer ).
    method_call->add_parameter( i_parameter_name  = 'I_LOG_NUMBER'
                                i_parameter_value = i_log_number ).
    method_call->add_parameter( i_parameter_name  = 'I_MESSAGE_NUMBER'
                                i_parameter_value = i_message_number ).
    method_call->add_parameter( i_parameter_name  = 'I_SERIALIZED_COLLECTION'
                                i_parameter_value = i_serialized_data_cont_coll ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~save_buffer_to_db.
    method_calls->insert_method_call( method_names-save_buffer_to_db ).
  ENDMETHOD.
ENDCLASS.

" ---------------------------------------------------------------------
" Helper-class to access private components of CUT
" ---------------------------------------------------------------------
CLASS lcl_private_data DEFINITION DEFERRED.
CLASS /usi/cl_bal_lstate_active DEFINITION LOCAL FRIENDS lcl_private_data.

CLASS lcl_private_data DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_cut TYPE REF TO /usi/if_bal_logger_state.

    METHODS get_message_counter
      RETURNING VALUE(r_result) TYPE int4.

    METHODS get_data_container_collection
      IMPORTING i_message_number TYPE /usi/bal_message_number
      RETURNING VALUE(r_result)  TYPE REF TO /usi/if_bal_data_container_col.

  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/cl_bal_lstate_active.
ENDCLASS.


CLASS lcl_private_data IMPLEMENTATION.
  METHOD constructor.
    TRY.
        cut ?= i_cut.
      CATCH cx_sy_move_cast_error INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_message_counter.
    r_result = cut->messages-highest_message_number.
  ENDMETHOD.

  METHOD get_data_container_collection.
    IF cut->messages-highest_message_number < i_message_number.
      cl_abap_unit_assert=>fail( msg = |Message { i_message_number } was not created yet!| ).
    ENDIF.

    TRY.
        r_result = cut->messages-unsaved_data-data_container_collections[ i_message_number ]-data_container_collection.
      CATCH cx_sy_itab_line_not_found.
        r_result = NEW /usi/cl_bal_dc_collection( ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Helper class: Data container factory
" ---------------------------------------------------------------------
CLASS lcl_data_container_factory DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS get_data_container_collection
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container_col.

    CLASS-METHODS get_callstack_container
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_html_container
      IMPORTING i_title          TYPE /usi/if_bal_text_container_c40=>ty_text OPTIONAL
                i_paragraph_text TYPE string
      RETURNING VALUE(r_result)  TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_itab_container
      IMPORTING i_title         TYPE /usi/if_bal_text_container_c40=>ty_text DEFAULT 'Test'
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_retcode_msg_container
      IMPORTING i_return_code   TYPE sysubrc DEFAULT 4
                i_text          TYPE symsgv  DEFAULT 'Test'
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_src_pos_caller_container
      IMPORTING i_program_name  TYPE syrepid DEFAULT 'FOO'
                i_include_name  TYPE include DEFAULT 'BAR'
                i_source_line   TYPE int4    DEFAULT 42
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_src_pos_cx_container
      IMPORTING i_program_name  TYPE syrepid DEFAULT 'FOO'
                i_include_name  TYPE include DEFAULT 'BAR'
                i_source_line   TYPE int4    DEFAULT 42
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.
ENDCLASS.


CLASS lcl_data_container_factory IMPLEMENTATION.
  METHOD get_data_container_collection.
    r_result = NEW /usi/cl_bal_dc_collection( ).
  ENDMETHOD.

  METHOD get_callstack_container.
    DATA callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING callstack = callstack.

    r_result = NEW /usi/cl_bal_dc_callstack( i_callstack = callstack ).
  ENDMETHOD.

  METHOD get_html_container.
    r_result = NEW /usi/cl_bal_dc_html( i_html_document  = |<html><head/><body><p>{
                                                             cl_abap_dyn_prg=>escape_xss_xml_html( i_paragraph_text )
                                                           }</p></body></html>|
                                        i_document_title = COND #( WHEN i_title IS NOT INITIAL
                                                                   THEN NEW /usi/cl_bal_tc_literal_c40( i_title ) ) ).
  ENDMETHOD.

  METHOD get_itab_container.
    TRY.
        r_result = NEW /usi/cl_bal_dc_itab(
                           i_internal_table = VALUE string_table( ( `Just` )
                                                                  ( `a`    )
                                                                  ( `test` ) )
                           i_title          = COND #( WHEN i_title IS NOT INITIAL
                                                      THEN NEW /usi/cl_bal_tc_literal_c40( i_title ) ) ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_retcode_msg_container.
    r_result = NEW /usi/cl_bal_dc_retcode_and_msg( i_message     = VALUE #( msgid = '38'
                                                                            msgno = '000'
                                                                            msgty = 'E'
                                                                            msgv1 = i_text )
                                                   i_return_code = i_return_code ).
  ENDMETHOD.

  METHOD get_src_pos_caller_container.
    r_result = NEW /usi/cl_bal_dc_src_pos_caller( VALUE #( program_name = i_program_name
                                                           include_name = i_include_name
                                                           source_line  = i_source_line ) ).
  ENDMETHOD.

  METHOD get_src_pos_cx_container.
    r_result = NEW /usi/cl_bal_dc_src_pos_cx( VALUE #( program_name = i_program_name
                                                       include_name = i_include_name
                                                       source_line  = i_source_line ) ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Helper class: Exception factory
" ---------------------------------------------------------------------
CLASS lcl_exception_factory DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS get_exception
      IMPORTING i_text          TYPE symsgv                           DEFAULT 'Test exception'
                i_previous      TYPE REF TO cx_root                   OPTIONAL
                i_details       TYPE REF TO /usi/if_exception_details OPTIONAL
      RETURNING VALUE(r_result) TYPE REF TO /usi/cx_bal_root.
ENDCLASS.


CLASS lcl_exception_factory IMPLEMENTATION.
  METHOD get_exception.
    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found
          EXPORTING textid   = /usi/cx_bal_not_found=>free_text
                    param1   = i_text
                    previous = i_previous
                    details  = i_details.
      CATCH /usi/cx_bal_root INTO r_result.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Delegation
" ---------------------------------------------------------------------
CLASS lcl_unit_test_dao_delegation DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut             TYPE REF TO /usi/if_bal_logger_state,
          log_dao_spy     TYPE REF TO lcl_dao_spy,
          dc_coll_dao_spy TYPE REF TO lcl_data_cont_coll_dao_spy,
          token           TYPE REF TO /usi/if_bal_token.

    METHODS setup.
    METHODS test_free                    FOR TESTING.
    METHODS test_save                    FOR TESTING.
    METHODS test_skip_save_for_empty_log FOR TESTING.
    METHODS test_save_multiple_times     FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_dao_delegation IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).

    token           = bl_factory->get_token( ).
    log_dao_spy     = NEW #( ).
    dc_coll_dao_spy = NEW #( ).

    cut = NEW /usi/cl_bal_lstate_active(
                  i_factory                  = bl_factory
                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                  i_auto_save_pckg_size      = 0
                  i_log_dao                  = log_dao_spy
                  i_data_cont_coll_dao       = dc_coll_dao_spy
                  i_token                    = token
                  i_relevant_data_containers = VALUE #(
                      ( /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) ) ) ).
  ENDMETHOD.

  METHOD test_free.
    TRY.
        cut->free( token ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    log_dao_spy->method_calls->assert_method_was_called( log_dao_spy->method_names-free ).
  ENDMETHOD.

  METHOD test_save.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = 'Just a test' ).
        cut->save( token ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    " Log-DAO (Saves the log)
    log_dao_spy->method_calls->assert_method_was_called( log_dao_spy->method_names-add_message ).
    log_dao_spy->method_calls->assert_method_was_called( log_dao_spy->method_names-save ).

    " Data-Container-Collection-DAO (Saves data container collections)
    dc_coll_dao_spy->method_calls->assert_method_was_called( dc_coll_dao_spy->method_names-insert_into_buffer ).
    dc_coll_dao_spy->method_calls->assert_method_was_called( dc_coll_dao_spy->method_names-save_buffer_to_db ).
  ENDMETHOD.

  METHOD test_skip_save_for_empty_log.
    TRY.
        cut->save( token ).
        cl_abap_unit_assert=>fail( msg = 'Should throw an exception, if no unsaved messages exist' ).
      CATCH /usi/cx_bal_root.
        " This was supposed to happen since the log was empty
        TRY.
            cut->free( token ).
          CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
            /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
        ENDTRY.
    ENDTRY.

    " Log-DAO (Saves the log)
    log_dao_spy->method_calls->assert_method_was_not_called( log_dao_spy->method_names-add_message ).
    log_dao_spy->method_calls->assert_method_was_not_called( log_dao_spy->method_names-save ).

    " Data-Container-Collection-DAO (Saves data container collections)
    dc_coll_dao_spy->method_calls->assert_method_was_not_called( dc_coll_dao_spy->method_names-insert_into_buffer ).
    dc_coll_dao_spy->method_calls->assert_method_was_not_called( dc_coll_dao_spy->method_names-save_buffer_to_db ).
  ENDMETHOD.

  METHOD test_save_multiple_times.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = `Test` ).
        cut->save( token ).

        TRY.
            cut->save( token ).
            cl_abap_unit_assert=>fail( msg = 'Should throw an exception, if no unsaved messages exist' ).
          CATCH /usi/cx_bal_root.
            " This was supposed to happen, as there were no unsaved changes
        ENDTRY.

        log_dao_spy->method_calls->assert_method_called_n_times(
            i_method_name              = log_dao_spy->method_names-add_message
            i_expected_number_of_calls = 1 ).
        log_dao_spy->method_calls->assert_method_called_n_times(
            i_method_name              = log_dao_spy->method_names-save
            i_expected_number_of_calls = 1 ).

        dc_coll_dao_spy->method_calls->assert_method_called_n_times(
            i_method_name              = dc_coll_dao_spy->method_names-insert_into_buffer
            i_expected_number_of_calls = 1 ).
        dc_coll_dao_spy->method_calls->assert_method_called_n_times(
            i_method_name              = dc_coll_dao_spy->method_names-save_buffer_to_db
            i_expected_number_of_calls = 1 ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Test messages without data containers (Simple Calls)
" ---------------------------------------------------------------------
" The test has to ensure, that calls to the ADD_*-Methods are accepted
" by cut (The active state of the logger is supposed to accept them).
" ---------------------------------------------------------------------
CLASS lcl_unit_test_messages_wo_dcs DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_logger_state.

    METHODS setup.
    METHODS test_add_exception FOR TESTING.
    METHODS test_add_free_text FOR TESTING.
    METHODS test_add_message   FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_messages_wo_dcs IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).

    cut = NEW /usi/cl_bal_lstate_active(
                  i_factory                  = bl_factory
                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                  i_auto_save_pckg_size      = 0
                  i_log_dao                  = NEW lcl_dao_spy( )
                  i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                  i_token                    = bl_factory->get_token( )
                  i_relevant_data_containers = VALUE #(
                      ( /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_src_pos_cx=>/usi/if_bal_data_container~get_classname( ) ) ) ).
  ENDMETHOD.

  METHOD test_add_exception.
    TRY.
        cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_exception     = lcl_exception_factory=>get_exception( )
                            i_log_previous  = abap_false ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_free_text.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = 'Just a test' ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_message.
    TRY.
        cut->add_message( i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
                          i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type       = /usi/cl_bal_enum_message_type=>error
                          i_message_class      = '38'
                          i_message_number     = '000'
                          i_message_variable_1 = 'Just a test' ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Automatic creation of data containers
" ---------------------------------------------------------------------
CLASS lcl_unit_test_auto_data_cont DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_logger_state.

    METHODS setup.
    METHODS test_add_exception FOR TESTING.
    METHODS test_add_free_text FOR TESTING.
    METHODS test_add_message   FOR TESTING.

    METHODS get_data_container_collection
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container_col.

    METHODS assert_has_container
      IMPORTING i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col
                i_data_container_classname  TYPE /usi/bal_data_cont_classname.
ENDCLASS.


CLASS lcl_unit_test_auto_data_cont IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).

    cut = NEW /usi/cl_bal_lstate_active(
                  i_factory                  = bl_factory
                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                  i_auto_save_pckg_size      = 0
                  i_log_dao                  = NEW lcl_dao_spy( )
                  i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                  i_token                    = bl_factory->get_token( )
                  i_relevant_data_containers = VALUE #(
                      ( /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_src_pos_cx=>/usi/if_bal_data_container~get_classname( ) ) ) ).
  ENDMETHOD.

  METHOD test_add_exception.
    TRY.
        cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_exception     = lcl_exception_factory=>get_exception( )
                            i_log_previous  = abap_false ).

        DATA(data_container_collection) = get_data_container_collection( ).

        assert_has_container(
            i_data_container_collection = data_container_collection
            i_data_container_classname  = /usi/cl_bal_dc_src_pos_cx=>/usi/if_bal_data_container~get_classname( ) ).

        assert_has_container(
            i_data_container_collection = data_container_collection
            i_data_container_classname  = /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) ).

        assert_has_container(
            i_data_container_collection = data_container_collection
            i_data_container_classname  = /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( ) ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_free_text.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = `Just a test` ).

        DATA(data_container_collection) = get_data_container_collection( ).

        assert_has_container(
            i_data_container_collection = data_container_collection
            i_data_container_classname  = /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) ).

        assert_has_container(
            i_data_container_collection = data_container_collection
            i_data_container_classname  = /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( ) ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_message.
    TRY.
        cut->add_message( i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
                          i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type       = /usi/cl_bal_enum_message_type=>error
                          i_message_class      = '38'
                          i_message_number     = '000'
                          i_message_variable_1 = 'Just a test' ).

        DATA(data_container_collection) = get_data_container_collection( ).

        assert_has_container(
            i_data_container_collection = data_container_collection
            i_data_container_classname  = /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) ).

        assert_has_container(
            i_data_container_collection = data_container_collection
            i_data_container_classname  = /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( ) ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_data_container_collection.
    DATA(private_data) = NEW lcl_private_data( cut ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = private_data->get_message_counter( )
                                        msg = `The buffer should contain exactly one message!` ).

    r_result = private_data->get_data_container_collection( 1 ).
    cl_abap_unit_assert=>assert_bound( act = r_result
                                       msg = `Data container collection is not bound!` ).
  ENDMETHOD.

  METHOD assert_has_container.
    LOOP AT i_data_container_collection->get_data_containers( ) ASSIGNING FIELD-SYMBOL(<data_container>).
      IF <data_container>->get_classname( ) <> i_data_container_classname.
        CONTINUE.
      ENDIF.
      RETURN.
    ENDLOOP.

    cl_abap_unit_assert=>fail( msg    = `Expected container not found`
                               detail = i_data_container_classname ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Filter data containers
" ---------------------------------------------------------------------
CLASS lcl_unit_test_filter_data_cont DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut                       TYPE REF TO /usi/if_bal_logger_state,
          irrelevant_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    METHODS setup.
    METHODS test_add_exception FOR TESTING.
    METHODS test_add_message   FOR TESTING.
    METHODS test_add_free_text FOR TESTING.
    METHODS assert_collection_is_empty.
ENDCLASS.


CLASS lcl_unit_test_filter_data_cont IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).

    cut = NEW /usi/cl_bal_lstate_active( i_factory                  = bl_factory
                                         i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                         i_auto_save_pckg_size      = 0
                                         i_log_dao                  = NEW lcl_dao_spy( )
                                         i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                                         i_token                    = bl_factory->get_token( )
                                         i_relevant_data_containers = VALUE #( ) ).

    irrelevant_data_cont_coll = lcl_data_container_factory=>get_data_container_collection( )->insert(
                                    lcl_data_container_factory=>get_html_container( `Irrelevant container` ) ).
  ENDMETHOD.

  METHOD test_add_exception.
    DATA(irrelevant_data_container) = lcl_data_container_factory=>get_retcode_msg_container( i_text = 'Irrelevant' ).
    DATA(exception)                 = lcl_exception_factory=>get_exception( i_details = irrelevant_data_container ).
    TRY.
        cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_exception     = exception
                            i_log_previous  = abap_false
                            i_details       = irrelevant_data_cont_coll ).
        assert_collection_is_empty( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_free_text.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = `Just a test`
                            i_details       = irrelevant_data_cont_coll ).
        assert_collection_is_empty( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_message.
    TRY.
        cut->add_message( i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
                          i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type       = /usi/cl_bal_enum_message_type=>error
                          i_message_class      = '38'
                          i_message_number     = '000'
                          i_message_variable_1 = 'Just a test'
                          i_details            = irrelevant_data_cont_coll ).
        assert_collection_is_empty( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_collection_is_empty.
    DATA(private_data) = NEW lcl_private_data( cut ).

    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = private_data->get_message_counter( )
                                        msg = `The buffer should contain exactly one message!` ).

    DATA(data_container_collection) = private_data->get_data_container_collection( 1 ).
    IF data_container_collection IS BOUND.
      cl_abap_unit_assert=>assert_false( act = data_container_collection->has_data_containers( )
                                         msg = `The collection should not have any containers!` ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: CUT must not return a token
" ---------------------------------------------------------------------
CLASS lcl_unit_test_token DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut         TYPE REF TO /usi/if_bal_logger_state,
          dc_coll_dao TYPE REF TO lcl_data_cont_coll_dao_spy,
          bl_factory  TYPE REF TO /usi/if_bal_logger_bl_factory,
          log_dao     TYPE REF TO lcl_dao_spy.

    METHODS setup.
    METHODS test_throws_on_get_token FOR TESTING.
    METHODS test_save_checks_token   FOR TESTING.
    METHODS test_free_checks_token   FOR TESTING.
    METHODS add_test_message.
ENDCLASS.


CLASS lcl_unit_test_token IMPLEMENTATION.
  METHOD setup.
    bl_factory  = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).
    log_dao     = NEW #( ).
    dc_coll_dao = NEW #( ).

    cut = NEW /usi/cl_bal_lstate_active( i_factory                  = bl_factory
                                         i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                         i_auto_save_pckg_size      = 0
                                         i_log_dao                  = log_dao
                                         i_data_cont_coll_dao       = dc_coll_dao
                                         i_token                    = bl_factory->get_token( )
                                         i_relevant_data_containers = VALUE #( ) ).
  ENDMETHOD.

  METHOD test_throws_on_get_token.
    TRY.
        cut->claim_ownership( ).
        cl_abap_unit_assert=>fail( `GET_TOKEN should throw an exception!` ).
      CATCH /usi/cx_bal_root.
        " Expected result
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_free_checks_token.
    add_test_message( ).
    DATA(wrong_token) = bl_factory->get_token( ).

    TRY.
        cut->free( wrong_token ).
        cl_abap_unit_assert=>fail( `Should throw exception on wrong token!` ).
      CATCH /usi/cx_bal_root.
        log_dao->method_calls->assert_method_was_not_called( log_dao->method_names-free ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_save_checks_token.
    add_test_message( ).
    DATA(wrong_token) = bl_factory->get_token( ).

    TRY.
        cut->save( wrong_token ).
        cl_abap_unit_assert=>fail( `Should throw exception on wrong token!` ).
      CATCH /usi/cx_bal_root.
        log_dao->method_calls->assert_method_was_not_called( log_dao->method_names-save ).
        dc_coll_dao->method_calls->assert_method_was_not_called( dc_coll_dao->method_names-save_buffer_to_db ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_test_message.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = 'Test message...' ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Filtering of messages by log level
" ---------------------------------------------------------------------
CLASS lcl_unit_test_msg_filter DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_logger_state.

    METHODS setup.

    METHODS test_add_irrelevant_exception FOR TESTING.
    METHODS test_add_irrelevant_free_text FOR TESTING.
    METHODS test_add_irrelevant_message   FOR TESTING.
    METHODS test_add_relevant_exception   FOR TESTING.
    METHODS test_add_relevant_free_text   FOR TESTING.
    METHODS test_add_relevant_message     FOR TESTING.

    METHODS assert_message_was_appended.
    METHODS assert_message_wasnt_appended.

    METHODS is_message_buffer_initial
      RETURNING VALUE(r_result) TYPE abap_bool.
ENDCLASS.


CLASS lcl_unit_test_msg_filter IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).

    cut = NEW /usi/cl_bal_lstate_active( i_factory                  = bl_factory
                                         i_log_level                = /usi/cl_bal_enum_log_level=>very_important
                                         i_auto_save_pckg_size      = 0
                                         i_log_dao                  = NEW lcl_dao_spy( )
                                         i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                                         i_token                    = bl_factory->get_token( )
                                         i_relevant_data_containers = VALUE #( ) ).
  ENDMETHOD.

  METHOD test_add_irrelevant_exception.
    TRY.
        cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_exception     = lcl_exception_factory=>get_exception( )
                            i_log_previous  = abap_false ).
        assert_message_wasnt_appended( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_irrelevant_free_text.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = 'Should be ignored...' ).
        assert_message_wasnt_appended( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_irrelevant_message.
    TRY.
        cut->add_message( i_problem_class      = /usi/cl_bal_enum_problem_class=>important
                          i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type       = /usi/cl_bal_enum_message_type=>error
                          i_message_class      = '38'
                          i_message_number     = '000'
                          i_message_variable_1 = 'Should be ignored...' ).
        assert_message_wasnt_appended( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_relevant_exception.
    TRY.
        cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_exception     = lcl_exception_factory=>get_exception( )
                            i_log_previous  = abap_false ).
        assert_message_was_appended( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_relevant_free_text.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = 'Should be appended...' ).
        assert_message_was_appended( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_relevant_message.
    TRY.
        cut->add_message( i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
                          i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type       = /usi/cl_bal_enum_message_type=>error
                          i_message_class      = '38'
                          i_message_number     = '000'
                          i_message_variable_1 = 'Should be appended...' ).
        assert_message_was_appended( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_message_wasnt_appended.
    IF is_message_buffer_initial( ) <> abap_true.
      cl_abap_unit_assert=>fail( `Message buffer should be initial!` ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_message_was_appended.
    IF is_message_buffer_initial( ) = abap_true.
      cl_abap_unit_assert=>fail( `Message buffer should _NOT_ be initial!` ).
    ENDIF.
  ENDMETHOD.

  METHOD is_message_buffer_initial.
    r_result = boolc( NEW lcl_private_data( cut )->get_message_counter( ) = 0 ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Add previous exceptions, if requested
" ---------------------------------------------------------------------
CLASS lcl_unit_test_log_previous DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut          TYPE REF TO /usi/if_bal_logger_state,
          bl_factory   TYPE REF TO /usi/if_bal_logger_bl_factory,
          private_data TYPE REF TO lcl_private_data.

    METHODS setup.
    METHODS test_log_with_previous       FOR TESTING.
    METHODS test_log_without_previous    FOR TESTING.
    METHODS test_previous_data_container FOR TESTING.

    METHODS get_exception
      RETURNING VALUE(r_result) TYPE REF TO /usi/cx_bal_root.

    METHODS get_actual_result
      IMPORTING i_message_number TYPE /usi/bal_message_number
      RETURNING VALUE(r_result)  TYPE /usi/bal_data_cont_classnames.

    METHODS get_expected_result
      IMPORTING i_exception                 TYPE REF TO cx_root
                i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col OPTIONAL
                i_is_previous               TYPE abap_bool
      RETURNING VALUE(r_result)             TYPE /usi/bal_data_cont_classnames.
ENDCLASS.


CLASS lcl_unit_test_log_previous IMPLEMENTATION.
  METHOD setup.
    bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).

    cut = NEW /usi/cl_bal_lstate_active(
                  i_factory                  = bl_factory
                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                  i_auto_save_pckg_size      = 0
                  i_log_dao                  = NEW lcl_dao_spy( )
                  i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                  i_token                    = bl_factory->get_token( )
                  i_relevant_data_containers = VALUE #(
                      ( /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_src_pos_cx=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_html=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_retcode_and_msg=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_itab=>/usi/if_bal_data_container~get_classname( ) ) ) ).

    private_data = NEW #( cut ).
  ENDMETHOD.

  METHOD get_exception.
    DATA(previous) = lcl_exception_factory=>get_exception(
                         i_text    = 'Pre-Previous'
                         i_details = lcl_data_container_factory=>get_html_container( `Exception-specific container` ) ).

    previous = lcl_exception_factory=>get_exception(
                   i_text     = 'Previous'
                   i_previous = previous
                   i_details  = lcl_data_container_factory=>get_data_container_collection( )->insert(
                                    lcl_data_container_factory=>get_retcode_msg_container( ) ) ).

    r_result = lcl_exception_factory=>get_exception(
                   i_text     = 'The exception'
                   i_previous = previous
                   i_details  = lcl_data_container_factory=>get_itab_container( `Exception-specific container` ) ).
  ENDMETHOD.

  METHOD test_log_with_previous.
    TRY.
        cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_exception     = get_exception( )
                            i_log_previous  = abap_true ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    cl_abap_unit_assert=>assert_equals( exp = 3
                                        act = private_data->get_message_counter( )
                                        msg = `Not all previous exceptions have been appended!` ).
  ENDMETHOD.

  METHOD test_log_without_previous.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root,
          message_counter      TYPE int4.

    TRY.
        cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_exception     = get_exception( )
                            i_log_previous  = abap_false ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    message_counter = private_data->get_message_counter( ).
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = message_counter
                                        msg = `Previous exceptions have been appended. This was not requested!` ).
  ENDMETHOD.

  METHOD test_previous_data_container.
    DATA: actual_result                 TYPE /usi/bal_data_cont_classnames,
          current_exception             TYPE REF TO cx_root,
          expected_result               TYPE /usi/bal_data_cont_classnames,
          index_starting_with_last_line TYPE int4,
          message_counter               TYPE int4.

    DATA(exception) = get_exception( ).
    DATA(data_container_collection) = lcl_data_container_factory=>get_data_container_collection( )->insert(
                                          lcl_data_container_factory=>get_html_container( `Some HTML container` ) ).

    TRY.
        cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_exception     = exception
                            i_log_previous  = abap_true
                            i_details       = data_container_collection ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    message_counter = private_data->get_message_counter( ).
    current_exception = exception.
    WHILE current_exception IS BOUND.
      " Previous will be appended before the main-exception (inverted order)
      index_starting_with_last_line = message_counter + 1 - sy-index.
      actual_result                 = get_actual_result( CONV #( index_starting_with_last_line ) ).

      IF current_exception = exception.
        expected_result = get_expected_result( i_exception                 = current_exception
                                               i_data_container_collection = data_container_collection
                                               i_is_previous               = abap_false ).
      ELSE.
        expected_result = get_expected_result( i_exception   = current_exception
                                               i_is_previous = abap_true ).
      ENDIF.

      cl_abap_unit_assert=>assert_equals( exp = expected_result
                                          act = actual_result
                                          msg = `Container Collection did not look, like expected!` ).

      current_exception = current_exception->previous.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_actual_result.
    LOOP AT private_data->get_data_container_collection( i_message_number )->get_data_containers( )
         ASSIGNING FIELD-SYMBOL(<data_container>).
      INSERT <data_container>->get_classname( ) INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_expected_result.
    DATA exception_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    " Exception-Containers (directly attached to the exception itself)
    exception_data_cont_coll = lcl_data_container_factory=>get_data_container_collection( ).
    bl_factory->get_exception_mapper( i_exception )->get_data_containers( exception_data_cont_coll ).
    LOOP AT exception_data_cont_coll->get_data_containers( ) ASSIGNING FIELD-SYMBOL(<data_container>).
      INSERT <data_container>->get_classname( ) INTO TABLE r_result.
    ENDLOOP.

    " Exception-related, automatic containers (Source code position of the RAISE-EXCEPTION-statement)
    INSERT /usi/cl_bal_dc_src_pos_cx=>/usi/if_bal_data_container~get_classname( ) INTO TABLE r_result.

    " Caller-related containers (For main-exception only!)
    IF i_is_previous <> abap_true.
      " Auto-Containers
      INSERT /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) INTO TABLE r_result.
      INSERT /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( ) INTO TABLE r_result.

      " Containers, that were passed as an independent collection along with the main exception
      IF i_data_container_collection IS BOUND.
        LOOP AT i_data_container_collection->get_data_containers( ) ASSIGNING <data_container>.
          INSERT <data_container>->get_classname( ) INTO TABLE r_result.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Priority of data containers
" ---------------------------------------------------------------------
CLASS lcl_unit_test_data_cont_prio DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut          TYPE REF TO /usi/if_bal_logger_state,
          private_data TYPE REF TO lcl_private_data.

    METHODS setup.
    METHODS test_add_exception FOR TESTING.
    METHODS test_add_free_text FOR TESTING.
    METHODS test_add_message   FOR TESTING.

    METHODS get_data_container_collection
      RETURNING VALUE(r_result) TYPE REF TO /usi/cl_bal_dc_collection.

    METHODS assert_has_data_cont_coll
      IMPORTING i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col.
ENDCLASS.


CLASS lcl_unit_test_data_cont_prio IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).

    cut = NEW /usi/cl_bal_lstate_active(
                  i_factory                  = bl_factory
                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                  i_auto_save_pckg_size      = 0
                  i_log_dao                  = NEW lcl_dao_spy( )
                  i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                  i_token                    = bl_factory->get_token( )
                  i_relevant_data_containers = VALUE #(
                      ( /usi/cl_bal_dc_callstack=>/usi/if_bal_data_container~get_classname( )      )
                      ( /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) )
                      ( /usi/cl_bal_dc_src_pos_cx=>/usi/if_bal_data_container~get_classname( )     ) ) ).

    private_data = NEW #( cut ).
  ENDMETHOD.

  METHOD test_add_exception.
    DATA(data_cont_coll_direct)    = get_data_container_collection( ).
    DATA(data_cont_coll_exception) = get_data_container_collection( ).

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found
          EXPORTING textid  = /usi/cx_bal_not_found=>no_db_entries_found
                    details = data_cont_coll_exception.
      CATCH /usi/cx_bal_root INTO DATA(exception_to_log).
        TRY.
            cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                i_message_type  = /usi/cl_bal_enum_message_type=>error
                                i_exception     = exception_to_log
                                i_log_previous  = abap_false
                                i_details       = data_cont_coll_direct ).
            assert_has_data_cont_coll( data_cont_coll_direct ).

            cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                i_message_type  = /usi/cl_bal_enum_message_type=>error
                                i_exception     = exception_to_log
                                i_log_previous  = abap_false ).
            assert_has_data_cont_coll( data_cont_coll_exception ).
          CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
            /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_free_text.
    DATA(data_cont_coll_direct) = get_data_container_collection( ).

    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = `Just a test...`
                            i_details       = data_cont_coll_direct ).

        assert_has_data_cont_coll( data_cont_coll_direct ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_message.
    DATA(data_cont_coll_direct) = get_data_container_collection( ).

    TRY.
        cut->add_message( i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
                          i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type       = /usi/cl_bal_enum_message_type=>error
                          i_message_class      = '38'
                          i_message_number     = '000'
                          i_message_variable_1 = 'Just a test...'
                          i_details            = data_cont_coll_direct ).

        assert_has_data_cont_coll( data_cont_coll_direct ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_data_container_collection.
    r_result = NEW /usi/cl_bal_dc_collection( ).
    r_result->insert( lcl_data_container_factory=>get_callstack_container( ) ).
    r_result->insert( lcl_data_container_factory=>get_src_pos_caller_container( ) ).
    r_result->insert( lcl_data_container_factory=>get_src_pos_cx_container( ) ).
  ENDMETHOD.

  METHOD assert_has_data_cont_coll.
    DATA(expected_data_containers) = i_data_container_collection->get_data_containers( ).

    DATA(actual_result_index)        = private_data->get_message_counter( ).
    DATA(actual_data_container_coll) = private_data->get_data_container_collection( CONV #( actual_result_index ) ).
    DATA(actual_data_containers)     = actual_data_container_coll->get_data_containers( ).

    LOOP AT expected_data_containers ASSIGNING FIELD-SYMBOL(<expected_data_container>).
      IF NOT line_exists( actual_data_containers[ table_line = <expected_data_container> ] ).
        cl_abap_unit_assert=>fail( msg    = `Expected container not found!`
                                   detail = <expected_data_container>->get_classname( ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Verify callback functions
" ---------------------------------------------------------------------
CLASS lcl_unit_test_callback DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut          TYPE REF TO /usi/if_bal_logger_state,
          dao_spy      TYPE REF TO lcl_dao_spy,
          private_data TYPE REF TO lcl_private_data.

    METHODS setup.
    METHODS test_message_callback         FOR TESTING.
    METHODS test_no_callback_without_data FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_callback IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).
    dao_spy = NEW #( ).

    cut = NEW /usi/cl_bal_lstate_active( i_factory                  = bl_factory
                                         i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                         i_auto_save_pckg_size      = 0
                                         i_log_dao                  = dao_spy
                                         i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                                         i_token                    = bl_factory->get_token( )
                                         i_relevant_data_containers = VALUE #(
                                             ( /usi/cl_bal_dc_html=>/usi/if_bal_data_container~get_classname( ) ) ) ).

    private_data = NEW #( cut ).
  ENDMETHOD.

  METHOD test_message_callback.
    DATA(data_container_collection) = lcl_data_container_factory=>get_data_container_collection( ).
    data_container_collection->insert( lcl_data_container_factory=>get_html_container( `Test` ) ).

    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = `Just a test...`
                            i_details       = data_container_collection ).

        DATA(logged_message) = dao_spy->get_message( 1 ).
        cl_abap_unit_assert=>assert_not_initial( act = logged_message-params-callback-userexitf
                                                 msg = `Callback function is not set!` ).

        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING  funcname           = logged_message-params-callback-userexitf
          EXCEPTIONS function_not_exist = 1
                     OTHERS             = 2.
        DATA(return_code) = sy-subrc.
        cl_abap_unit_assert=>assert_subrc( exp = 0
                                           act = return_code
                                           msg = `Callback function does not exist!` ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_no_callback_without_data.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = `Just a test...` ).

        DATA(logged_message) = dao_spy->get_message( 1 ).
        cl_abap_unit_assert=>assert_initial( act = logged_message-params-callback-userexitf
                                             msg = `Callback function is set!` ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Auto-Save
" ---------------------------------------------------------------------
CLASS lcl_unit_test_auto_save DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_test_objects,
             cut     TYPE REF TO /usi/if_bal_logger_state,
             log_dao TYPE REF TO lcl_dao_spy,
           END   OF ty_test_objects.

    METHODS prepare_test
      IMPORTING i_auto_save_pckg_size TYPE /usi/bal_auto_save_pckg_size
      RETURNING VALUE(r_result)       TYPE ty_test_objects.

    METHODS test_auto_save_exception FOR TESTING.
    METHODS test_auto_save_free_text FOR TESTING.
    METHODS test_auto_save_message   FOR TESTING.
    METHODS test_no_early_auto_save  FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_auto_save IMPLEMENTATION.
  METHOD prepare_test.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).
    r_result-log_dao = NEW #( ).

    r_result-cut     = NEW /usi/cl_bal_lstate_active(
                               i_factory                  = bl_factory
                               i_log_level                = /usi/cl_bal_enum_log_level=>everything
                               i_auto_save_pckg_size      = i_auto_save_pckg_size
                               i_log_dao                  = r_result-log_dao
                               i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                               i_token                    = bl_factory->get_token( )
                               i_relevant_data_containers = VALUE #( ) ).
  ENDMETHOD.

  METHOD test_auto_save_exception.
    DATA(test_objects) = prepare_test( 1 ).
    test_objects-log_dao->method_calls->assert_method_was_not_called( test_objects-log_dao->method_names-save ).

    TRY.
        DATA(given_exception) = lcl_exception_factory=>get_exception(
                                    i_text     = 'Outer exception'
                                    i_previous = lcl_exception_factory=>get_exception( i_text = 'Inner exception' ) ).

        test_objects-cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                         i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                         i_message_type  = /usi/cl_bal_enum_message_type=>information
                                         i_exception     = given_exception
                                         i_log_previous  = abap_true ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    test_objects-log_dao->method_calls->assert_method_called_n_times(
        i_method_name              = test_objects-log_dao->method_names-save
        i_expected_number_of_calls = 2 ).
  ENDMETHOD.

  METHOD test_auto_save_free_text.
    DATA(test_objects) = prepare_test( 1 ).
    test_objects-log_dao->method_calls->assert_method_was_not_called( test_objects-log_dao->method_names-save ).

    TRY.
        test_objects-cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                         i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                         i_message_type  = /usi/cl_bal_enum_message_type=>information
                                         i_free_text     = 'Should be saved immediately...' ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    test_objects-log_dao->method_calls->assert_method_was_called( test_objects-log_dao->method_names-save ).
  ENDMETHOD.

  METHOD test_auto_save_message.
    DATA(test_objects) = prepare_test( 1 ).
    test_objects-log_dao->method_calls->assert_method_was_not_called( test_objects-log_dao->method_names-save ).

    TRY.
        test_objects-cut->add_message( i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
                                       i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
                                       i_message_type       = /usi/cl_bal_enum_message_type=>information
                                       i_message_class      = '38'
                                       i_message_number     = '000'
                                       i_message_variable_1 = 'Should be saved immediately...' ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    test_objects-log_dao->method_calls->assert_method_was_called( test_objects-log_dao->method_names-save ).
  ENDMETHOD.

  METHOD test_no_early_auto_save.
    DATA(test_objects) = prepare_test( 2 ).

    TRY.
        test_objects-cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                         i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                         i_message_type  = /usi/cl_bal_enum_message_type=>information
                                         i_free_text     = 'Should not save (1st Message; Package Size = 2)' ).
        test_objects-log_dao->method_calls->assert_method_was_not_called( test_objects-log_dao->method_names-save ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    TRY.
        test_objects-cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                         i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                         i_message_type  = /usi/cl_bal_enum_message_type=>information
                                         i_free_text     = 'Should save (2nd Message; Package Size = 2)' ).
        test_objects-log_dao->method_calls->assert_method_was_called( test_objects-log_dao->method_names-save ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Auto-Save
" ---------------------------------------------------------------------
CLASS lcl_unit_test_multiple_saves DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut             TYPE REF TO /usi/if_bal_logger_state,
          dc_coll_dao_spy TYPE REF TO lcl_data_cont_coll_dao_spy,
          token           TYPE REF TO /usi/if_bal_token.

    METHODS setup.
    METHODS append_and_save_message.

    METHODS get_last_message_number
      RETURNING VALUE(r_result) TYPE /usi/bal_message_number.

    METHODS test_message_number_is_kept FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_multiple_saves IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).
    token           = bl_factory->get_token( ).
    dc_coll_dao_spy = NEW #( ).

    cut = NEW /usi/cl_bal_lstate_active(
                  i_factory                  = bl_factory
                  i_log_level                = /usi/cl_bal_enum_log_level=>everything
                  i_auto_save_pckg_size      = 0
                  i_log_dao                  = NEW lcl_dao_spy( )
                  i_data_cont_coll_dao       = dc_coll_dao_spy
                  i_token                    = token
                  i_relevant_data_containers = VALUE #(
                      ( /usi/cl_bal_dc_src_pos_caller=>/usi/if_bal_data_container~get_classname( ) ) ) ).
  ENDMETHOD.

  METHOD test_message_number_is_kept.
    DATA: expected_message_number TYPE /usi/bal_message_number,
          actual_message_number   TYPE /usi/bal_message_number.

    DO 2 TIMES.
      expected_message_number = sy-index.

      append_and_save_message( ).
      actual_message_number = get_last_message_number( ).

      cl_abap_unit_assert=>assert_equals( exp = expected_message_number
                                          act = actual_message_number
                                          msg = 'Unexpected message number!' ).
    ENDDO.
  ENDMETHOD.

  METHOD append_and_save_message.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = `Just a test` ).
        cut->save( token ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_last_message_number.
    DATA message_number TYPE /usi/bal_message_number.

    DATA(method_calls) = dc_coll_dao_spy->method_calls->get_method_calls(
                             dc_coll_dao_spy->method_names-insert_into_buffer ).
    cl_abap_unit_assert=>assert_not_initial( act = method_calls
                                             msg = 'Method was not called!' ).

    LOOP AT method_calls REFERENCE INTO DATA(method_call).
      method_call->method_call->get_parameter(
        EXPORTING i_parameter_name  = dc_coll_dao_spy->parameter_names-message_number
        IMPORTING e_parameter_value = message_number ).

      IF message_number > r_result.
        r_result = message_number.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Test consistency checks
" ---------------------------------------------------------------------
CLASS lcl_unit_test_consistency DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    CONSTANTS: BEGIN OF some_random_message,
                 msgid TYPE symsgid VALUE '/USI/BAL',
                 msgno TYPE symsgno VALUE '005',
               END   OF some_random_message.

    DATA cut TYPE REF TO /usi/if_bal_logger_state.

    METHODS setup.

    METHODS test_throws_on_initial_msgid FOR TESTING.
    METHODS test_accepts_initial_msgno   FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_consistency IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).

    cut = NEW /usi/cl_bal_lstate_active( i_factory                  = bl_factory
                                         i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                         i_auto_save_pckg_size      = 0
                                         i_log_dao                  = NEW lcl_dao_spy( )
                                         i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                                         i_token                    = bl_factory->get_token( )
                                         i_relevant_data_containers = VALUE #( ) ).
  ENDMETHOD.

  METHOD test_throws_on_initial_msgid.
    CONSTANTS initial_message_id TYPE symsgid VALUE space.

    TRY.
        cut->add_message( i_problem_class  = /usi/cl_bal_enum_problem_class=>very_important
                          i_detail_level   = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type   = /usi/cl_bal_enum_message_type=>information
                          i_message_class  = initial_message_id
                          i_message_number = some_random_message-msgno ).

        cl_abap_unit_assert=>fail( msg = 'Empty message-ID should throw an exception' ).
      CATCH /usi/cx_bal_root.
        " This is the expected behavior
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_accepts_initial_msgno.
    CONSTANTS initial_message_number TYPE symsgno VALUE 0.

    TRY.
        cut->add_message( i_problem_class  = /usi/cl_bal_enum_problem_class=>very_important
                          i_detail_level   = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type   = /usi/cl_bal_enum_message_type=>information
                          i_message_class  = some_random_message-msgid
                          i_message_number = initial_message_number ).

        " This is the expected behavior
        RETURN.
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Test logger->display( )
" ---------------------------------------------------------------------
CLASS lcl_unit_test_display DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA: cut   TYPE REF TO /usi/if_bal_logger_state,
          token TYPE REF TO /usi/if_bal_token.

    METHODS setup.

    METHODS test_throws_if_partly_saved FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_display IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).
    token = bl_factory->get_token( ).

    cut = NEW /usi/cl_bal_lstate_active( i_factory                  = bl_factory
                                         i_log_level                = /usi/cl_bal_enum_log_level=>everything
                                         i_auto_save_pckg_size      = 0
                                         i_log_dao                  = NEW lcl_dao_spy( )
                                         i_data_cont_coll_dao       = NEW lcl_data_cont_coll_dao_spy( )
                                         i_token                    = token
                                         i_relevant_data_containers = VALUE #( ) ).
  ENDMETHOD.

  METHOD test_throws_if_partly_saved.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>information
                            i_free_text     = `Saved message` ).
        cut->save( token ).

        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>information
                            i_free_text     = `Unsaved message` ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    TRY.
        cut->display( ).
        cl_abap_unit_assert=>fail( msg = `Logger accepts call to display( ) for partially saved log!` ).
      CATCH /usi/cx_bal_root.
        " Expected behavior
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
