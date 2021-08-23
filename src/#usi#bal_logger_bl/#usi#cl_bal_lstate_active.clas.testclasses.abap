*"* use this source file for your ABAP unit test classes

*--------------------------------------------------------------------*
* Test-Double for Log-DAO
*--------------------------------------------------------------------*
CLASS /usi/cl_bal_aunit_method_call DEFINITION LOAD.
CLASS lcl_dao_spy DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_log_dao.

    CONSTANTS: BEGIN OF method_names,
                 add_message    TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'ADD_MESSAGE',
                 free           TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'FREE',
                 get_log_number TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'GET_LOG_NUMBER',
                 save           TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'SAVE',
               END   OF method_names.

    DATA: method_calls TYPE REF TO /usi/cl_bal_aunit_method_calls READ-ONLY.

    METHODS constructor.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_dao_spy IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT method_calls.
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~add_message.
    DATA method_call TYPE REF TO /usi/cl_bal_aunit_method_call.
    method_call = method_calls->insert_method_call( method_names-add_message ).
    method_call->add_parameter(
      i_parameter_name  = 'I_MESSAGE'
      i_parameter_value = i_message
    ).
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
ENDCLASS.

*--------------------------------------------------------------------*
* Test-Double for Data-container-collection-DAO
*--------------------------------------------------------------------*
CLASS lcl_data_cont_coll_dao_spy DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_cont_coll_dao.

    CONSTANTS:
      BEGIN OF method_names,
        delete_collection             TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'DELETE_COLLECTION',
        get_collection                TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'GET_COLLECTION',
        insert_collection_into_buffer TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'INSERT_COLLECTION_INTO_BUFFER',
        save_buffer_to_db             TYPE /usi/cl_bal_aunit_method_call=>ty_method_name VALUE 'SAVE_BUFFER_TO_DB',
      END   OF method_names,

      BEGIN OF parameter_names,
        log_number            TYPE /usi/cl_bal_aunit_method_call=>ty_parameter_name VALUE 'I_LOG_NUMBER',
        message_number        TYPE /usi/cl_bal_aunit_method_call=>ty_parameter_name VALUE 'I_MESSAGE_NUMBER',
        serialized_collection TYPE /usi/cl_bal_aunit_method_call=>ty_parameter_name VALUE 'I_SERIALIZED_COLLECTION',
      END   OF parameter_names.

    DATA: method_calls TYPE REF TO /usi/cl_bal_aunit_method_calls READ-ONLY.

    METHODS constructor.
ENDCLASS.

CLASS lcl_data_cont_coll_dao_spy IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT method_calls.
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~delete_collections.
    DATA method_call TYPE REF TO /usi/cl_bal_aunit_method_call.

    method_call = method_calls->insert_method_call( method_names-delete_collection ).
    method_call->add_parameter( i_parameter_name  = 'I_LOG_NUMBER'
                                i_parameter_value = i_log_numbers ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~get_collection.
    DATA method_call TYPE REF TO /usi/cl_bal_aunit_method_call.

    method_call = method_calls->insert_method_call( method_names-get_collection ).
    method_call->add_parameter( i_parameter_name  = 'I_LOG_NUMBER'
                                i_parameter_value = i_log_number ).
    method_call->add_parameter( i_parameter_name  = 'I_MESSAGE_NUMBER'
                                i_parameter_value = i_message_number ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_cont_coll_dao~insert_collection_into_buffer.
    DATA method_call TYPE REF TO /usi/cl_bal_aunit_method_call.

    method_call = method_calls->insert_method_call( method_names-insert_collection_into_buffer ).
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

*--------------------------------------------------------------------*
* Helper-class to access private components of CUT
*--------------------------------------------------------------------*
CLASS lcl_private_data DEFINITION DEFERRED.
CLASS /usi/cl_bal_lstate_active DEFINITION LOCAL FRIENDS lcl_private_data.

CLASS lcl_private_data DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_cut TYPE REF TO /usi/if_bal_logger_state.

    METHODS get_message_counter
      RETURNING
        VALUE(r_result) TYPE int4.

    METHODS get_message
      IMPORTING
        i_index         TYPE int4
      RETURNING
        VALUE(r_result) TYPE bal_s_msg.

    METHODS get_data_container_collection
      IMPORTING
        i_index         TYPE int4
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_data_container_col.

  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/cl_bal_lstate_active.
ENDCLASS.

CLASS lcl_private_data IMPLEMENTATION.
  METHOD constructor.
    DATA unexpected_exception TYPE REF TO cx_sy_move_cast_error.
    TRY.
        cut ?= i_cut.
      CATCH cx_sy_move_cast_error INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_message_counter.
    r_result = lines( cut->messages-message_buffer ).
  ENDMETHOD.

  METHOD get_message.
    FIELD-SYMBOLS: <message> TYPE cut->ty_message.

    READ TABLE  cut->messages-message_buffer
      ASSIGNING <message>
      INDEX     i_index.
    IF sy-subrc NE 0.
      cl_aunit_assert=>fail( `Requested entry does not exist!` ).
    ENDIF.

    r_result = <message>-message.
  ENDMETHOD.

  METHOD get_data_container_collection.
    FIELD-SYMBOLS: <message> TYPE cut->ty_message.

    READ TABLE  cut->messages-message_buffer
      ASSIGNING <message>
      INDEX     i_index.
    IF sy-subrc NE 0.
      cl_aunit_assert=>fail( `Requested entry does not exist!` ).
    ENDIF.

    r_result = <message>-data_container_collection.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Helper class: Data container factory
*--------------------------------------------------------------------*
CLASS lcl_data_container_factory DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS get_data_container_collection
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_data_container_col.

    CLASS-METHODS get_callstack_container
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_html_container
      IMPORTING
        i_title          TYPE /usi/if_bal_text_container_c40=>ty_text OPTIONAL
        i_paragraph_text TYPE string
      RETURNING
        VALUE(r_result)  TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_itab_container
      IMPORTING
        i_title         TYPE /usi/if_bal_text_container_c40=>ty_text DEFAULT 'Test'
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_retcode_msg_container
      IMPORTING
        i_return_code   TYPE sysubrc DEFAULT 4
        i_text          TYPE symsgv  DEFAULT 'Test'
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_src_pos_caller_container
      IMPORTING
        i_program_name  TYPE syrepid DEFAULT 'FOO'
        i_include_name  TYPE include DEFAULT 'BAR'
        i_source_line   TYPE int4    DEFAULT 42
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    CLASS-METHODS get_src_pos_cx_container
      IMPORTING
        i_program_name  TYPE syrepid DEFAULT 'FOO'
        i_include_name  TYPE include DEFAULT 'BAR'
        i_source_line   TYPE int4    DEFAULT 42
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.
ENDCLASS.

CLASS lcl_data_container_factory IMPLEMENTATION.
  METHOD get_data_container_collection.
    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_collection.
  ENDMETHOD.

  METHOD get_callstack_container.
    DATA callstack TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.

    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_callstack
      EXPORTING
        i_callstack = callstack.
  ENDMETHOD.

  METHOD get_html_container.
    DATA: document_title TYPE REF TO /usi/if_bal_text_container_c40,
          html_document  TYPE string,
          paragraph_text TYPE string.

    paragraph_text  = cl_abap_dyn_prg=>escape_xss_xml_html( i_paragraph_text ).
    CONCATENATE '<html><head/><body><p>'
                paragraph_text
                '</p></body></html>'
           INTO html_document
           IN CHARACTER MODE.

    IF i_title IS NOT INITIAL.
      CREATE OBJECT document_title TYPE /usi/cl_bal_tc_literal_c40
        EXPORTING
          i_text = i_title.
    ENDIF.

    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_html
      EXPORTING
        i_html_document  = html_document
        i_document_title = document_title.
  ENDMETHOD.

  METHOD get_itab_container.
    DATA: t000_table           TYPE STANDARD TABLE OF t000 WITH NON-UNIQUE DEFAULT KEY,
          title                TYPE REF TO /usi/if_bal_text_container_c40,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    SELECT *
      FROM t000
      INTO TABLE t000_table
      WHERE mandt EQ sy-mandt.

    IF i_title IS NOT INITIAL.
      CREATE OBJECT title TYPE /usi/cl_bal_tc_literal_c40
        EXPORTING
          i_text = i_title.
    ENDIF.

    TRY.
        CREATE OBJECT r_result TYPE /usi/cl_bal_dc_itab
          EXPORTING
            i_internal_table = t000_table
            i_title          = title.
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_retcode_msg_container.
    DATA message TYPE symsg.

    message-msgid = '38'.
    message-msgno = '000'.
    message-msgty = 'E'.
    message-msgv1 = i_text.

    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_retcode_and_msg
      EXPORTING
        i_message     = message
        i_return_code = i_return_code.
  ENDMETHOD.

  METHOD get_src_pos_caller_container.
    DATA source_code_position TYPE /usi/bal_source_code_position.

    source_code_position-program_name = i_program_name.
    source_code_position-include_name = i_include_name.
    source_code_position-source_line  = i_source_line.

    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_src_pos_caller
      EXPORTING
        i_source_code_position = source_code_position.
  ENDMETHOD.

  METHOD get_src_pos_cx_container.
    DATA source_code_position TYPE /usi/bal_source_code_position.

    source_code_position-program_name = i_program_name.
    source_code_position-include_name = i_include_name.
    source_code_position-source_line  = i_source_line.

    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_src_pos_cx
      EXPORTING
        i_source_code_position = source_code_position.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Helper class: Exception factory
*--------------------------------------------------------------------*
CLASS lcl_exception_factory DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    CLASS-METHODS get_exception
      IMPORTING
        i_text          TYPE symsgv DEFAULT 'Test exception'
        i_previous      TYPE REF TO cx_root                   OPTIONAL
        i_details       TYPE REF TO /usi/if_exception_details OPTIONAL
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cx_bal_root.
ENDCLASS.

CLASS lcl_exception_factory IMPLEMENTATION.
  METHOD get_exception.
    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found
          EXPORTING
            textid   = /usi/cx_bal_not_found=>free_text
            param1   = i_text
            previous = i_previous
            details  = i_details.
      CATCH /usi/cx_bal_root INTO r_result.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Delegation
*--------------------------------------------------------------------*
CLASS lcl_unit_test_dao_delegation DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut             TYPE REF TO /usi/if_bal_logger_state,
          log_dao_spy     TYPE REF TO lcl_dao_spy,
          dc_coll_dao_spy TYPE REF TO lcl_data_cont_coll_dao_spy,
          token           TYPE REF TO /usi/if_bal_token.

    METHODS setup.
    METHODS test_free                     FOR TESTING.
    METHODS test_save                     FOR TESTING.
    METHODS test_skip_save_for_empty_log  FOR TESTING.
    METHODS test_save_multiple_times      FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_dao_delegation IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory         TYPE REF TO /usi/if_bal_cust_eval_factory,
          data_container_classnames TYPE /usi/bal_data_cont_classnames,
          data_container_classname  TYPE /usi/bal_data_cont_classname,
          logger_bl_factory         TYPE REF TO /usi/if_bal_logger_bl_factory.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao_spy.
    CREATE OBJECT dc_coll_dao_spy.

    data_container_classname = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
    INSERT data_container_classname INTO TABLE data_container_classnames.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao_spy
        i_data_cont_coll_dao       = dc_coll_dao_spy
        i_token                    = token
        i_relevant_data_containers = data_container_classnames.
  ENDMETHOD.

  METHOD test_free.
    DATA unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->free( token ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    log_dao_spy->method_calls->assert_method_was_called( log_dao_spy->method_names-free ).
  ENDMETHOD.

  METHOD test_save.
    DATA unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = 'Just a test'
        ).
        cut->save( token ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    " Log-DAO (Saves the log)
    log_dao_spy->method_calls->assert_method_was_called(
      log_dao_spy->method_names-add_message
    ).
    log_dao_spy->method_calls->assert_method_was_called(
      log_dao_spy->method_names-save
    ).

    " Data-Container-Collection-DAO (Saves data container collections)
    dc_coll_dao_spy->method_calls->assert_method_was_called(
      dc_coll_dao_spy->method_names-insert_collection_into_buffer
    ).
    dc_coll_dao_spy->method_calls->assert_method_was_called(
      dc_coll_dao_spy->method_names-save_buffer_to_db
    ).
  ENDMETHOD.

  METHOD test_skip_save_for_empty_log.
    DATA unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->save( token ).
        cut->free( token ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    " Log-DAO (Saves the log)
    log_dao_spy->method_calls->assert_method_was_not_called(
      log_dao_spy->method_names-add_message
    ).
    log_dao_spy->method_calls->assert_method_was_not_called(
      log_dao_spy->method_names-save
    ).

    " Data-Container-Collection-DAO (Saves data container collections)
    dc_coll_dao_spy->method_calls->assert_method_was_not_called(
      dc_coll_dao_spy->method_names-insert_collection_into_buffer
    ).
    dc_coll_dao_spy->method_calls->assert_method_was_not_called(
      dc_coll_dao_spy->method_names-save_buffer_to_db
    ).
  ENDMETHOD.

  METHOD test_save_multiple_times.
    DATA: unexpected_exception   TYPE REF TO /usi/cx_bal_root,
          actual_number_of_calls TYPE int4.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = `Test`
        ).

        cut->save( token ).
        cut->save( token ).

        log_dao_spy->method_calls->assert_method_called_n_times(
          i_method_name              = log_dao_spy->method_names-add_message
          i_expected_number_of_calls = 1
        ).
        log_dao_spy->method_calls->assert_method_called_n_times(
          i_method_name              = log_dao_spy->method_names-save
          i_expected_number_of_calls = 1
        ).

        dc_coll_dao_spy->method_calls->assert_method_called_n_times(
          i_method_name              = dc_coll_dao_spy->method_names-insert_collection_into_buffer
          i_expected_number_of_calls = 1
        ).
        dc_coll_dao_spy->method_calls->assert_method_called_n_times(
          i_method_name              = dc_coll_dao_spy->method_names-save_buffer_to_db
          i_expected_number_of_calls = 1
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Unbound data container collections
*--------------------------------------------------------------------*
CLASS lcl_unit_test_unbound_dc_coll DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_logger_state.

    METHODS setup.
    METHODS test_add_exception FOR TESTING.
    METHODS test_add_free_text FOR TESTING.
    METHODS test_add_message   FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_unbound_dc_coll IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          data_container_coll_dao  TYPE REF TO /usi/if_bal_data_cont_coll_dao,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          log_dao                  TYPE REF TO /usi/if_bal_log_dao,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          relevant_data_container  TYPE /usi/bal_data_cont_classname,
          token                    TYPE REF TO /usi/if_bal_token.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao TYPE lcl_dao_spy.
    CREATE OBJECT data_container_coll_dao TYPE lcl_data_cont_coll_dao_spy.

    relevant_data_container = /usi/cl_bal_dc_callstack=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container = /usi/cl_bal_dc_src_pos_cx=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao
        i_data_cont_coll_dao       = data_container_coll_dao
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.
  ENDMETHOD.

  METHOD test_add_exception.
    DATA: exception            TYPE REF TO /usi/cx_bal_root,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    exception = lcl_exception_factory=>get_exception( ).
    TRY.
        cut->add_exception(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_exception     = exception
          i_log_previous  = abap_false
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_free_text.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = 'Just a test'
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_message.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_message(
          i_problem_class       = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level        = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type        = /usi/cl_bal_enum_message_type=>error
          i_message_class       = '38'
          i_message_number      = '000'
          i_message_variable_1  = 'Just a test'
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Automatic creation of data containers
*--------------------------------------------------------------------*
CLASS lcl_unit_test_auto_data_cont DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut             TYPE REF TO /usi/if_bal_logger_state,
          dc_coll_dao_spy TYPE REF TO lcl_data_cont_coll_dao_spy,
          log_dao_spy     TYPE REF TO lcl_dao_spy,
          token           TYPE REF TO /usi/if_bal_token.

    METHODS setup.
    METHODS test_add_exception FOR TESTING.
    METHODS test_add_free_text FOR TESTING.
    METHODS test_add_message   FOR TESTING.

    METHODS get_data_container_collection
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_data_container_col.

    METHODS assert_has_container
      IMPORTING
        i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col
        i_data_container_classname  TYPE /usi/bal_data_cont_classname.
ENDCLASS.

CLASS lcl_unit_test_auto_data_cont IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          relevant_data_container  TYPE /usi/bal_data_cont_classname.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao_spy.
    CREATE OBJECT dc_coll_dao_spy.

    relevant_data_container = /usi/cl_bal_dc_callstack=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container = /usi/cl_bal_dc_src_pos_cx=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao_spy
        i_data_cont_coll_dao       = dc_coll_dao_spy
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.
  ENDMETHOD.

  METHOD test_add_exception.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_container_classname  TYPE /usi/bal_data_cont_classname,
          exception                 TYPE REF TO /usi/cx_bal_root,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    exception = lcl_exception_factory=>get_exception( ).
    TRY.
        cut->add_exception(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_exception     = exception
          i_log_previous  = abap_false
        ).

        data_container_collection = get_data_container_collection( ).

        data_container_classname = /usi/cl_bal_dc_src_pos_cx=>get_classname( ).
        assert_has_container(
          i_data_container_collection = data_container_collection
          i_data_container_classname  = data_container_classname
        ).

        data_container_classname = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
        assert_has_container(
          i_data_container_collection = data_container_collection
          i_data_container_classname  = data_container_classname
        ).

        data_container_classname = /usi/cl_bal_dc_callstack=>get_classname( ).
        assert_has_container(
          i_data_container_collection = data_container_collection
          i_data_container_classname  = data_container_classname
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_free_text.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_container_classname  TYPE /usi/bal_data_cont_classname,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = `Just a test`
        ).

        data_container_collection = get_data_container_collection( ).

        data_container_classname = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
        assert_has_container(
          i_data_container_collection = data_container_collection
          i_data_container_classname  = data_container_classname
        ).

        data_container_classname = /usi/cl_bal_dc_callstack=>get_classname( ).
        assert_has_container(
          i_data_container_collection = data_container_collection
          i_data_container_classname  = data_container_classname
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_message.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_container_classname  TYPE /usi/bal_data_cont_classname,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_message(
          i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type       = /usi/cl_bal_enum_message_type=>error
          i_message_class      = '38'
          i_message_number     = '000'
          i_message_variable_1 = 'Just a test'
        ).

        data_container_collection = get_data_container_collection( ).

        data_container_classname = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
        assert_has_container(
          i_data_container_collection = data_container_collection
          i_data_container_classname  = data_container_classname
        ).

        data_container_classname = /usi/cl_bal_dc_callstack=>get_classname( ).
        assert_has_container(
          i_data_container_collection = data_container_collection
          i_data_container_classname  = data_container_classname
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_data_container_collection.
    DATA: private_data  TYPE REF TO lcl_private_data,
          message_count TYPE int4.

    CREATE OBJECT private_data
      EXPORTING
        i_cut = cut.

    message_count  = private_data->get_message_counter( ).
    cl_aunit_assert=>assert_equals(
      exp = 1
      act = message_count
      msg = `The buffer should contain exactly one message!`
    ).

    r_result = private_data->get_data_container_collection( 1 ).
    cl_aunit_assert=>assert_bound(
      act = r_result
      msg = `Data container collection is not bound!`
    ).
  ENDMETHOD.

  METHOD assert_has_container.
    DATA data_containers TYPE /usi/bal_data_containers.
    FIELD-SYMBOLS <data_container> TYPE REF TO /usi/if_bal_data_container.

    data_containers = i_data_container_collection->get_data_containers( ).
    LOOP AT data_containers ASSIGNING <data_container>.
      CHECK <data_container>->get_classname( ) EQ i_data_container_classname.
      RETURN.
    ENDLOOP.

    cl_aunit_assert=>fail(
      msg    = `Expected container not found`
      detail = i_data_container_classname
    ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Filter data containers
*--------------------------------------------------------------------*
CLASS lcl_unit_test_filter_data_cont DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut                       TYPE REF TO /usi/if_bal_logger_state,
          dc_coll_dao_spy           TYPE REF TO lcl_data_cont_coll_dao_spy,
          irrelevant_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col,
          log_dao_spy               TYPE REF TO lcl_dao_spy,
          token                     TYPE REF TO /usi/if_bal_token.

    METHODS setup.
    METHODS test_add_exception  FOR TESTING.
    METHODS test_add_message    FOR TESTING.
    METHODS test_add_free_text  FOR TESTING.
    METHODS assert_collection_is_empty.
ENDCLASS.

CLASS lcl_unit_test_filter_data_cont IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory         TYPE REF TO /usi/if_bal_cust_eval_factory,
          irrelevant_data_container TYPE REF TO /usi/if_bal_data_container,
          logger_bl_factory         TYPE REF TO /usi/if_bal_logger_bl_factory,
          relevant_data_containers  TYPE /usi/bal_data_cont_classnames.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao_spy.
    CREATE OBJECT dc_coll_dao_spy.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao_spy
        i_data_cont_coll_dao       = dc_coll_dao_spy
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.

    irrelevant_data_cont_coll = lcl_data_container_factory=>get_data_container_collection( ).
    irrelevant_data_container = lcl_data_container_factory=>get_html_container( `Irrelevant container` ).
    irrelevant_data_cont_coll->insert( irrelevant_data_container ).
  ENDMETHOD.

  METHOD test_add_exception.
    DATA: exception                 TYPE REF TO /usi/cx_bal_root,
          irrelevant_data_container TYPE REF TO /usi/if_bal_data_container,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    irrelevant_data_container = lcl_data_container_factory=>get_retcode_msg_container( i_text = 'Irrelevant' ).
    exception                 = lcl_exception_factory=>get_exception( i_details  = irrelevant_data_container ).
    TRY.
        cut->add_exception(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_exception     = exception
          i_log_previous  = abap_false
          i_details       = irrelevant_data_cont_coll
        ).
        assert_collection_is_empty( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_free_text.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = `Just a test`
          i_details       = irrelevant_data_cont_coll
        ).
        assert_collection_is_empty( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_message.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_message(
          i_problem_class       = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level        = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type        = /usi/cl_bal_enum_message_type=>error
          i_message_class       = '38'
          i_message_number      = '000'
          i_message_variable_1  = 'Just a test'
          i_details             = irrelevant_data_cont_coll
        ).
        assert_collection_is_empty( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD assert_collection_is_empty.
    DATA: private_data              TYPE REF TO lcl_private_data,
          message_count             TYPE int4,
          data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          has_data_containers       TYPE abap_bool.

    CREATE OBJECT private_data
      EXPORTING
        i_cut = cut.

    message_count  = private_data->get_message_counter( ).
    cl_aunit_assert=>assert_equals(
      exp = 1
      act = message_count
      msg = `The buffer should contain exactly one message!`
    ).

    data_container_collection = private_data->get_data_container_collection( 1 ).
    IF data_container_collection IS BOUND.
      has_data_containers = data_container_collection->has_data_containers( ).

      cl_aunit_assert=>assert_equals(
        exp = abap_false
        act = has_data_containers
        msg = `The collection should not have any containers!`
      ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: CUT must not return a token
*--------------------------------------------------------------------*
CLASS lcl_unit_test_token DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut               TYPE REF TO /usi/if_bal_logger_state,
          dc_coll_dao       TYPE REF TO lcl_data_cont_coll_dao_spy,
          logger_bl_factory TYPE REF TO /usi/if_bal_logger_bl_factory,
          log_dao           TYPE REF TO lcl_dao_spy.

    METHODS setup.
    METHODS test_throws_on_get_token FOR TESTING.
    METHODS test_save_checks_token   FOR TESTING.
    METHODS test_free_checks_token   FOR TESTING.
    METHODS add_test_message.
ENDCLASS.

CLASS lcl_unit_test_token IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          token                    TYPE REF TO /usi/if_bal_token.

    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token   = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao.
    CREATE OBJECT dc_coll_dao.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao
        i_data_cont_coll_dao       = dc_coll_dao
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.
  ENDMETHOD.

  METHOD test_throws_on_get_token.
    TRY.
        cut->claim_ownership( ).
        cl_aunit_assert=>fail( `GET_TOKEN should throw an exception!` ).
      CATCH /usi/cx_bal_root.
        " Expected result
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_free_checks_token.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root,
          wrong_token          TYPE REF TO /usi/if_bal_token.

    add_test_message( ).
    wrong_token = logger_bl_factory->get_token( ).

    TRY.
        cut->free( wrong_token ).

        cl_aunit_assert=>fail( `Should throw exception on wrong token!` ).
      CATCH /usi/cx_bal_root.
        log_dao->method_calls->assert_method_was_not_called( log_dao->method_names-free ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_save_checks_token.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root,
          wrong_token          TYPE REF TO /usi/if_bal_token.

    add_test_message( ).
    wrong_token = logger_bl_factory->get_token( ).

    TRY.
        cut->save( wrong_token ).

        cl_aunit_assert=>fail( `Should throw exception on wrong token!` ).
      CATCH /usi/cx_bal_root.
        log_dao->method_calls->assert_method_was_not_called( log_dao->method_names-save ).
        dc_coll_dao->method_calls->assert_method_was_not_called( dc_coll_dao->method_names-save_buffer_to_db ).
    ENDTRY.
  ENDMETHOD.

  METHOD add_test_message.
    DATA unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = 'Test message...'
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Filtering of messages by log level
*--------------------------------------------------------------------*
CLASS lcl_unit_test_msg_filter DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
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
      RETURNING
        VALUE(r_result) TYPE abap_bool.
ENDCLASS.

CLASS lcl_unit_test_msg_filter IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          log_dao                  TYPE REF TO lcl_dao_spy,
          dc_coll_dao              TYPE REF TO lcl_data_cont_coll_dao_spy,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          token                    TYPE REF TO /usi/if_bal_token.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao.
    CREATE OBJECT dc_coll_dao.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>very_important
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao
        i_data_cont_coll_dao       = dc_coll_dao
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.
  ENDMETHOD.

  METHOD test_add_irrelevant_exception.
    DATA: exception            TYPE REF TO /usi/cx_bal_root,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    exception = lcl_exception_factory=>get_exception( ).
    TRY.
        cut->add_exception(
          i_problem_class = /usi/cl_bal_enum_problem_class=>important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_exception     = exception
          i_log_previous  = abap_false
        ).
        assert_message_wasnt_appended( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_irrelevant_free_text.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = 'Should be ignored...'
        ).
        assert_message_wasnt_appended( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_irrelevant_message.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_message(
          i_problem_class       = /usi/cl_bal_enum_problem_class=>important
          i_detail_level        = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type        = /usi/cl_bal_enum_message_type=>error
          i_message_class       = '38'
          i_message_number      = '000'
          i_message_variable_1  = 'Should be ignored...'
        ).
        assert_message_wasnt_appended( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_relevant_exception.
    DATA: exception            TYPE REF TO /usi/cx_bal_root,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    exception = lcl_exception_factory=>get_exception( ).
    TRY.
        cut->add_exception(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_exception     = exception
          i_log_previous  = abap_false
        ).
        assert_message_was_appended( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_relevant_free_text.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = 'Should be appended...'
        ).
        assert_message_was_appended( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_relevant_message.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_message(
          i_problem_class       = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level        = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type        = /usi/cl_bal_enum_message_type=>error
          i_message_class       = '38'
          i_message_number      = '000'
          i_message_variable_1  = 'Should be appended...'
        ).
        assert_message_was_appended( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

  ENDMETHOD.

  METHOD assert_message_wasnt_appended.
    IF is_message_buffer_initial( ) NE abap_true.
      cl_aunit_assert=>fail( `Message buffer should be initial!` ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_message_was_appended.
    IF is_message_buffer_initial( ) EQ abap_true.
      cl_aunit_assert=>fail( `Message buffer should _NOT_ be initial!` ).
    ENDIF.
  ENDMETHOD.

  METHOD is_message_buffer_initial.
    DATA: private_data    TYPE REF TO lcl_private_data,
          message_counter TYPE int4.

    CREATE OBJECT private_data
      EXPORTING
        i_cut = cut.

    message_counter = private_data->get_message_counter( ).
    IF message_counter EQ 0.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Add previous exceptions, if requested
*--------------------------------------------------------------------*
CLASS lcl_unit_test_log_previous DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut               TYPE REF TO /usi/if_bal_logger_state,
          exception         TYPE REF TO /usi/cx_bal_root,
          logger_bl_factory TYPE REF TO /usi/if_bal_logger_bl_factory,
          private_data      TYPE REF TO lcl_private_data.

    METHODS setup.
    METHODS test_log_with_previous        FOR TESTING.
    METHODS test_log_without_previous     FOR TESTING.
    METHODS test_previous_data_container  FOR TESTING.

    METHODS get_exception
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cx_bal_root.

    METHODS get_actual_result
      IMPORTING
        VALUE(i_index)  TYPE int4
      RETURNING
        VALUE(r_result) TYPE /usi/bal_data_cont_classnames.

    METHODS get_expected_result
      IMPORTING
        i_exception                 TYPE REF TO cx_root
        i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col OPTIONAL
        i_is_previous               TYPE abap_bool
      RETURNING
        VALUE(r_result)             TYPE /usi/bal_data_cont_classnames.
ENDCLASS.

CLASS lcl_unit_test_log_previous IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          log_dao                  TYPE REF TO lcl_dao_spy,
          dc_coll_dao              TYPE REF TO lcl_data_cont_coll_dao_spy,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          relevant_data_container  TYPE /usi/bal_data_cont_classname,
          token                    TYPE REF TO /usi/if_bal_token.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao.
    CREATE OBJECT dc_coll_dao.

    relevant_data_container =  /usi/cl_bal_dc_callstack=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container =  /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container =  /usi/cl_bal_dc_src_pos_cx=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.

    relevant_data_container =  /usi/cl_bal_dc_html=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container =  /usi/cl_bal_dc_retcode_and_msg=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container =  /usi/cl_bal_dc_itab=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao
        i_data_cont_coll_dao       = dc_coll_dao
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.

    CREATE OBJECT private_data
      EXPORTING
        i_cut = cut.

    exception = get_exception( ).
  ENDMETHOD.

  METHOD get_exception.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_container            TYPE REF TO /usi/if_bal_data_container,
          previous                  TYPE REF TO /usi/cx_bal_root.

    data_container  = lcl_data_container_factory=>get_html_container( `Exception-specific container` ).
    previous        = lcl_exception_factory=>get_exception(
                        i_text     = 'Pre-Previous'
                        i_details  = data_container
                      ).

    data_container_collection = lcl_data_container_factory=>get_data_container_collection( ).
    data_container            = lcl_data_container_factory=>get_retcode_msg_container( ).
    data_container_collection->insert( data_container ).
    previous                  = lcl_exception_factory=>get_exception(
                                  i_text     = 'Previous'
                                  i_previous = previous
                                  i_details  = data_container_collection
                                ).

    data_container  = lcl_data_container_factory=>get_itab_container( `Exception-specific container` ).
    r_result        = lcl_exception_factory=>get_exception(
                        i_text     = 'The exception'
                        i_previous = previous
                        i_details  = data_container
                      ).
  ENDMETHOD.

  METHOD test_log_with_previous.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root,
          message_counter      TYPE int4.

    TRY.
        cut->add_exception(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_exception     = exception
          i_log_previous  = abap_true
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    message_counter = private_data->get_message_counter( ).
    cl_aunit_assert=>assert_equals(
      exp = 3
      act = message_counter
      msg = `Not all previous exceptions have been appended!`
    ).
  ENDMETHOD.

  METHOD test_log_without_previous.
    DATA: unexpected_exception TYPE REF TO /usi/cx_bal_root,
          message_counter      TYPE int4.

    TRY.
        cut->add_exception(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_exception     = exception
          i_log_previous  = abap_false
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    message_counter = private_data->get_message_counter( ).
    cl_aunit_assert=>assert_equals(
      exp = 1
      act = message_counter
      msg = `Previous exceptions have been appended. This was not requested!`
    ).
  ENDMETHOD.

  METHOD test_previous_data_container.
    DATA: actual_result                 TYPE /usi/bal_data_cont_classnames,
          data_container_collection     TYPE REF TO /usi/if_bal_data_container_col,
          data_container                TYPE REF TO /usi/if_bal_data_container,
          current_exception             TYPE REF TO cx_root,
          expected_result               TYPE /usi/bal_data_cont_classnames,
          index_starting_with_last_line TYPE int4,
          message_counter               TYPE int4,
          unexpected_exception          TYPE REF TO /usi/cx_bal_root.

    data_container            = lcl_data_container_factory=>get_html_container( `Some HTML container` ).
    data_container_collection = lcl_data_container_factory=>get_data_container_collection( ).
    data_container_collection->insert( data_container ).

    TRY.
        cut->add_exception(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_exception     = exception
          i_log_previous  = abap_true
          i_details       = data_container_collection
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    message_counter = private_data->get_message_counter( ).
    current_exception = exception.
    WHILE current_exception IS BOUND.
      " Previous will be appended before the main-exception (inverted order)
      index_starting_with_last_line = message_counter + 1 - sy-index.
      actual_result                 = get_actual_result( index_starting_with_last_line ).

      IF current_exception EQ exception.
        expected_result = get_expected_result(
                            i_exception                 = current_exception
                            i_data_container_collection = data_container_collection
                            i_is_previous               = abap_false
                          ).
      ELSE.
        expected_result = get_expected_result(
                            i_exception   = current_exception
                            i_is_previous = abap_true
                          ).
      ENDIF.

      cl_aunit_assert=>assert_equals(
        exp = expected_result
        act = actual_result
        msg = `Container list did not look, like expected!`
      ).

      current_exception = current_exception->previous.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_actual_result.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_containers           TYPE /usi/bal_data_containers,
          data_container_classname  TYPE /usi/bal_data_cont_classname.

    FIELD-SYMBOLS <data_container> TYPE REF TO /usi/if_bal_data_container.

    data_container_collection = private_data->get_data_container_collection( i_index ).
    data_containers = data_container_collection->get_data_containers( ).

    LOOP AT data_containers ASSIGNING <data_container>.
      data_container_classname = <data_container>->get_classname( ).
      INSERT data_container_classname INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_expected_result.
    DATA: data_containers          TYPE /usi/bal_data_containers,
          data_container_classname TYPE /usi/bal_data_cont_classname,
          exception_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col,
          exception_mapper         TYPE REF TO /usi/if_bal_exception_mapper.

    FIELD-SYMBOLS <data_container> TYPE REF TO /usi/if_bal_data_container.

    " Exception-Containers (directly attached to the exception itself)
    exception_mapper          = logger_bl_factory->get_exception_mapper( i_exception ).
    exception_data_cont_coll  = lcl_data_container_factory=>get_data_container_collection( ).
    exception_mapper->get_data_containers( exception_data_cont_coll ).
    data_containers           = exception_data_cont_coll->get_data_containers( ).
    LOOP AT data_containers ASSIGNING <data_container>.
      data_container_classname = <data_container>->get_classname( ).
      INSERT data_container_classname INTO TABLE r_result.
    ENDLOOP.

    " Exception-related, automatic containers (Source code position of the RAISE-EXCEPTION-statement)
    data_container_classname = /usi/cl_bal_dc_src_pos_cx=>get_classname( ).
    INSERT data_container_classname INTO TABLE r_result.

    " Caller-related containers (For main-exception only!)
    IF i_is_previous NE abap_true.
      " Auto-Containers
      data_container_classname = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
      INSERT data_container_classname INTO TABLE r_result.
      data_container_classname = /usi/cl_bal_dc_callstack=>get_classname( ).
      INSERT data_container_classname INTO TABLE r_result.

      " Containers, that were passed as an independent collection along with the main exception
      IF i_data_container_collection IS BOUND.
        data_containers = i_data_container_collection->get_data_containers( ).
        LOOP AT data_containers ASSIGNING <data_container>.
          data_container_classname = <data_container>->get_classname( ).
          INSERT data_container_classname INTO TABLE r_result.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Priority of data containers
*--------------------------------------------------------------------*
CLASS lcl_unit_test_data_cont_prio DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut          TYPE REF TO /usi/if_bal_logger_state,
          private_data TYPE REF TO lcl_private_data.

    METHODS setup.
    METHODS test_add_exception  FOR TESTING.
    METHODS test_add_free_text  FOR TESTING.
    METHODS test_add_message    FOR TESTING.

    METHODS get_data_container_collection
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_dc_collection.

    METHODS assert_has_data_cont_coll
      IMPORTING
        i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col.
ENDCLASS.

CLASS lcl_unit_test_data_cont_prio IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          log_dao                  TYPE REF TO lcl_dao_spy,
          dc_coll_dao              TYPE REF TO lcl_data_cont_coll_dao_spy,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          relevant_data_container  TYPE /usi/bal_data_cont_classname,
          token                    TYPE REF TO /usi/if_bal_token.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao.
    CREATE OBJECT dc_coll_dao.

    relevant_data_container = /usi/cl_bal_dc_callstack=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.
    relevant_data_container = /usi/cl_bal_dc_src_pos_cx=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao
        i_data_cont_coll_dao       = dc_coll_dao
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.

    CREATE OBJECT private_data
      EXPORTING
        i_cut = cut.
  ENDMETHOD.

  METHOD test_add_exception.
    DATA: exception_to_log         TYPE REF TO /usi/cx_bal_root,
          data_cont_coll_direct    TYPE REF TO /usi/if_bal_data_container_col,
          data_cont_coll_exception TYPE REF TO /usi/if_bal_data_container_col,
          unexpected_exception     TYPE REF TO /usi/cx_bal_root.

    data_cont_coll_direct    = get_data_container_collection( ).
    data_cont_coll_exception = get_data_container_collection( ).

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found
          EXPORTING
            textid  = /usi/cx_bal_not_found=>no_db_entries_found
            details = data_cont_coll_exception.
      CATCH /usi/cx_bal_root INTO exception_to_log.
        TRY.
            cut->add_exception(
              i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
              i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
              i_message_type  = /usi/cl_bal_enum_message_type=>error
              i_exception     = exception_to_log
              i_log_previous  = abap_false
              i_details       = data_cont_coll_direct
            ).
            assert_has_data_cont_coll( data_cont_coll_direct ).

            cut->add_exception(
              i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
              i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
              i_message_type  = /usi/cl_bal_enum_message_type=>error
              i_exception     = exception_to_log
              i_log_previous  = abap_false
            ).
            assert_has_data_cont_coll( data_cont_coll_exception ).
          CATCH /usi/cx_bal_root INTO unexpected_exception.
            /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_free_text.
    DATA: data_cont_coll_direct TYPE REF TO /usi/if_bal_data_container_col,
          unexpected_exception  TYPE REF TO /usi/cx_bal_root.

    data_cont_coll_direct    = get_data_container_collection( ).

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = `Just a test...`
          i_details       = data_cont_coll_direct
        ).

        assert_has_data_cont_coll( data_cont_coll_direct ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_add_message.
    DATA: data_cont_coll_direct TYPE REF TO /usi/if_bal_data_container_col,
          unexpected_exception  TYPE REF TO /usi/cx_bal_root.

    data_cont_coll_direct    = get_data_container_collection( ).

    TRY.
        cut->add_message(
          i_problem_class       = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level        = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type        = /usi/cl_bal_enum_message_type=>error
          i_message_class       = '38'
          i_message_number      = '000'
          i_message_variable_1  = 'Just a test...'
          i_details             = data_cont_coll_direct
        ).

        assert_has_data_cont_coll( data_cont_coll_direct ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_data_container_collection.
    DATA data_container TYPE REF TO /usi/if_bal_data_container.

    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_collection.

    data_container = lcl_data_container_factory=>get_callstack_container( ).
    r_result->insert( data_container ).

    data_container = lcl_data_container_factory=>get_src_pos_caller_container( ).
    r_result->insert( data_container ).

    data_container = lcl_data_container_factory=>get_src_pos_cx_container( ).
    r_result->insert( data_container ).
  ENDMETHOD.

  METHOD assert_has_data_cont_coll.
    DATA: expected_data_containers TYPE /usi/bal_data_containers,
          actual_data_cont_coll    TYPE REF TO /usi/if_bal_data_container_col,
          actual_data_containers   TYPE /usi/bal_data_containers,
          actual_result_index      TYPE int4,
          data_container_classname TYPE /usi/bal_data_cont_classname.

    FIELD-SYMBOLS <expected_data_container> TYPE REF TO /usi/if_bal_data_container.

    expected_data_containers  = i_data_container_collection->get_data_containers( ).

    actual_result_index       = private_data->get_message_counter( ).
    actual_data_cont_coll     = private_data->get_data_container_collection( actual_result_index ).
    actual_data_containers    = actual_data_cont_coll->get_data_containers( ).

    LOOP AT expected_data_containers ASSIGNING <expected_data_container>.
      READ TABLE actual_data_containers
        TRANSPORTING NO FIELDS
        WITH TABLE KEY table_line = <expected_data_container>.

      IF sy-subrc NE 0.
        data_container_classname = <expected_data_container>->get_classname( ).
        cl_aunit_assert=>fail(
          msg    = `Expected container not found!`
          detail = data_container_classname
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Verify callback functions
*--------------------------------------------------------------------*
CLASS lcl_unit_test_callback DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut          TYPE REF TO /usi/if_bal_logger_state,
          log_dao      TYPE REF TO lcl_dao_spy,
          private_data TYPE REF TO lcl_private_data.

    METHODS setup.
    METHODS test_message_callback         FOR TESTING.
    METHODS test_no_callback_without_data FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_callback IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          dc_coll_dao              TYPE REF TO lcl_data_cont_coll_dao_spy,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          relevant_data_container  TYPE /usi/bal_data_cont_classname,
          token                    TYPE REF TO /usi/if_bal_token.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao.
    CREATE OBJECT dc_coll_dao.

    relevant_data_container = /usi/cl_bal_dc_html=>get_classname( ).
    INSERT relevant_data_container INTO TABLE relevant_data_containers.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao
        i_data_cont_coll_dao       = dc_coll_dao
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.

    CREATE OBJECT private_data
      EXPORTING
        i_cut = cut.
  ENDMETHOD.

  METHOD test_message_callback.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_container            TYPE REF TO /usi/if_bal_data_container,
          logged_message            TYPE bal_s_msg,
          return_code               TYPE sysubrc,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    data_container_collection = lcl_data_container_factory=>get_data_container_collection( ).
    data_container            = lcl_data_container_factory=>get_html_container( `Test` ).
    data_container_collection->insert( data_container ).

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = `Just a test...`
          i_details       = data_container_collection
        ).

        logged_message = private_data->get_message( i_index = 1 ).
        cl_aunit_assert=>assert_not_initial(
          act = logged_message-params-callback-userexitf
          msg = `Callback function is not set!`
        ).

        CALL FUNCTION 'FUNCTION_EXISTS'
          EXPORTING
            funcname           = logged_message-params-callback-userexitf
          EXCEPTIONS
            function_not_exist = 1
            OTHERS             = 2.
        return_code = sy-subrc.
        cl_aunit_assert=>assert_equals(
          exp = 0
          act = return_code
          msg = `Callback function does not exist!`
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_no_callback_without_data.
    DATA: logged_message       TYPE bal_s_msg,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text(
          i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
          i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
          i_message_type  = /usi/cl_bal_enum_message_type=>error
          i_free_text     = `Just a test...`
        ).

        logged_message = private_data->get_message( i_index = 1 ).
        cl_aunit_assert=>assert_initial(
          act = logged_message-params-callback-userexitf
          msg = `Callback function is set!`
        ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Auto-Save
*--------------------------------------------------------------------*
CLASS lcl_unit_test_auto_save DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_test_objects,
             cut     TYPE REF TO /usi/if_bal_logger_state,
             log_dao TYPE REF TO lcl_dao_spy,
           END   OF ty_test_objects.

    METHODS prepare_test
      IMPORTING
        i_auto_save_pckg_size TYPE /usi/bal_auto_save_pckg_size
      RETURNING
        VALUE(r_result)       TYPE ty_test_objects.

    METHODS test_auto_save_exception FOR TESTING.
    METHODS test_auto_save_free_text FOR TESTING.
    METHODS test_auto_save_message   FOR TESTING.
    METHODS test_no_early_auto_save  FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_auto_save IMPLEMENTATION.
  METHOD prepare_test.
    DATA: cust_eval_factory        TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory        TYPE REF TO /usi/if_bal_logger_bl_factory,
          dc_coll_dao              TYPE REF TO lcl_data_cont_coll_dao_spy,
          relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          relevant_data_container  TYPE /usi/bal_data_cont_classname,
          token                    TYPE REF TO /usi/if_bal_token.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT r_result-log_dao.
    CREATE OBJECT dc_coll_dao.

    CREATE OBJECT r_result-cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = i_auto_save_pckg_size
        i_log_dao                  = r_result-log_dao
        i_data_cont_coll_dao       = dc_coll_dao
        i_token                    = token
        i_relevant_data_containers = relevant_data_containers.
  ENDMETHOD.

  METHOD test_auto_save_exception.
    DATA: test_objects         TYPE ty_test_objects,
          given_exception      TYPE REF TO /usi/cx_bal_root,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    test_objects = prepare_test( i_auto_save_pckg_size = 1 ).

    test_objects-log_dao->method_calls->assert_method_was_not_called( test_objects-log_dao->method_names-save ).

    TRY.
        given_exception = lcl_exception_factory=>get_exception( i_text     = 'Inner exception' ).
        given_exception = lcl_exception_factory=>get_exception( i_text     = 'Outer exception'
                                                                i_previous = given_exception ).

        test_objects-cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                         i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                         i_message_type  = /usi/cl_bal_enum_message_type=>information
                                         i_exception     = given_exception
                                         i_log_previous  = abap_true ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    test_objects-log_dao->method_calls->assert_method_called_n_times(
      i_method_name              = test_objects-log_dao->method_names-save
      i_expected_number_of_calls = 2 ).
  ENDMETHOD.

  METHOD test_auto_save_free_text.
    DATA: test_objects         TYPE ty_test_objects,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    test_objects = prepare_test( i_auto_save_pckg_size = 1 ).

    test_objects-log_dao->method_calls->assert_method_was_not_called( test_objects-log_dao->method_names-save ).

    TRY.
        test_objects-cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                         i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                         i_message_type  = /usi/cl_bal_enum_message_type=>information
                                         i_free_text     = 'Should be saved immediately...' ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    test_objects-log_dao->method_calls->assert_method_was_called( test_objects-log_dao->method_names-save ).
  ENDMETHOD.

  METHOD test_auto_save_message.
    DATA: test_objects         TYPE ty_test_objects,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    test_objects = prepare_test( i_auto_save_pckg_size = 1 ).

    test_objects-log_dao->method_calls->assert_method_was_not_called( test_objects-log_dao->method_names-save ).

    TRY.
        test_objects-cut->add_message( i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
                                       i_detail_level       = /usi/cl_bal_enum_detail_level=>detail_level_1
                                       i_message_type       = /usi/cl_bal_enum_message_type=>information
                                       i_message_class      = '38'
                                       i_message_number     = '000'
                                       i_message_variable_1 = 'Should be saved immediately...' ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    test_objects-log_dao->method_calls->assert_method_was_called( test_objects-log_dao->method_names-save ).
  ENDMETHOD.

  METHOD test_no_early_auto_save.
    DATA: test_objects         TYPE ty_test_objects,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    test_objects = prepare_test( i_auto_save_pckg_size = 2 ).

    TRY.
        test_objects-cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                         i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                         i_message_type  = /usi/cl_bal_enum_message_type=>information
                                         i_free_text     = 'Should not save (1st Message; Package Size = 2)' ).
        test_objects-log_dao->method_calls->assert_method_was_not_called( test_objects-log_dao->method_names-save ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
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

*--------------------------------------------------------------------*
* Unit test: Auto-Save
*--------------------------------------------------------------------*
CLASS lcl_unit_test_multiple_saves DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut             TYPE REF TO /usi/if_bal_logger_state,
          dc_coll_dao_spy TYPE REF TO lcl_data_cont_coll_dao_spy,
          log_dao_spy     TYPE REF TO lcl_dao_spy,
          token           TYPE REF TO /usi/if_bal_token.

    METHODS setup.
    METHODS append_and_save_message.
    METHODS get_last_message_number
      RETURNING
        VALUE(r_result) TYPE /usi/bal_message_number.

    METHODS test_message_number_is_kept FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_multiple_saves IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory         TYPE REF TO /usi/if_bal_cust_eval_factory,
          data_container_classnames TYPE /usi/bal_data_cont_classnames,
          data_container_classname  TYPE /usi/bal_data_cont_classname,
          logger_bl_factory         TYPE REF TO /usi/if_bal_logger_bl_factory.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT log_dao_spy.
    CREATE OBJECT dc_coll_dao_spy.

    data_container_classname = /usi/cl_bal_dc_src_pos_caller=>get_classname( ).
    INSERT data_container_classname INTO TABLE data_container_classnames.

    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_active
      EXPORTING
        i_factory                  = logger_bl_factory
        i_log_level                = /usi/cl_bal_enum_log_level=>everything
        i_auto_save_pckg_size      = 0
        i_log_dao                  = log_dao_spy
        i_data_cont_coll_dao       = dc_coll_dao_spy
        i_token                    = token
        i_relevant_data_containers = data_container_classnames.
  ENDMETHOD.

  METHOD test_message_number_is_kept.
    DATA: expected_message_number TYPE /usi/bal_message_number,
          actual_message_number   TYPE /usi/bal_message_number.

    DO 2 TIMES.
      expected_message_number = sy-index.

      append_and_save_message( ).
      actual_message_number = get_last_message_number( ).

      cl_aunit_assert=>assert_equals( exp = expected_message_number
                                      act = actual_message_number
                                      msg = 'Unexpected message number!' ).
    ENDDO.
  ENDMETHOD.

  METHOD append_and_save_message.
    DATA unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut->add_free_text( i_problem_class   = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level    = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type    = /usi/cl_bal_enum_message_type=>error
                            i_free_text       = `Just a test` ).
        cut->save( token ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD get_last_message_number.
    DATA: method_calls   TYPE /usi/cl_bal_aunit_method_calls=>ty_method_calls,
          message_number TYPE /usi/bal_message_number.
    FIELD-SYMBOLS: <method_call> TYPE /usi/cl_bal_aunit_method_calls=>ty_method_call.

    method_calls = dc_coll_dao_spy->method_calls->get_method_calls(
                     i_method_name = dc_coll_dao_spy->method_names-insert_collection_into_buffer
                   ).
    cl_aunit_assert=>assert_not_initial( act = method_calls
                                         msg = 'Method was not called!' ).

    LOOP AT method_calls ASSIGNING <method_call>.
      <method_call>-method_call->get_parameter(
        EXPORTING
          i_parameter_name  = dc_coll_dao_spy->parameter_names-message_number
        IMPORTING
          e_parameter_value = message_number
      ).

      IF message_number GT r_result.
        r_result = message_number.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
