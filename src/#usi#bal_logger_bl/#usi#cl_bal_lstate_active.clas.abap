CLASS /usi/cl_bal_lstate_active DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_logger_state.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_factory                  | BL-Factory (Internal object creation)
    "! @parameter i_log_level                | Current log level (Customizing)
    "! @parameter i_auto_save_pckg_size      | Package size of auto save (Customizing)
    "! @parameter i_log_dao                  | DAO-Instance for messages (Persistency Layer)
    "! @parameter i_data_cont_coll_dao       | DAO-Instance for data-containers (Persistency Layer)
    "! @parameter i_token                    | Token (Proof of Ownership - needed to authorize critical actions later)
    "! @parameter i_relevant_data_containers | Relevant data containers (Customizing)
    METHODS constructor
      IMPORTING i_factory                  TYPE REF TO /usi/if_bal_logger_bl_factory
                i_log_level                TYPE REF TO /usi/cl_bal_enum_log_level
                i_auto_save_pckg_size      TYPE /usi/bal_auto_save_pckg_size
                i_log_dao                  TYPE REF TO /usi/if_bal_log_dao
                i_data_cont_coll_dao       TYPE REF TO /usi/if_bal_data_cont_coll_dao
                i_token                    TYPE REF TO /usi/if_bal_token
                i_relevant_data_containers TYPE /usi/bal_data_cont_classnames.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_message,
             number                    TYPE /usi/bal_message_number,
             message                   TYPE bal_s_msg,
             data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
           END   OF ty_message,
           ty_messages   TYPE STANDARD TABLE OF ty_message WITH EMPTY KEY,
           ty_exceptions TYPE STANDARD TABLE OF REF TO cx_root WITH EMPTY KEY.

    DATA: factory TYPE REF TO /usi/if_bal_logger_bl_factory,
          token   TYPE REF TO /usi/if_bal_token.

    DATA: BEGIN OF settings,
            log_level                TYPE REF TO /usi/cl_bal_enum_log_level,
            auto_save_pckg_size      TYPE /usi/bal_auto_save_pckg_size,
            relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          END   OF settings.

    DATA: BEGIN OF messages,
            message_buffer         TYPE ty_messages,
            highest_message_number TYPE /usi/bal_message_number,
          END   OF messages.

    DATA: BEGIN OF dao_objects,
            log                       TYPE REF TO /usi/if_bal_log_dao,
            data_container_collection TYPE REF TO /usi/if_bal_data_cont_coll_dao,
          END   OF dao_objects.

    METHODS get_exceptions_inverted_order
      IMPORTING i_main_exception TYPE REF TO cx_root
                i_log_previous   TYPE abap_bool
      RETURNING VALUE(r_result)  TYPE ty_exceptions.

    METHODS insert_message
      IMPORTING i_problem_class             TYPE REF TO /usi/cl_bal_enum_problem_class
                i_detail_level              TYPE REF TO /usi/cl_bal_enum_detail_level
                i_message_type              TYPE REF TO /usi/cl_bal_enum_message_type
                i_message_class             TYPE symsgid
                i_message_number            TYPE symsgno
                i_message_variable_1        TYPE symsgv
                i_message_variable_2        TYPE symsgv
                i_message_variable_3        TYPE symsgv
                i_message_variable_4        TYPE symsgv
                i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col
                i_message_context           TYPE bal_s_cont
      RAISING   /usi/cx_bal_root.

    METHODS get_new_data_container_coll
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container_col.

    METHODS collect_data_containers
      IMPORTING i_source TYPE REF TO /usi/if_bal_message_details
                i_target TYPE REF TO /usi/if_bal_data_container_col.

    METHODS add_cx_src_pos_container
      IMPORTING i_target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col
                i_exception             TYPE REF TO cx_root.

    METHODS add_caller_src_pos_container
      IMPORTING i_target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    METHODS add_callstack_container
      IMPORTING i_target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    METHODS insert_container_if_relevant
      IMPORTING i_data_containter       TYPE REF TO /usi/if_bal_data_container
                i_target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    METHODS is_data_container_relevant
      IMPORTING i_data_container_classname TYPE /usi/bal_data_cont_classname
      RETURNING VALUE(r_result)            TYPE abap_bool.

    METHODS save_log
      RETURNING VALUE(r_result) TYPE balognr
      RAISING   /usi/cx_bal_root.

    METHODS save_data_container_colls
      IMPORTING i_log_number TYPE balognr
      RAISING   /usi/cx_bal_root.

ENDCLASS.


CLASS /usi/cl_bal_lstate_active IMPLEMENTATION.
  METHOD /usi/if_bal_logger_state~add_exception.
    DATA: exception                    TYPE REF TO cx_root,
          exceptions                   TYPE STANDARD TABLE OF REF TO cx_root WITH EMPTY KEY,
          exception_mapper             TYPE REF TO /usi/if_bal_exception_mapper,
          target_data_cont_coll        TYPE REF TO /usi/if_bal_data_container_col,
          unfiltered_cx_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col,
          the_message                  TYPE symsg.

    IF settings-log_level->is_problem_class_relevant( i_problem_class ) <> abap_true.
      RETURN.
    ENDIF.

    exceptions = get_exceptions_inverted_order( i_main_exception = i_exception
                                                i_log_previous   = i_log_previous ).

    LOOP AT exceptions INTO exception.
      target_data_cont_coll = get_new_data_container_coll( ).

      " Add containers, that were passed by the caller (Prio 1)
      IF exception = i_exception.
        collect_data_containers( i_source = i_details
                                 i_target = target_data_cont_coll ).
      ENDIF.

      " Add containers, that were appended to the exception (Prio 2)
      unfiltered_cx_data_cont_coll = NEW /usi/cl_bal_dc_collection( ).
      exception_mapper = factory->get_exception_mapper( exception ).
      exception_mapper->get_data_containers( unfiltered_cx_data_cont_coll ).
      collect_data_containers( i_source = unfiltered_cx_data_cont_coll
                               i_target = target_data_cont_coll ).

      " Add automatic containers (Prio 3)
      IF exception = i_exception.
        add_caller_src_pos_container( target_data_cont_coll ).
        add_callstack_container( target_data_cont_coll ).
      ENDIF.
      add_cx_src_pos_container( i_target_data_cont_coll = target_data_cont_coll
                                i_exception             = exception ).

      the_message = exception_mapper->get_t100_message( ).
      insert_message( i_problem_class             = i_problem_class
                      i_detail_level              = i_detail_level
                      i_message_type              = i_message_type
                      i_message_class             = the_message-msgid
                      i_message_number            = the_message-msgno
                      i_message_variable_1        = the_message-msgv1
                      i_message_variable_2        = the_message-msgv2
                      i_message_variable_3        = the_message-msgv3
                      i_message_variable_4        = the_message-msgv4
                      i_data_container_collection = target_data_cont_coll
                      i_message_context           = i_message_context ).
    ENDLOOP.
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~add_free_text.
    DATA target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    IF settings-log_level->is_problem_class_relevant( i_problem_class ) <> abap_true.
      RETURN.
    ENDIF.

    target_data_cont_coll = get_new_data_container_coll( ).
    collect_data_containers( i_source = i_details
                             i_target = target_data_cont_coll ).

    add_caller_src_pos_container( target_data_cont_coll ).
    add_callstack_container( target_data_cont_coll ).

    insert_message( i_problem_class             = i_problem_class
                    i_detail_level              = i_detail_level
                    i_message_type              = i_message_type
                    i_message_class             = /usi/cx_bal_root=>free_text-msgid
                    i_message_number            = /usi/cx_bal_root=>free_text-msgno
                    i_message_variable_1        = i_free_text+0(50)
                    i_message_variable_2        = i_free_text+50(50)
                    i_message_variable_3        = i_free_text+100(50)
                    i_message_variable_4        = i_free_text+150(50)
                    i_data_container_collection = target_data_cont_coll
                    i_message_context           = i_message_context ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~add_message.
    DATA target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    IF settings-log_level->is_problem_class_relevant( i_problem_class ) <> abap_true.
      RETURN.
    ENDIF.

    target_data_cont_coll = get_new_data_container_coll( ).
    collect_data_containers( i_source = i_details
                             i_target = target_data_cont_coll ).

    add_caller_src_pos_container( target_data_cont_coll ).
    add_callstack_container( target_data_cont_coll ).

    insert_message( i_problem_class             = i_problem_class
                    i_detail_level              = i_detail_level
                    i_message_type              = i_message_type
                    i_message_class             = i_message_class
                    i_message_number            = i_message_number
                    i_message_variable_1        = i_message_variable_1
                    i_message_variable_2        = i_message_variable_2
                    i_message_variable_3        = i_message_variable_3
                    i_message_variable_4        = i_message_variable_4
                    i_data_container_collection = target_data_cont_coll
                    i_message_context           = i_message_context ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~claim_ownership.
    RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
      EXPORTING textid = /usi/cx_bal_not_allowed=>wrong_logger_state.
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~free.
    IF token->is_equal( i_token ) = abap_true.
      dao_objects-log->free( ).
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
        EXPORTING textid = /usi/cx_bal_not_allowed=>only_owner_may_do_that.
    ENDIF.
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~save.
    DATA log_number TYPE balognr.

    IF token->is_equal( i_token ) <> abap_true.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
        EXPORTING textid = /usi/cx_bal_not_allowed=>only_owner_may_do_that.
    ENDIF.

    TRY.
        log_number = save_log( ).
        save_data_container_colls( log_number ).
        CLEAR messages-message_buffer.
      CLEANUP.
        CLEAR messages-message_buffer.
    ENDTRY.
  ENDMETHOD.

  METHOD add_caller_src_pos_container.
    CONSTANTS caller_level TYPE i VALUE 4.

    DATA: source_code_position TYPE /usi/bal_source_code_position,
          new_data_container   TYPE REF TO /usi/cl_bal_dc_src_pos_caller,
          callstack            TYPE abap_callstack.

    FIELD-SYMBOLS <callstack_line> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING max_level = caller_level
      IMPORTING callstack = callstack.
    ASSIGN callstack[ caller_level ] TO <callstack_line>.

    source_code_position-program_name = <callstack_line>-mainprogram.
    source_code_position-include_name = <callstack_line>-include.
    source_code_position-source_line  = <callstack_line>-line.
    new_data_container = NEW #( i_source_code_position = source_code_position ).

    insert_container_if_relevant( i_data_containter       = new_data_container
                                  i_target_data_cont_coll = i_target_data_cont_coll ).
  ENDMETHOD.

  METHOD add_callstack_container.
    CONSTANTS number_of_api_internal_calls TYPE i VALUE 3.

    DATA: new_data_container TYPE REF TO /usi/cl_bal_dc_callstack,
          callstack          TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING callstack = callstack.
    DELETE callstack TO number_of_api_internal_calls.

    new_data_container = NEW #( i_callstack = callstack ).

    insert_container_if_relevant( i_data_containter       = new_data_container
                                  i_target_data_cont_coll = i_target_data_cont_coll ).
  ENDMETHOD.

  METHOD add_cx_src_pos_container.
    DATA: source_code_position TYPE /usi/bal_source_code_position,
          new_data_container   TYPE REF TO /usi/cl_bal_dc_src_pos_cx.

    i_exception->get_source_position( IMPORTING program_name = source_code_position-program_name
                                                include_name = source_code_position-include_name
                                                source_line  = source_code_position-source_line ).

    new_data_container = NEW #( i_source_code_position = source_code_position ).

    insert_container_if_relevant( i_data_containter       = new_data_container
                                  i_target_data_cont_coll = i_target_data_cont_coll ).
  ENDMETHOD.

  METHOD collect_data_containers.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_container            TYPE REF TO /usi/if_bal_data_container,
          data_containers           TYPE /usi/bal_data_containers.

    FIELD-SYMBOLS <data_container> TYPE REF TO /usi/if_bal_data_container.

    IF i_source IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        data_container_collection ?= i_source.
        data_containers = data_container_collection->get_data_containers( ).
      CATCH cx_sy_move_cast_error.
        CLEAR data_container_collection.
    ENDTRY.

    TRY.
        data_container ?= i_source.
        INSERT data_container INTO TABLE data_containers.
      CATCH cx_sy_move_cast_error.
        CLEAR data_container.
    ENDTRY.

    LOOP AT data_containers ASSIGNING <data_container>.
      insert_container_if_relevant( i_data_containter       = <data_container>
                                    i_target_data_cont_coll = i_target ).
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    factory                               = i_factory.
    token                                 = i_token.

    settings-log_level                = i_log_level.
    settings-auto_save_pckg_size      = i_auto_save_pckg_size.
    settings-relevant_data_containers = i_relevant_data_containers.

    dao_objects-data_container_collection = i_data_cont_coll_dao.
    dao_objects-log                       = i_log_dao.
  ENDMETHOD.

  METHOD get_exceptions_inverted_order.
    DATA previous TYPE REF TO cx_root.

    INSERT i_main_exception INTO TABLE r_result.

    " Invert the sort order of the exceptions, as previous was raised before main.
    IF i_log_previous = abap_true.
      previous = i_main_exception->previous.
      WHILE previous IS BOUND.
        INSERT previous INTO r_result INDEX 1.
        previous = previous->previous.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.

  METHOD get_new_data_container_coll.
    r_result = NEW /usi/cl_bal_dc_collection( ).
  ENDMETHOD.

  METHOD insert_container_if_relevant.
    DATA data_container_classname TYPE /usi/bal_data_cont_classname.

    data_container_classname = i_data_containter->get_classname( ).
    IF is_data_container_relevant( data_container_classname ) = abap_false.
      RETURN.
    ENDIF.

    i_target_data_cont_coll->insert( i_data_containter ).
  ENDMETHOD.

  METHOD insert_message.
    CONSTANTS:
      BEGIN OF callback_function,
        function_module TYPE baluef VALUE '/USI/BAL_POPUP_MESSAGE_DETAILS',
        callback_type   TYPE baluet VALUE 'F',
      END   OF callback_function.

    DATA: callback_parameter TYPE bal_s_par,
          message            TYPE ty_message.

    IF    i_message_class IS INITIAL
       OR i_message_type  IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDIF.

    messages-highest_message_number = messages-highest_message_number + 1.

    message-number            = messages-highest_message_number.
    message-message-detlevel  = i_detail_level->value.
    message-message-probclass = i_problem_class->value.
    message-message-msgty     = i_message_type->value.
    message-message-msgid     = i_message_class.
    message-message-msgno     = i_message_number.
    message-message-msgv1     = i_message_variable_1.
    message-message-msgv2     = i_message_variable_2.
    message-message-msgv3     = i_message_variable_3.
    message-message-msgv4     = i_message_variable_4.
    message-message-context   = i_message_context.

    IF i_data_container_collection->has_data_containers( ) = abap_true.
      message-data_container_collection         = i_data_container_collection.

      message-message-params-callback-userexitf = callback_function-function_module.
      message-message-params-callback-userexitt = callback_function-callback_type.
      callback_parameter-parname  = /usi/cl_bal_enum_message_param=>message_number->value.
      callback_parameter-parvalue = message-number.
      INSERT callback_parameter INTO TABLE message-message-params-t_par.
    ENDIF.

    INSERT message INTO TABLE messages-message_buffer.

    IF     settings-auto_save_pckg_size      > 0
       AND lines( messages-message_buffer ) >= settings-auto_save_pckg_size.
      TRY.
          /usi/if_bal_logger_state~save( token ).
        CATCH /usi/cx_bal_root.
          " Can never happen, as we are definitely passing the right token
          RETURN.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD is_data_container_relevant.
    r_result = boolc( line_exists( settings-relevant_data_containers[ table_line = i_data_container_classname ] ) ).
  ENDMETHOD.

  METHOD save_data_container_colls.
    DATA: serialized_data_cont_coll TYPE /usi/bal_xml_string,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root,
          unexpected_exception_text TYPE string,
          unsaved_data_exists       TYPE abap_bool.

    FIELD-SYMBOLS <message> TYPE ty_message.

    LOOP AT messages-message_buffer ASSIGNING <message> WHERE data_container_collection IS BOUND.
      IF <message>-data_container_collection->has_data_containers( ) = abap_false.
        CONTINUE.
      ENDIF.
      serialized_data_cont_coll = <message>-data_container_collection->serialize( ).
      TRY.
          dao_objects-data_container_collection->insert_collection_into_buffer(
              i_log_number                = i_log_number
              i_message_number            = <message>-number
              i_serialized_data_cont_coll = serialized_data_cont_coll ).

          unsaved_data_exists = abap_true.
        CATCH /usi/cx_bal_root INTO unexpected_exception.
          unexpected_exception_text = unexpected_exception->get_text( ).
          ASSERT ID /usi/bal_log_writer
                 FIELDS unexpected_exception_text
                 CONDITION unexpected_exception IS NOT BOUND.

          CONTINUE.
      ENDTRY.
    ENDLOOP.
    IF unsaved_data_exists = abap_true.
      dao_objects-data_container_collection->save_buffer_to_db( ).
    ENDIF.
  ENDMETHOD.

  METHOD save_log.
    DATA: unexpected_exception      TYPE REF TO /usi/cx_bal_root,
          unexpected_exception_text TYPE string,
          unsaved_data_exists       TYPE abap_bool.

    FIELD-SYMBOLS <message> TYPE ty_message.

    IF messages-message_buffer IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>log_is_empty.
    ENDIF.

    LOOP AT messages-message_buffer ASSIGNING <message>.
      TRY.
          dao_objects-log->add_message( <message>-message ).
          unsaved_data_exists = abap_true.
        CATCH /usi/cx_bal_root INTO unexpected_exception.
          unexpected_exception_text = unexpected_exception->get_text( ).
          ASSERT ID /usi/bal_log_writer
                 FIELDS unexpected_exception_text
                 CONDITION unexpected_exception IS NOT BOUND.

          " Don't save the containers, if the message could not be saved!
          CLEAR <message>-data_container_collection.
      ENDTRY.
    ENDLOOP.
    IF unsaved_data_exists = abap_true.
      dao_objects-log->save( ).
    ENDIF.

    r_result = dao_objects-log->get_log_number( ).
  ENDMETHOD.
ENDCLASS.
