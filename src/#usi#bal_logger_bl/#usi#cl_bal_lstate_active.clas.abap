CLASS /usi/cl_bal_lstate_active DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    INTERFACES /usi/if_bal_logger_state .

    METHODS constructor
      IMPORTING
        !i_factory                  TYPE REF TO /usi/if_bal_logger_bl_factory
        !i_log_level                TYPE REF TO /usi/cl_bal_enum_log_level
        !i_auto_save_pckg_size      TYPE /usi/bal_auto_save_pckg_size
        !i_log_dao                  TYPE REF TO /usi/if_bal_log_dao
        !i_data_cont_coll_dao       TYPE REF TO /usi/if_bal_data_cont_coll_dao
        !i_token                    TYPE REF TO /usi/if_bal_token
        !i_relevant_data_containers TYPE /usi/bal_data_cont_classnames .
  PROTECTED SECTION.
private section.

  aliases SAVE
    for /USI/IF_BAL_LOGGER_STATE~SAVE .

  types:
    BEGIN OF ty_message,
        number                    TYPE /usi/bal_message_number,
        message                   TYPE bal_s_msg,
        data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
      END   OF ty_message .
  types:
    ty_messages   TYPE STANDARD TABLE OF ty_message WITH NON-UNIQUE DEFAULT KEY .
  types:
    ty_exceptions TYPE STANDARD TABLE OF REF TO cx_root WITH NON-UNIQUE DEFAULT KEY .

  data FACTORY type ref to /USI/IF_BAL_LOGGER_BL_FACTORY .
  data TOKEN type ref to /USI/IF_BAL_TOKEN .
  data:
    BEGIN OF settings,
            log_level                TYPE REF TO /usi/cl_bal_enum_log_level,
            auto_save_pckg_size      TYPE /usi/bal_auto_save_pckg_size,
            relevant_data_containers TYPE /usi/bal_data_cont_classnames,
          END   OF settings .
  data:
    BEGIN OF messages,
            message_buffer         TYPE ty_messages,
            highest_message_number TYPE /usi/bal_message_number,
          END   OF messages .
  data:
    BEGIN OF dao_objects,
            log                       TYPE REF TO /usi/if_bal_log_dao,
            data_container_collection TYPE REF TO /usi/if_bal_data_cont_coll_dao,
          END   OF dao_objects .

  methods GET_EXCEPTIONS_INVERTED_ORDER
    importing
      !I_MAIN_EXCEPTION type ref to CX_ROOT
      !I_LOG_PREVIOUS type ABAP_BOOL
    returning
      value(R_RESULT) type TY_EXCEPTIONS .
  methods INSERT_MESSAGE
    importing
      !I_PROBLEM_CLASS type ref to /USI/CL_BAL_ENUM_PROBLEM_CLASS
      !I_DETAIL_LEVEL type ref to /USI/CL_BAL_ENUM_DETAIL_LEVEL
      !I_MESSAGE_TYPE type ref to /USI/CL_BAL_ENUM_MESSAGE_TYPE
      !I_MESSAGE_CLASS type SYMSGID
      !I_MESSAGE_NUMBER type SYMSGNO
      !I_MESSAGE_VARIABLE_1 type SYMSGV
      !I_MESSAGE_VARIABLE_2 type SYMSGV
      !I_MESSAGE_VARIABLE_3 type SYMSGV
      !I_MESSAGE_VARIABLE_4 type SYMSGV
      !I_DATA_CONTAINER_COLLECTION type ref to /USI/IF_BAL_DATA_CONTAINER_COL
      !I_MESSAGE_CONTEXT type BAL_S_CONT
    raising
      /USI/CX_BAL_ROOT .
  methods GET_NEW_DATA_CONTAINER_COLL
    returning
      value(R_RESULT) type ref to /USI/IF_BAL_DATA_CONTAINER_COL .
  methods COLLECT_DATA_CONTAINERS
    importing
      !I_SOURCE type ref to /USI/IF_BAL_MESSAGE_DETAILS
      !I_TARGET type ref to /USI/IF_BAL_DATA_CONTAINER_COL .
  methods ADD_CX_SRC_POS_CONTAINER
    importing
      !I_TARGET_DATA_CONT_COLL type ref to /USI/IF_BAL_DATA_CONTAINER_COL
      !I_EXCEPTION type ref to CX_ROOT .
  methods ADD_CALLER_SRC_POS_CONTAINER
    importing
      !I_TARGET_DATA_CONT_COLL type ref to /USI/IF_BAL_DATA_CONTAINER_COL .
  methods ADD_CALLSTACK_CONTAINER
    importing
      !I_TARGET_DATA_CONT_COLL type ref to /USI/IF_BAL_DATA_CONTAINER_COL .
  methods INSERT_CONTAINER_IF_RELEVANT
    importing
      !I_DATA_CONTAINTER type ref to /USI/IF_BAL_DATA_CONTAINER
      !I_TARGET_DATA_CONT_COLL type ref to /USI/IF_BAL_DATA_CONTAINER_COL .
  methods IS_DATA_CONTAINER_RELEVANT
    importing
      !I_DATA_CONTAINER_CLASSNAME type /USI/BAL_DATA_CONT_CLASSNAME
    returning
      value(R_RESULT) type ABAP_BOOL .
  methods SAVE_LOG
    returning
      value(R_RESULT) type BALOGNR
    raising
      /USI/CX_BAL_ROOT .
  methods SAVE_DATA_CONTAINER_COLLS
    importing
      !I_LOG_NUMBER type BALOGNR
    raising
      /USI/CX_BAL_ROOT .
ENDCLASS.



CLASS /USI/CL_BAL_LSTATE_ACTIVE IMPLEMENTATION.


  METHOD /usi/if_bal_logger_state~add_exception.
    DATA: exception                    TYPE REF TO cx_root,
          exceptions                   TYPE STANDARD TABLE OF REF TO cx_root WITH NON-UNIQUE DEFAULT KEY,
          exception_mapper             TYPE REF TO /usi/if_bal_exception_mapper,
          target_data_cont_coll        TYPE REF TO /usi/if_bal_data_container_col,
          unfiltered_cx_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col,
          the_message                  TYPE symsg.

    IF settings-log_level->is_problem_class_relevant( i_problem_class ) NE abap_true.
      RETURN.
    ENDIF.

    exceptions = get_exceptions_inverted_order(
                   i_main_exception = i_exception
                   i_log_previous   = i_log_previous
                 ).

    LOOP AT exceptions INTO exception.
      target_data_cont_coll = get_new_data_container_coll( ).

      " Add containers, that were passed by the caller (Prio 1)
      IF exception EQ i_exception.
        collect_data_containers(
          i_source = i_details
          i_target = target_data_cont_coll
        ).
      ENDIF.

      " Add containers, that were appended to the exception (Prio 2)
      CREATE OBJECT unfiltered_cx_data_cont_coll TYPE /usi/cl_bal_dc_collection.
      exception_mapper = factory->get_exception_mapper( exception ).
      exception_mapper->get_data_containers( unfiltered_cx_data_cont_coll ).
      collect_data_containers(
        i_source = unfiltered_cx_data_cont_coll
        i_target = target_data_cont_coll
      ).

      " Add automatic containers (Prio 3)
      IF exception EQ i_exception.
        add_caller_src_pos_container( target_data_cont_coll ).
        add_callstack_container( target_data_cont_coll ).
      ENDIF.
      add_cx_src_pos_container(
        i_target_data_cont_coll = target_data_cont_coll
        i_exception             = exception
      ).

      the_message = exception_mapper->get_t100_message( ).
      insert_message(
        i_problem_class             = i_problem_class
        i_detail_level              = i_detail_level
        i_message_type              = i_message_type
        i_message_class             = the_message-msgid
        i_message_number            = the_message-msgno
        i_message_variable_1        = the_message-msgv1
        i_message_variable_2        = the_message-msgv2
        i_message_variable_3        = the_message-msgv3
        i_message_variable_4        = the_message-msgv4
        i_data_container_collection = target_data_cont_coll
        i_message_context           = i_message_context
      ).
    ENDLOOP.
  ENDMETHOD.


  METHOD /usi/if_bal_logger_state~add_free_text.
    DATA: target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    IF settings-log_level->is_problem_class_relevant( i_problem_class ) NE abap_true.
      RETURN.
    ENDIF.

    target_data_cont_coll = get_new_data_container_coll( ).
    collect_data_containers(
      i_source = i_details
      i_target = target_data_cont_coll
    ).

    add_caller_src_pos_container( target_data_cont_coll ).
    add_callstack_container( target_data_cont_coll ).

    insert_message(
      i_problem_class             = i_problem_class
      i_detail_level              = i_detail_level
      i_message_type              = i_message_type
      i_message_class             = /usi/cx_bal_root=>free_text-msgid
      i_message_number            = /usi/cx_bal_root=>free_text-msgno
      i_message_variable_1        = i_free_text+0(50)
      i_message_variable_2        = i_free_text+50(50)
      i_message_variable_3        = i_free_text+100(50)
      i_message_variable_4        = i_free_text+150(50)
      i_data_container_collection = target_data_cont_coll
      i_message_context           = i_message_context
    ).
  ENDMETHOD.


  METHOD /usi/if_bal_logger_state~add_message.
    DATA: target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    IF settings-log_level->is_problem_class_relevant( i_problem_class ) NE abap_true.
      RETURN.
    ENDIF.

    target_data_cont_coll = get_new_data_container_coll( ).
    collect_data_containers(
      i_source = i_details
      i_target = target_data_cont_coll
    ).

    add_caller_src_pos_container( target_data_cont_coll ).
    add_callstack_container( target_data_cont_coll ).

    insert_message(
      i_problem_class             = i_problem_class
      i_detail_level              = i_detail_level
      i_message_type              = i_message_type
      i_message_class             = i_message_class
      i_message_number            = i_message_number
      i_message_variable_1        = i_message_variable_1
      i_message_variable_2        = i_message_variable_2
      i_message_variable_3        = i_message_variable_3
      i_message_variable_4        = i_message_variable_4
      i_data_container_collection = target_data_cont_coll
      i_message_context           = i_message_context
    ).
  ENDMETHOD.


  METHOD /usi/if_bal_logger_state~claim_ownership.
    RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
      EXPORTING
        textid = /usi/cx_bal_not_allowed=>wrong_logger_state.
  ENDMETHOD.


  METHOD /usi/if_bal_logger_state~free.
    IF token->is_equal( i_token ) EQ abap_true.
      dao_objects-log->free( ).
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
        EXPORTING
          textid = /usi/cx_bal_not_allowed=>only_owner_may_do_that.
    ENDIF.
  ENDMETHOD.


  METHOD /usi/if_bal_logger_state~save.
    DATA: log_number TYPE balognr.

    IF token->is_equal( i_token ) NE abap_true.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
        EXPORTING
          textid = /usi/cx_bal_not_allowed=>only_owner_may_do_that.
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
    CONSTANTS: caller_level TYPE i VALUE 4.

    DATA: source_code_position TYPE /usi/bal_source_code_position,
          new_data_container   TYPE REF TO /usi/cl_bal_dc_src_pos_caller,
          callstack            TYPE abap_callstack.

    FIELD-SYMBOLS: <callstack_line> TYPE abap_callstack_line.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level = caller_level
      IMPORTING
        callstack = callstack.
    READ TABLE callstack ASSIGNING <callstack_line> INDEX caller_level.

    source_code_position-program_name = <callstack_line>-mainprogram.
    source_code_position-include_name = <callstack_line>-include.
    source_code_position-source_line  = <callstack_line>-line.
    CREATE OBJECT new_data_container
      EXPORTING
        i_source_code_position = source_code_position.

    insert_container_if_relevant(
      i_data_containter       = new_data_container
      i_target_data_cont_coll = i_target_data_cont_coll
    ).
  ENDMETHOD.


  METHOD add_callstack_container.
    CONSTANTS: number_of_api_internal_calls TYPE i VALUE 3.

    DATA: new_data_container TYPE REF TO /usi/cl_bal_dc_callstack,
          callstack          TYPE abap_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = callstack.
    DELETE callstack TO number_of_api_internal_calls.

    CREATE OBJECT new_data_container
      EXPORTING
        i_callstack = callstack.

    insert_container_if_relevant(
      i_data_containter       = new_data_container
      i_target_data_cont_coll = i_target_data_cont_coll
    ).
  ENDMETHOD.


  METHOD add_cx_src_pos_container.
    DATA: source_code_position TYPE /usi/bal_source_code_position,
          new_data_container   TYPE REF TO /usi/cl_bal_dc_src_pos_cx.

    i_exception->get_source_position(
      IMPORTING
        program_name = source_code_position-program_name
        include_name = source_code_position-include_name
        source_line  = source_code_position-source_line
    ).

    CREATE OBJECT new_data_container
      EXPORTING
        i_source_code_position = source_code_position.

    insert_container_if_relevant(
      i_data_containter       = new_data_container
      i_target_data_cont_coll = i_target_data_cont_coll
    ).
  ENDMETHOD.


  METHOD collect_data_containers.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_container            TYPE REF TO /usi/if_bal_data_container,
          data_containers           TYPE /usi/bal_data_containers.

    FIELD-SYMBOLS: <data_container> TYPE REF TO  /usi/if_bal_data_container.

    IF i_source IS BOUND.
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
        insert_container_if_relevant(
          i_data_containter       = <data_container>
          i_target_data_cont_coll = i_target
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    factory                               = i_factory.
    token                                 = i_token.

    settings-log_level                    = i_log_level.
    settings-auto_save_pckg_size          = i_auto_save_pckg_size.
    settings-relevant_data_containers     = i_relevant_data_containers.

    dao_objects-data_container_collection = i_data_cont_coll_dao.
    dao_objects-log                       = i_log_dao.
  ENDMETHOD.


  METHOD get_exceptions_inverted_order.
    DATA previous TYPE REF TO cx_root.

    INSERT i_main_exception INTO TABLE r_result.

    " Invert the sort order of the exceptions, as previous was raised before main.
    IF i_log_previous EQ abap_true.
      previous = i_main_exception->previous.
      WHILE previous IS BOUND.
        INSERT previous INTO r_result INDEX 1.
        previous = previous->previous.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.


  METHOD get_new_data_container_coll.
    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_collection.
  ENDMETHOD.


  METHOD insert_container_if_relevant.
    DATA data_container_classname TYPE /usi/bal_data_cont_classname.

    data_container_classname = i_data_containter->get_classname( ).
    IF is_data_container_relevant( data_container_classname ) EQ abap_false.
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

    IF i_message_class IS INITIAL OR
       i_message_type  IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING
          textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDIF.

    ADD 1 TO messages-highest_message_number.
    message-number               = messages-highest_message_number.

    message-message-detlevel     = i_detail_level->value.
    message-message-probclass    = i_problem_class->value.
    message-message-msgty        = i_message_type->value.
    message-message-msgid        = i_message_class.
    message-message-msgno        = i_message_number.
    message-message-msgv1        = i_message_variable_1.
    message-message-msgv2        = i_message_variable_2.
    message-message-msgv3        = i_message_variable_3.
    message-message-msgv4        = i_message_variable_4.
    message-message-context      = i_message_context.

    IF i_data_container_collection->has_data_containers( ) EQ abap_true.
      message-data_container_collection         = i_data_container_collection.

      message-message-params-callback-userexitf = callback_function-function_module.
      message-message-params-callback-userexitt = callback_function-callback_type.
      callback_parameter-parname                = /usi/cl_bal_enum_message_param=>message_number->value.
      callback_parameter-parvalue               = message-number.
      INSERT callback_parameter INTO TABLE message-message-params-t_par.
    ENDIF.

    INSERT message INTO TABLE messages-message_buffer.

    IF settings-auto_save_pckg_size GT 0 AND
       settings-auto_save_pckg_size LE lines( messages-message_buffer ).
      TRY.
          save( token ).
        CATCH /usi/cx_bal_root.
          " Can never happen, as we are definitely passing the right token
          RETURN.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD is_data_container_relevant.
    READ TABLE settings-relevant_data_containers
      TRANSPORTING NO FIELDS
      WITH KEY table_line = i_data_container_classname.
    IF sy-subrc EQ 0.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD save_data_container_colls.
    DATA: serialized_data_cont_coll TYPE /usi/bal_xml_string,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root,
          unexpected_exception_text TYPE string,
          unsaved_data_exists       TYPE abap_bool.

    FIELD-SYMBOLS: <message> TYPE ty_message.

    LOOP AT messages-message_buffer ASSIGNING <message> WHERE data_container_collection IS BOUND.
      CHECK <message>-data_container_collection->has_data_containers( ) EQ abap_true.
      serialized_data_cont_coll = <message>-data_container_collection->serialize( ).
      TRY.
          dao_objects-data_container_collection->insert_collection_into_buffer(
            i_log_number                = i_log_number
            i_message_number            = <message>-number
            i_serialized_data_cont_coll = serialized_data_cont_coll
          ).
          unsaved_data_exists = abap_true.
        CATCH /usi/cx_bal_root INTO unexpected_exception.
          unexpected_exception_text = unexpected_exception->get_text( ).
          ASSERT ID /usi/bal_log_writer
            FIELDS unexpected_exception_text
            CONDITION unexpected_exception IS NOT BOUND.

          CONTINUE.
      ENDTRY.
    ENDLOOP.
    IF unsaved_data_exists EQ abap_true.
      dao_objects-data_container_collection->save_buffer_to_db( ).
    ENDIF.
  ENDMETHOD.


  METHOD save_log.
    DATA: unexpected_exception      TYPE REF TO /usi/cx_bal_root,
          unexpected_exception_text TYPE string,
          unsaved_data_exists       TYPE abap_bool.

    FIELD-SYMBOLS: <message> TYPE ty_message.

    IF messages-message_buffer IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING
          textid = /usi/cx_bal_invalid_input=>log_is_empty.
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
    IF unsaved_data_exists EQ abap_true.
      dao_objects-log->save( ).
    ENDIF.

    r_result = dao_objects-log->get_log_number( ).
  ENDMETHOD.
ENDCLASS.
