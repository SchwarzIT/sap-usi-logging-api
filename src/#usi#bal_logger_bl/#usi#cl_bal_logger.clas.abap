CLASS /usi/cl_bal_logger DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_logger.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_factory                  | BL-Factory (Internal object creation)
    "! @parameter i_parent_logger            | Parent Logger
    "! @parameter i_relevant_data_containers | Relevant data containers (Customizing)
    "! @parameter i_log_level                | Current log level (Customizing)
    "! @parameter i_auto_save_pckg_size      | Package size of auto save (Customizing)
    "! @parameter i_log_dao                  | DAO-Instance for messages (Persistency Layer)
    "! @parameter i_data_cont_coll_dao       | DAO-Instance for data-containers (Persistency Layer)
    METHODS constructor
      IMPORTING i_factory                  TYPE REF TO /usi/if_bal_logger_bl_factory
                i_parent_logger            TYPE REF TO /usi/cl_bal_logger OPTIONAL
                i_relevant_data_containers TYPE /usi/bal_data_cont_classnames
                i_log_level                TYPE REF TO /usi/cl_bal_enum_log_level
                i_auto_save_pckg_size      TYPE /usi/bal_auto_save_pckg_size
                i_log_dao                  TYPE REF TO /usi/if_bal_log_dao
                i_data_cont_coll_dao       TYPE REF TO /usi/if_bal_data_cont_coll_dao.

  PRIVATE SECTION.
    DATA: factory                       TYPE REF TO /usi/if_bal_logger_bl_factory,
          parent_logger                 TYPE REF TO /usi/cl_bal_logger,
          child_loggers                 TYPE STANDARD TABLE OF REF TO /usi/cl_bal_logger WITH EMPTY KEY,
          log_level                     TYPE REF TO /usi/cl_bal_enum_log_level,
          log_dao                       TYPE REF TO /usi/if_bal_log_dao,
          data_container_collection_dao TYPE REF TO /usi/if_bal_data_cont_coll_dao,
          relevant_data_containers      TYPE /usi/bal_data_cont_classnames,
          state                         TYPE REF TO /usi/if_bal_logger_state,
          auto_save_pckg_size           TYPE /usi/bal_auto_save_pckg_size,
          token                         TYPE REF TO /usi/if_bal_token.

    METHODS on_child_logger_invalidation
      FOR EVENT instance_invalidated OF /usi/if_bal_logger
      IMPORTING sender.

    METHODS on_parent_logger_invalidation
      FOR EVENT instance_invalidated OF /usi/if_bal_logger.

    METHODS register_child_logger
      IMPORTING i_child_logger TYPE REF TO /usi/cl_bal_logger.

    METHODS register_parent_logger
      IMPORTING i_parent_logger TYPE REF TO /usi/cl_bal_logger.

ENDCLASS.


CLASS /usi/cl_bal_logger IMPLEMENTATION.
  METHOD constructor.
    factory                       = i_factory.
    log_level                     = i_log_level.
    log_dao                       = i_log_dao.
    data_container_collection_dao = i_data_cont_coll_dao.
    relevant_data_containers      = i_relevant_data_containers.
    auto_save_pckg_size           = i_auto_save_pckg_size.

    " Set initial state: 'Not claimed'
    state = NEW /usi/cl_bal_lstate_not_claimed( i_factory = factory ).

    IF i_parent_logger IS BOUND.
      register_parent_logger( i_parent_logger ).
      i_parent_logger->register_child_logger( me ).
    ENDIF.
  ENDMETHOD.

  METHOD register_parent_logger.
    parent_logger = i_parent_logger.
    SET HANDLER on_parent_logger_invalidation FOR i_parent_logger.

    " Claim child loggers on creation
    "
    " When creating a child logger two trace messages are written: One in the parent and one in the child log.
    " Both contain data containers that provide a navigation to the respective logs. Writing these messages can
    " only work, if both loggers have been claimed.
    /usi/if_bal_logger~claim_ownership( ).

    IF 1 = 0. MESSAGE i070(/usi/bal) WITH space space space space. ENDIF.
    /usi/if_bal_logger~add_message(
        i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
        i_message_type       = /usi/cl_bal_enum_message_type=>information
        i_message_class      = '/USI/BAL'
        i_message_number     = '070'
        i_message_variable_1 = i_parent_logger->log_dao->log_header-object
        i_message_variable_2 = i_parent_logger->log_dao->log_header-subobject
        i_message_variable_3 = i_parent_logger->log_dao->log_header-extnumber+00(50)
        i_message_variable_4 = i_parent_logger->log_dao->log_header-extnumber+50(50)
        i_details            = NEW /usi/cl_bal_dc_parent_log( i_parent_logger->log_dao->log_handle ) ).
  ENDMETHOD.

  METHOD on_parent_logger_invalidation.
    CLEAR parent_logger.
  ENDMETHOD.

  METHOD register_child_logger.
    INSERT i_child_logger INTO TABLE child_loggers.

    SET HANDLER on_child_logger_invalidation FOR i_child_logger.

    IF 1 = 0. MESSAGE i071(/usi/bal) WITH space space space space. ENDIF.
    /usi/if_bal_logger~add_message(
        i_problem_class      = /usi/cl_bal_enum_problem_class=>very_important
        i_message_type       = /usi/cl_bal_enum_message_type=>information
        i_message_class      = '/USI/BAL'
        i_message_number     = '071'
        i_message_variable_1 = i_child_logger->log_dao->log_header-object
        i_message_variable_2 = i_child_logger->log_dao->log_header-subobject
        i_message_variable_3 = i_child_logger->log_dao->log_header-extnumber+00(50)
        i_message_variable_4 = i_child_logger->log_dao->log_header-extnumber+50(50)
        i_details            = NEW /usi/cl_bal_dc_child_log( i_child_logger->log_dao->log_handle ) ).
  ENDMETHOD.

  METHOD on_child_logger_invalidation.
    DELETE child_loggers WHERE table_line = sender.
  ENDMETHOD.

  METHOD /usi/if_bal_logger~claim_ownership.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    TRY.
        token = state->claim_ownership( ).
        r_result = token.

        " State transition: 'Not claimed' -> 'Active'
        state = NEW /usi/cl_bal_lstate_active( i_factory                  = factory
                                               i_log_level                = log_level
                                               i_auto_save_pckg_size      = auto_save_pckg_size
                                               i_log_dao                  = log_dao
                                               i_data_cont_coll_dao       = data_container_collection_dao
                                               i_token                    = r_result
                                               i_relevant_data_containers = relevant_data_containers ).
      CATCH /usi/cx_bal_root INTO exception.
        " Not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.

        " Return dummy-token, that grants no authorizations.
        r_result = factory->get_token( ).
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_logger~add_exception.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    TRY.
        state->add_exception( i_problem_class   = i_problem_class
                              i_detail_level    = i_detail_level
                              i_message_type    = i_message_type
                              i_exception       = i_exception
                              i_log_previous    = i_log_previous
                              i_details         = i_details
                              i_message_context = i_message_context ).
      CATCH /usi/cx_bal_root INTO exception.
        " Error during operation or not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_logger~add_free_text.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string,
          free_text      TYPE /usi/if_bal_logger_state=>ty_free_text.

    TRY.
        free_text = i_free_text.

        state->add_free_text( i_problem_class   = i_problem_class
                              i_detail_level    = i_detail_level
                              i_message_type    = i_message_type
                              i_free_text       = free_text
                              i_details         = i_details
                              i_message_context = i_message_context ).
      CATCH /usi/cx_bal_root INTO exception.
        " Error during operation or not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_logger~add_message.
    DATA: exception          TYPE REF TO /usi/cx_bal_root,
          exception_text     TYPE string,
          message_variable_1 TYPE symsgv,
          message_variable_2 TYPE symsgv,
          message_variable_3 TYPE symsgv,
          message_variable_4 TYPE symsgv.

    WRITE: i_message_variable_1 TO message_variable_1 LEFT-JUSTIFIED,
           i_message_variable_2 TO message_variable_2 LEFT-JUSTIFIED,
           i_message_variable_3 TO message_variable_3 LEFT-JUSTIFIED,
           i_message_variable_4 TO message_variable_4 LEFT-JUSTIFIED.

    TRY.
        state->add_message( i_problem_class      = i_problem_class
                            i_detail_level       = i_detail_level
                            i_message_type       = i_message_type
                            i_message_class      = i_message_class
                            i_message_number     = i_message_number
                            i_message_variable_1 = message_variable_1
                            i_message_variable_2 = message_variable_2
                            i_message_variable_3 = message_variable_3
                            i_message_variable_4 = message_variable_4
                            i_details            = i_details
                            i_message_context    = i_message_context ).
      CATCH /usi/cx_bal_root INTO exception.
        " Error during operation or not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_logger~add_message_from_sy_fields.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string,
          message_type   TYPE REF TO /usi/cl_bal_enum_message_type.

    " Check sy-msgty and sy-msgid
    ASSERT ID /usi/bal_log_writer
           FIELDS sy-msgty
                  sy-msgid
           CONDITION     sy-msgty IS NOT INITIAL
                     AND sy-msgid IS NOT INITIAL.
    IF    sy-msgty IS INITIAL
       OR sy-msgid IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        message_type = /usi/cl_bal_enum_message_type=>get_by_value( sy-msgty ).
      CATCH /usi/cx_bal_root INTO exception.
        " SY-MSGTY contains an invalid message type!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
        RETURN.
    ENDTRY.

    " Log the message
    TRY.
        state->add_message( i_problem_class      = i_problem_class
                            i_detail_level       = i_detail_level
                            i_message_type       = message_type
                            i_message_class      = sy-msgid
                            i_message_number     = sy-msgno
                            i_message_variable_1 = sy-msgv1
                            i_message_variable_2 = sy-msgv2
                            i_message_variable_3 = sy-msgv3
                            i_message_variable_4 = sy-msgv4
                            i_details            = i_details
                            i_message_context    = i_message_context ).
      CATCH /usi/cx_bal_root INTO exception.
        " Error during operation or not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_logger~display.
    state->display( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger~save.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    TRY.
        state->save( i_token ).

        LOOP AT child_loggers REFERENCE INTO DATA(child_logger).
          child_logger->*->/usi/if_bal_logger~save( child_logger->*->token ).
        ENDLOOP.
      CATCH /usi/cx_bal_root INTO exception.
        " Error during operation or not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_logger~free.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    TRY.
        state->free( i_token ).

        " State transition: 'Active' -> 'Invalidated'
        state = NEW /usi/cl_bal_lstate_invalidated( ).

        LOOP AT child_loggers REFERENCE INTO DATA(child_logger).
          child_logger->*->/usi/if_bal_logger~free( child_logger->*->token ).
        ENDLOOP.

        RAISE EVENT /usi/if_bal_logger~instance_invalidated.
      CATCH /usi/cx_bal_root INTO exception.
        " Not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
