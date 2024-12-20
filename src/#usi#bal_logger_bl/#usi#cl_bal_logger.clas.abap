CLASS /usi/cl_bal_logger DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_logger.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_factory                  | BL-Factory (Internal object creation)
    "! @parameter i_relevant_data_containers | Relevant data containers (Customizing)
    "! @parameter i_log_level                | Current log level (Customizing)
    "! @parameter i_auto_save_pckg_size      | Package size of auto save (Customizing)
    "! @parameter i_log_dao                  | DAO-Instance for messages (Persistency Layer)
    "! @parameter i_data_cont_coll_dao       | DAO-Instance for data-containers (Persistency Layer)
    METHODS constructor
      IMPORTING i_factory                  TYPE REF TO /usi/if_bal_logger_bl_factory
                i_relevant_data_containers TYPE /usi/bal_data_cont_classnames
                i_log_level                TYPE REF TO /usi/cl_bal_enum_log_level
                i_auto_save_pckg_size      TYPE /usi/bal_auto_save_pckg_size
                i_log_dao                  TYPE REF TO /usi/if_bal_log_dao
                i_data_cont_coll_dao       TYPE REF TO /usi/if_bal_data_cont_coll_dao.

  PRIVATE SECTION.
    DATA: factory                       TYPE REF TO /usi/if_bal_logger_bl_factory,
          log_level                     TYPE REF TO /usi/cl_bal_enum_log_level,
          log_dao                       TYPE REF TO /usi/if_bal_log_dao,
          data_container_collection_dao TYPE REF TO /usi/if_bal_data_cont_coll_dao,
          relevant_data_containers      TYPE /usi/bal_data_cont_classnames,
          state                         TYPE REF TO /usi/if_bal_logger_state,
          auto_save_pckg_size           TYPE /usi/bal_auto_save_pckg_size.

ENDCLASS.


CLASS /usi/cl_bal_logger IMPLEMENTATION.
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

  METHOD /usi/if_bal_logger~claim_ownership.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    TRY.
        r_result = state->claim_ownership( ).

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

  METHOD /usi/if_bal_logger~free.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    TRY.
        state->free( i_token ).

        " State transition: 'Active' -> 'Invalidated'
        state = NEW /usi/cl_bal_lstate_invalidated( ).

        RAISE EVENT /usi/if_bal_logger~instance_invalidated.
      CATCH /usi/cx_bal_root INTO exception.
        " Not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_logger~save.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    TRY.
        state->save( i_token ).
      CATCH /usi/cx_bal_root INTO exception.
        " Error during operation or not allowed in current state!
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.
    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    factory                       = i_factory.
    log_level                     = i_log_level.
    log_dao                       = i_log_dao.
    data_container_collection_dao = i_data_cont_coll_dao.
    relevant_data_containers      = i_relevant_data_containers.
    auto_save_pckg_size           = i_auto_save_pckg_size.

    " Set initial state: 'Not claimed'
    state = NEW /usi/cl_bal_lstate_not_claimed( i_factory = factory ).
  ENDMETHOD.
ENDCLASS.
