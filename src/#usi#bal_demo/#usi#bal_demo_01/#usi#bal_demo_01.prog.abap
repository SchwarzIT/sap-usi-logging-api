*-----------------------------------------------------------------------*
* Title   | Simple demo for USI Logging API                             *
*-----------------------------------------------------------------------*
* Purpose | Simple demo report for USI Logging API.                     *
*         |                                                             *
*         | Hint: Maintain table /USI/BAL_LV_USER in transaction SM30   *
*         |       and set your user's log level to the maximum (6).     *
*         |       Otherwise this demo is not going to produce any logs. *
*-----------------------------------------------------------------------*
REPORT /usi/bal_demo_01.

CLASS lcl_report DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD run.
    DATA: factory TYPE REF TO /usi/if_bal_factory,
          logger  TYPE REF TO /usi/if_bal_logger,
          token   TYPE REF TO /usi/if_bal_token.

    " Get the factory
    factory = /usi/cl_bal_factory=>get_instance( ).

    " Create a logger
    "
    "   Important: Check your log object in transaction SLG0.
    "              If it has sub objects, the parameter is _MANDATORY_,
    "              otherwise it _MUST NOT_ be used.
    logger = factory->create_new_logger( i_log_object  = '/USI/BAL_DEMO_01'
                                         i_sub_object  = 'RUN'
                                         i_external_id = 'My external ID' ).

    " Claim ownership
    "
    "   Hint: Increase your user-specific log level to at least 5
    "         before running this demo - otherwise nothing will be
    "         logged.
    "
    "         Transaction SM30; Table name /USI/BAL_LV_USER
    token = logger->claim_ownership( ).

    " Add message
    logger->add_free_text( i_message_type = /usi/cl_bal_enum_message_type=>information
                           i_free_text    = 'Hello World!'(t01) ).

    " Save and destroy the log
    logger->save( token ).
    logger->free( token ).

    WRITE AT /1: 'End of demo. Check results using transaction SLG1.'(t02).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  /usi/cl_auth=>check_tcode( ).
  lcl_report=>run( ).
