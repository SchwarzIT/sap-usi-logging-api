CLASS /usi/cl_bal_lstate_invalidated DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_logger_state.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS raise_wrong_state_exception
      RAISING
        /usi/cx_bal_root.

ENDCLASS.



CLASS /usi/cl_bal_lstate_invalidated IMPLEMENTATION.
  METHOD /usi/if_bal_logger_state~add_exception.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~add_free_text.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~add_message.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~claim_ownership.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~display.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~free.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~save.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD raise_wrong_state_exception.
    RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
      EXPORTING
        textid = /usi/cx_bal_not_allowed=>wrong_logger_state.
  ENDMETHOD.
ENDCLASS.
