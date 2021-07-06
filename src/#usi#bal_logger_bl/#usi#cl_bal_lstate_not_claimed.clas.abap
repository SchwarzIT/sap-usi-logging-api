CLASS /usi/cl_bal_lstate_not_claimed DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    INTERFACES /usi/if_bal_logger_state .

    METHODS constructor
      IMPORTING
        !i_factory TYPE REF TO /usi/if_bal_logger_bl_factory .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA factory TYPE REF TO /usi/if_bal_logger_bl_factory.

    METHODS raise_wrong_state_exception
      RAISING
        /usi/cx_bal_root.
ENDCLASS.



CLASS /usi/cl_bal_lstate_not_claimed IMPLEMENTATION.
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
    r_result = factory->get_token( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~free.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD /usi/if_bal_logger_state~save.
    raise_wrong_state_exception( ).
  ENDMETHOD.

  METHOD constructor.
    factory = i_factory.
  ENDMETHOD.

  METHOD raise_wrong_state_exception.
    RAISE EXCEPTION TYPE /usi/cx_bal_not_allowed
      EXPORTING
        textid = /usi/cx_bal_not_allowed=>wrong_logger_state.
  ENDMETHOD.
ENDCLASS.
