CLASS /usi/cl_bal_token DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_token.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /usi/cl_bal_token IMPLEMENTATION.
  METHOD /usi/if_bal_token~is_equal.
    IF     i_token IS BOUND
       AND i_token  = me.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
