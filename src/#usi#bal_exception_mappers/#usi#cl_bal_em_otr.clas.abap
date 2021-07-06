CLASS /usi/cl_bal_em_otr DEFINITION
  PUBLIC
  INHERITING FROM /usi/cl_bal_em_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /usi/if_bal_exception_mapper~get_t100_message
         REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /USI/CL_BAL_EM_OTR IMPLEMENTATION.


  METHOD /usi/if_bal_exception_mapper~get_t100_message.

    DATA exception_text TYPE char200.

    exception_text = exception->get_text( ).

    r_result-msgty = 'E'.
    r_result-msgid = /usi/cx_bal_root=>free_text-msgid.
    r_result-msgno = /usi/cx_bal_root=>free_text-msgno.
    r_result-msgv1 = exception_text+000(50).
    r_result-msgv2 = exception_text+050(50).
    r_result-msgv3 = exception_text+100(50).
    r_result-msgv4 = exception_text+150(50).

  ENDMETHOD.
ENDCLASS.
