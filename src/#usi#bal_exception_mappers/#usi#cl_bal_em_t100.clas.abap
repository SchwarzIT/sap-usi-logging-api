CLASS /usi/cl_bal_em_t100 DEFINITION
  PUBLIC
  INHERITING FROM /usi/cl_bal_em_base
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !i_exception TYPE REF TO cx_root
      RAISING
        /usi/cx_bal_root .

    METHODS /usi/if_bal_exception_mapper~get_t100_message
         REDEFINITION .

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA t100_exception TYPE REF TO if_t100_message .

ENDCLASS.



CLASS /USI/CL_BAL_EM_T100 IMPLEMENTATION.


  METHOD /usi/if_bal_exception_mapper~get_t100_message.
    cl_message_helper=>set_msg_vars_for_if_t100_msg( text = t100_exception ).
    r_result-msgty = 'E'.
    r_result-msgid = sy-msgid.
    r_result-msgno = sy-msgno.
    r_result-msgv1 = sy-msgv1.
    r_result-msgv2 = sy-msgv2.
    r_result-msgv3 = sy-msgv3.
    r_result-msgv4 = sy-msgv4.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( i_exception ).

    TRY.
        t100_exception ?= i_exception.
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
          EXPORTING
            textid = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
