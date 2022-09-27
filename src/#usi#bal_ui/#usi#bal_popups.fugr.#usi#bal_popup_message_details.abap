FUNCTION /usi/bal_popup_message_details.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      I_T_PARAMS STRUCTURE  BAL_S_PAR
*"----------------------------------------------------------------------
  CONSTANTS: BEGIN OF dynpro_start_position,
               column TYPE int1 VALUE 2,
               line   TYPE int1 VALUE 5,
             END   OF dynpro_start_position.

  DATA exception TYPE REF TO /usi/cx_bal_root.

  TRY.
      CREATE OBJECT screen_controller TYPE lcl_log_message_detail
        EXPORTING
          i_message_parameters = i_t_params[].

      CALL SCREEN 2000 STARTING AT dynpro_start_position-column
                                   dynpro_start_position-line.

      screen_controller->free( ).
      CLEAR screen_controller.
    CATCH /usi/cx_bal_root INTO exception.
      MESSAGE exception TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDFUNCTION.
