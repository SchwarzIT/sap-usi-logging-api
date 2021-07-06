INTERFACE /usi/if_bal_log_dao
  PUBLIC .


  DATA log_header TYPE bal_s_log READ-ONLY .

  METHODS add_message
    IMPORTING
      !i_message TYPE bal_s_msg
    RAISING
      /usi/cx_bal_root .

  METHODS free .

  METHODS get_log_number
    RETURNING
      VALUE(r_result) TYPE balognr
    RAISING
      /usi/cx_bal_root .

  METHODS save
    RAISING
      /usi/cx_bal_root .
ENDINTERFACE.
