INTERFACE /usi/if_bal_log_dao PUBLIC.

  DATA: log_header TYPE bal_s_log  READ-ONLY,
        log_handle TYPE balloghndl READ-ONLY.

  "! <h1>Add message to buffer</h1>
  "!
  "! @parameter i_message        | Message
  "! @raising   /usi/cx_bal_root | Error in standard API
  METHODS add_message
    IMPORTING i_message TYPE bal_s_msg
    RAISING   /usi/cx_bal_root.

  "! <h1>Destroy log and free memory</h1>
  METHODS free.

  "! <h1>Get log number</h1>
  "!
  "! <p><strong>IMPORTANT:</strong> this will only work after saving the log for the first time!</p>
  "!
  "! @parameter r_result         | Log number (As in DB-Table BALHDR)
  "! @raising   /usi/cx_bal_root | Error (Not saved?)
  METHODS get_log_number
    RETURNING VALUE(r_result) TYPE balognr
    RAISING   /usi/cx_bal_root.

  "! <h1>Save buffer to DB</h1>
  "!
  "! @raising /usi/cx_bal_root | Error in standard API
  METHODS save
    RAISING /usi/cx_bal_root.

ENDINTERFACE.
