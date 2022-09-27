INTERFACE /usi/if_bal_token PUBLIC.

  "! <h1>Is same instance?</h1>
  "!
  "! @parameter i_token | Token to compare
  "! @parameter r_result | abap_true, if same instance
  METHODS is_equal
    IMPORTING
      i_token         TYPE REF TO /usi/if_bal_token
    RETURNING
      VALUE(r_result) TYPE abap_bool.

ENDINTERFACE.
