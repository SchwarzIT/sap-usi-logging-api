INTERFACE /usi/if_bal_logger_bl_factory PUBLIC.

  "! <h1>Get exception mapper for exception</h1>
  "!
  "! @parameter i_exception | to-be-mapped exception
  "! @parameter r_result | exception mapper
  METHODS get_exception_mapper
    IMPORTING
      i_exception     TYPE REF TO cx_root
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_exception_mapper.

  "! <h1>Create a token</h1>
  "!
  "! <p>Critical actions such as destroying the log must not be done by anyone except the owner of that log.</p>
  "!
  "! <p>Tokens are used as a proof of ownership.</p>
  "!
  "! @parameter r_result | Token
  METHODS get_token
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_token.

ENDINTERFACE.
