INTERFACE /usi/if_bal_logger PUBLIC.

  EVENTS instance_invalidated.

  "! <h1>Add exception to internal buffer</h1>
  "!
  "! @parameter i_problem_class   | Problem class (Severity of the message)
  "! @parameter i_detail_level    | Detail level
  "! @parameter i_message_type    | Message type
  "! @parameter i_exception       | Exception
  "! @parameter i_log_previous    | Flag: Log previous exceptions as well?
  "! @parameter i_details         | Details (Data container / Data container collection)
  "! @parameter i_message_context | Context structure
  METHODS add_exception
    IMPORTING i_problem_class   TYPE REF TO /usi/cl_bal_enum_problem_class DEFAULT /usi/cl_bal_enum_problem_class=>other
              i_detail_level    TYPE REF TO /usi/cl_bal_enum_detail_level  DEFAULT /usi/cl_bal_enum_detail_level=>detail_level_1
              i_message_type    TYPE REF TO /usi/cl_bal_enum_message_type  DEFAULT /usi/cl_bal_enum_message_type=>error
              i_exception       TYPE REF TO cx_root
              i_log_previous    TYPE abap_bool                             DEFAULT abap_true
              i_details         TYPE REF TO /usi/if_bal_message_details    OPTIONAL
              i_message_context TYPE bal_s_cont                            OPTIONAL.

  "! <h1>Add free text to internal buffer</h1>
  "!
  "! @parameter i_problem_class   | Problem class (Severity of the message)
  "! @parameter i_detail_level    | Detail level
  "! @parameter i_message_type    | Message type
  "! @parameter i_free_text       | Free text
  "! @parameter i_details         | Details (Data container / Data container collection)
  "! @parameter i_message_context | Context structure
  METHODS add_free_text
    IMPORTING i_problem_class   TYPE REF TO /usi/cl_bal_enum_problem_class DEFAULT /usi/cl_bal_enum_problem_class=>other
              i_detail_level    TYPE REF TO /usi/cl_bal_enum_detail_level  DEFAULT /usi/cl_bal_enum_detail_level=>detail_level_1
              i_message_type    TYPE REF TO /usi/cl_bal_enum_message_type  DEFAULT /usi/cl_bal_enum_message_type=>error
              i_free_text       TYPE csequence
              i_details         TYPE REF TO /usi/if_bal_message_details    OPTIONAL
              i_message_context TYPE bal_s_cont                            OPTIONAL.

  "! <h1>Add message to internal buffer</h1>
  "!
  "! @parameter i_problem_class      | Problem class (Severity of the message)
  "! @parameter i_detail_level       | Detail level
  "! @parameter i_message_type       | Message type
  "! @parameter i_message_class      | Message class
  "! @parameter i_message_number     | Message number
  "! @parameter i_message_variable_1 | 1st message variable
  "! @parameter i_message_variable_2 | 2nd message variable
  "! @parameter i_message_variable_3 | 3rd message variable
  "! @parameter i_message_variable_4 | 4th message variable
  "! @parameter i_details            | Details (Data container / Data container collection)
  "! @parameter i_message_context    | Context structure
  METHODS add_message
    IMPORTING i_problem_class      TYPE REF TO /usi/cl_bal_enum_problem_class DEFAULT /usi/cl_bal_enum_problem_class=>other
              i_detail_level       TYPE REF TO /usi/cl_bal_enum_detail_level  DEFAULT /usi/cl_bal_enum_detail_level=>detail_level_1
              i_message_type       TYPE REF TO /usi/cl_bal_enum_message_type  DEFAULT /usi/cl_bal_enum_message_type=>error
              i_message_class      TYPE symsgid
              i_message_number     TYPE symsgno
              i_message_variable_1 TYPE simple                                OPTIONAL
              i_message_variable_2 TYPE simple                                OPTIONAL
              i_message_variable_3 TYPE simple                                OPTIONAL
              i_message_variable_4 TYPE simple                                OPTIONAL
              i_details            TYPE REF TO /usi/if_bal_message_details    OPTIONAL
              i_message_context    TYPE bal_s_cont                            OPTIONAL.

  "! <h1>Add message to internal buffer (From SY-MSG*-Fields)</h1>
  "!
  "! @parameter i_problem_class   | Problem class (Severity of the message)
  "! @parameter i_detail_level    | Detail level
  "! @parameter i_details         | Details (Data container / Data container collection)
  "! @parameter i_message_context | Context structure
  METHODS add_message_from_sy_fields
    IMPORTING i_problem_class   TYPE REF TO /usi/cl_bal_enum_problem_class DEFAULT /usi/cl_bal_enum_problem_class=>other
              i_detail_level    TYPE REF TO /usi/cl_bal_enum_detail_level  DEFAULT /usi/cl_bal_enum_detail_level=>detail_level_1
              i_details         TYPE REF TO /usi/if_bal_message_details    OPTIONAL
              i_message_context TYPE bal_s_cont                            OPTIONAL.

  "! <h1>Claim ownership of a log writer instance</h1>
  "!
  "! <p>The first call will return the "real" token, that will make the caller the actual owner of the log writer
  "! instance. Subsequent calls will return "bogus-tokens" that are basically good for nothing.</p>
  "!
  "! @parameter r_result | Token (Proof of ownership)
  METHODS claim_ownership
    RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_token.

  "! <h1>Display the log</h1>
  "!
  "! <p>Works for 'active' logger instances. 'Active' means, that the logger has already been claimed by method
  "! claim_ownership( ) but not yet invalidated by method free( ).</p>
  "!
  "! @raising /usi/cx_bal_root | Error
  METHODS display
    RAISING /usi/cx_bal_root.

  "! <h1>Destructor</h1>
  "!
  "! <p>The log writer is currently a singleton held by the factory. Once created it can only be destroyed by its
  "! owner. The token will be needed to authorize this action.</p>
  "!
  "! @parameter i_token | Token (Proof of ownership)
  METHODS free
    IMPORTING i_token TYPE REF TO /usi/if_bal_token.

  "! <h1>Save the log</h1>
  "!
  "! <p>Saves the log to the database. The token will be needed to authorize this action.</p>
  "!
  "! @parameter i_token | Token (Proof of ownership)
  METHODS save
    IMPORTING i_token TYPE REF TO /usi/if_bal_token.

ENDINTERFACE.
