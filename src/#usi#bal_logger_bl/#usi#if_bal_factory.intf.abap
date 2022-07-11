INTERFACE /usi/if_bal_factory PUBLIC.

  "! <h1>Create new logger</h1>
  "!
  "! <p>First call creates a new logger. Subsequent calls will return the same instance until the owner of that
  "! instance explicitly destroys it.</p>
  "!
  "! <p><strong>CAUTION:</strong> If a log object has sub objects, that parameter becomes mandatory!
  "! Otherwise it must not be used.</p>
  "!
  "! <p>Since the API is meant to be extremely error tolerant passing an invalid combination of log object and
  "! sub object will create a logger for:
  "!   <ul>
  "!     <li>log object <strong>/USI/BAL</strong></li>
  "!     <li>sub object <strong>WRONG_API_CALLS</strong></li>
  "!   </ul>
  "! </p>
  "!
  "! @parameter i_log_object | Log Object (Defined in SLG0)
  "! @parameter i_sub_object | Sub Object (Defined in SLG0)
  "! @parameter i_external_id | External ID of the log (Filter in SLG1 - use ID of processed object if feasible)
  "! @parameter r_result | The log writer
  METHODS create_new_logger
    IMPORTING
      i_log_object    TYPE balobj_d
      i_sub_object    TYPE balsubobj OPTIONAL
      i_external_id   TYPE balnrext  OPTIONAL
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_logger.

  "! <h1>Re-obtain previously created log writer</h1>
  "!
  "! <p>Once created you can re-obtain your logger where ever you like by calling this method.</p>
  "!
  "! <p>If no logger was created so far, the method will implicitly create a logger for:
  "!   <ul>
  "!     <li>log object <strong>/USI/BAL</strong></li>
  "!     <li>sub object <strong>WRONG_API_CALLS</strong></li>
  "!   </ul>
  "! </p>
  "!
  "! @parameter r_result | The log writer
  METHODS get_existing_logger
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_logger.

ENDINTERFACE.
