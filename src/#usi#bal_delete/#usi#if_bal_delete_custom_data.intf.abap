INTERFACE /usi/if_bal_delete_custom_data PUBLIC.

  INTERFACES if_badi_interface.

  "! <h1>Delete API-Specific data for given Logs</h1>
  "!
  "! <p>When a log gets deleted the custom data related to that log has to be deleted as well.</p>
  "!
  "! <p>This can be done by calling this BAdI-method.</p>
  "!
  "! <p><strong>HINT:</strong> The BAdI is called from function module /USI/BAL_DELETE_CUSTOM_DATA and allows
  "! multiple use.</p>
  "!
  "! @parameter i_log_headers | <p class="shorttext synchronized" lang="en">Log headers to delete</p>
  METHODS delete_custom_data
    IMPORTING
      i_log_headers TYPE balhdr_t.

ENDINTERFACE.
