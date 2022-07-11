INTERFACE /usi/if_bal_cust_validity PUBLIC.

  INTERFACES if_badi_interface.

  TYPES ty_maximum_validity_in_days TYPE int1.

  "! <h1>Get maximum validity in days for increased log level (BAdI-Method)</h1>
  "!
  "! @parameter r_result | Maximum validity in days
  METHODS get_maximum_validity_in_days
    RETURNING
      VALUE(r_result) TYPE ty_maximum_validity_in_days.

ENDINTERFACE.
