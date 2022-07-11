CLASS /usi/cl_bal_cust_validity_clnt DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cust_validity.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /usi/cl_bal_cust_validity_clnt IMPLEMENTATION.
  METHOD /usi/if_bal_cust_validity~get_maximum_validity_in_days.
    CONSTANTS c_maximum_validity_in_days TYPE /usi/if_bal_cust_validity~ty_maximum_validity_in_days VALUE 3.

    r_result = c_maximum_validity_in_days.
  ENDMETHOD.
ENDCLASS.
