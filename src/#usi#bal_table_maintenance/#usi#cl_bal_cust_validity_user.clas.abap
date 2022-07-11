CLASS /usi/cl_bal_cust_validity_user DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cust_validity.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /usi/cl_bal_cust_validity_user IMPLEMENTATION.
  METHOD /usi/if_bal_cust_validity~get_maximum_validity_in_days.
    CONSTANTS c_maximum_validity_in_days TYPE /usi/if_bal_cust_validity~ty_maximum_validity_in_days VALUE 14.

    r_result = c_maximum_validity_in_days.
  ENDMETHOD.
ENDCLASS.
