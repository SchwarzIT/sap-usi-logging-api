interface /USI/IF_BAL_TOKEN
  public .

  type-pools ABAP .

  methods IS_EQUAL
    importing
      !I_TOKEN type ref to /USI/IF_BAL_TOKEN
    returning
      value(R_RESULT) type ABAP_BOOL .
endinterface.
