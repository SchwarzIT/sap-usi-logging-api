interface /USI/IF_BAL_DELETE_CUSTOM_DATA
  public .


  interfaces IF_BADI_INTERFACE .

  methods DELETE_CUSTOM_DATA
    importing
      !I_LOG_HEADERS type BALHDR_T .
endinterface.
