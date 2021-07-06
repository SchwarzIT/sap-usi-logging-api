interface /USI/IF_BAL_LOGGER_STATE
  public .

  type-pools ABAP .

  types:
    ty_free_text TYPE c LENGTH 200 .

  methods ADD_EXCEPTION
    importing
      !I_PROBLEM_CLASS type ref to /USI/CL_BAL_ENUM_PROBLEM_CLASS
      !I_DETAIL_LEVEL type ref to /USI/CL_BAL_ENUM_DETAIL_LEVEL
      !I_MESSAGE_TYPE type ref to /USI/CL_BAL_ENUM_MESSAGE_TYPE
      !I_EXCEPTION type ref to CX_ROOT
      !I_LOG_PREVIOUS type ABAP_BOOL
      !I_DETAILS type ref to /USI/IF_BAL_MESSAGE_DETAILS optional
      !I_MESSAGE_CONTEXT type BAL_S_CONT optional
    raising
      /USI/CX_BAL_ROOT .
  methods ADD_FREE_TEXT
    importing
      !I_PROBLEM_CLASS type ref to /USI/CL_BAL_ENUM_PROBLEM_CLASS
      !I_DETAIL_LEVEL type ref to /USI/CL_BAL_ENUM_DETAIL_LEVEL
      !I_MESSAGE_TYPE type ref to /USI/CL_BAL_ENUM_MESSAGE_TYPE
      !I_FREE_TEXT type TY_FREE_TEXT
      !I_DETAILS type ref to /USI/IF_BAL_MESSAGE_DETAILS optional
      !I_MESSAGE_CONTEXT type BAL_S_CONT optional
    raising
      /USI/CX_BAL_ROOT .
  methods ADD_MESSAGE
    importing
      !I_PROBLEM_CLASS type ref to /USI/CL_BAL_ENUM_PROBLEM_CLASS
      !I_DETAIL_LEVEL type ref to /USI/CL_BAL_ENUM_DETAIL_LEVEL
      !I_MESSAGE_TYPE type ref to /USI/CL_BAL_ENUM_MESSAGE_TYPE
      !I_MESSAGE_CLASS type SYMSGID
      !I_MESSAGE_NUMBER type SYMSGNO
      !I_MESSAGE_VARIABLE_1 type SYMSGV optional
      !I_MESSAGE_VARIABLE_2 type SYMSGV optional
      !I_MESSAGE_VARIABLE_3 type SYMSGV optional
      !I_MESSAGE_VARIABLE_4 type SYMSGV optional
      !I_DETAILS type ref to /USI/IF_BAL_MESSAGE_DETAILS optional
      !I_MESSAGE_CONTEXT type BAL_S_CONT optional
    raising
      /USI/CX_BAL_ROOT .
  methods CLAIM_OWNERSHIP
    returning
      value(R_RESULT) type ref to /USI/IF_BAL_TOKEN
    raising
      /USI/CX_BAL_ROOT .
  methods FREE
    importing
      !I_TOKEN type ref to /USI/IF_BAL_TOKEN
    raising
      /USI/CX_BAL_ROOT .
  methods SAVE
    importing
      !I_TOKEN type ref to /USI/IF_BAL_TOKEN
    raising
      /USI/CX_BAL_ROOT .
endinterface.
