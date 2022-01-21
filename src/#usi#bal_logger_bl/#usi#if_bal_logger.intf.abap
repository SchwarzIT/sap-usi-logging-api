interface /USI/IF_BAL_LOGGER
  public .

  type-pools ABAP .

  events INSTANCE_INVALIDATED .

  methods ADD_EXCEPTION
    importing
      !I_PROBLEM_CLASS type ref to /USI/CL_BAL_ENUM_PROBLEM_CLASS default /USI/CL_BAL_ENUM_PROBLEM_CLASS=>OTHER
      !I_DETAIL_LEVEL type ref to /USI/CL_BAL_ENUM_DETAIL_LEVEL default /USI/CL_BAL_ENUM_DETAIL_LEVEL=>DETAIL_LEVEL_1
      !I_MESSAGE_TYPE type ref to /USI/CL_BAL_ENUM_MESSAGE_TYPE default /USI/CL_BAL_ENUM_MESSAGE_TYPE=>ERROR
      !I_EXCEPTION type ref to CX_ROOT
      !I_LOG_PREVIOUS type ABAP_BOOL default ABAP_TRUE
      !I_DETAILS type ref to /USI/IF_BAL_MESSAGE_DETAILS optional
      !I_MESSAGE_CONTEXT type BAL_S_CONT optional .
  methods ADD_FREE_TEXT
    importing
      !I_PROBLEM_CLASS type ref to /USI/CL_BAL_ENUM_PROBLEM_CLASS default /USI/CL_BAL_ENUM_PROBLEM_CLASS=>OTHER
      !I_DETAIL_LEVEL type ref to /USI/CL_BAL_ENUM_DETAIL_LEVEL default /USI/CL_BAL_ENUM_DETAIL_LEVEL=>DETAIL_LEVEL_1
      !I_MESSAGE_TYPE type ref to /USI/CL_BAL_ENUM_MESSAGE_TYPE default /USI/CL_BAL_ENUM_MESSAGE_TYPE=>ERROR
      !I_FREE_TEXT type CSEQUENCE
      !I_DETAILS type ref to /USI/IF_BAL_MESSAGE_DETAILS optional
      !I_MESSAGE_CONTEXT type BAL_S_CONT optional .
  methods ADD_MESSAGE
    importing
      !I_PROBLEM_CLASS type ref to /USI/CL_BAL_ENUM_PROBLEM_CLASS default /USI/CL_BAL_ENUM_PROBLEM_CLASS=>OTHER
      !I_DETAIL_LEVEL type ref to /USI/CL_BAL_ENUM_DETAIL_LEVEL default /USI/CL_BAL_ENUM_DETAIL_LEVEL=>DETAIL_LEVEL_1
      !I_MESSAGE_TYPE type ref to /USI/CL_BAL_ENUM_MESSAGE_TYPE default /USI/CL_BAL_ENUM_MESSAGE_TYPE=>ERROR
      !I_MESSAGE_CLASS type SYMSGID
      !I_MESSAGE_NUMBER type SYMSGNO
      !I_MESSAGE_VARIABLE_1 type SIMPLE optional
      !I_MESSAGE_VARIABLE_2 type SIMPLE optional
      !I_MESSAGE_VARIABLE_3 type SIMPLE optional
      !I_MESSAGE_VARIABLE_4 type SIMPLE optional
      !I_DETAILS type ref to /USI/IF_BAL_MESSAGE_DETAILS optional
      !I_MESSAGE_CONTEXT type BAL_S_CONT optional .
  methods ADD_MESSAGE_FROM_SY_FIELDS
    importing
      !I_PROBLEM_CLASS type ref to /USI/CL_BAL_ENUM_PROBLEM_CLASS default /USI/CL_BAL_ENUM_PROBLEM_CLASS=>OTHER
      !I_DETAIL_LEVEL type ref to /USI/CL_BAL_ENUM_DETAIL_LEVEL default /USI/CL_BAL_ENUM_DETAIL_LEVEL=>DETAIL_LEVEL_1
      !I_DETAILS type ref to /USI/IF_BAL_MESSAGE_DETAILS optional
      !I_MESSAGE_CONTEXT type BAL_S_CONT optional .
  methods CLAIM_OWNERSHIP
    returning
      value(R_RESULT) type ref to /USI/IF_BAL_TOKEN .
  methods FREE
    importing
      !I_TOKEN type ref to /USI/IF_BAL_TOKEN .
  methods SAVE
    importing
      !I_TOKEN type ref to /USI/IF_BAL_TOKEN .
endinterface.
