interface /USI/IF_BAL_DATA_CONT_COLL_DAO
  public .


  methods INSERT_COLLECTION_INTO_BUFFER
    importing
      !I_LOG_NUMBER type BALOGNR
      !I_MESSAGE_NUMBER type /USI/BAL_MESSAGE_NUMBER
      !I_SERIALIZED_DATA_CONT_COLL type /USI/BAL_XML_STRING
    raising
      /USI/CX_BAL_ROOT .
  methods SAVE_BUFFER_TO_DB
    raising
      /USI/CX_BAL_ROOT .
  methods GET_COLLECTION
    importing
      !I_LOG_NUMBER type BALOGNR
      !I_MESSAGE_NUMBER type /USI/BAL_MESSAGE_NUMBER
    returning
      value(R_RESULT) type /USI/BAL_XML_STRING
    raising
      /USI/CX_BAL_ROOT .
  methods DELETE_COLLECTIONS
    importing
      !I_LOG_NUMBERS type BAL_T_LOGN .
endinterface.
