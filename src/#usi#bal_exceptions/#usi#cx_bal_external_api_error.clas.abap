class /USI/CX_BAL_EXTERNAL_API_ERROR definition
  public
  inheriting from /USI/CX_BAL_ROOT
  final
  create public .

public section.

  constants:
    begin of /USI/CX_BAL_EXTERNAL_API_ERROR,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '017',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of /USI/CX_BAL_EXTERNAL_API_ERROR .
  constants:
    begin of LOG_CREATE_ERROR,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '011',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of LOG_CREATE_ERROR .
  constants:
    begin of MESSAGE_NOT_LOGGED,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '008',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MESSAGE_NOT_LOGGED .
  constants:
    begin of LOG_SAVE_ERROR,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '012',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of LOG_SAVE_ERROR .
  constants:
    begin of SAVE_LOG_FIRST,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of SAVE_LOG_FIRST .

  "! <h1>Constructor</h1>
  "!
  "! @parameter TEXTID | Text-ID (T100-Key)
  "! @parameter PREVIOUS | Previous exception
  "! @parameter PARAM1 | 1st message parameter
  "! @parameter PARAM2 | 2nd message parameter
  "! @parameter PARAM3 | 3rd message parameter
  "! @parameter PARAM4 | 4th message parameter
  "! @parameter DETAILS | Details (Single data-container or data-container-collection)
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !PARAM1 type SYMSGV optional
      !PARAM2 type SYMSGV optional
      !PARAM3 type SYMSGV optional
      !PARAM4 type SYMSGV optional
      !DETAILS type ref to /USI/IF_EXCEPTION_DETAILS optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /USI/CX_BAL_EXTERNAL_API_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
PARAM1 = PARAM1
PARAM2 = PARAM2
PARAM3 = PARAM3
PARAM4 = PARAM4
DETAILS = DETAILS
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = /USI/CX_BAL_EXTERNAL_API_ERROR .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
