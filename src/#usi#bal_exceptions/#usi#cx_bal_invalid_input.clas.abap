class /USI/CX_BAL_INVALID_INPUT definition
  public
  inheriting from /USI/CX_BAL_ROOT
  create public .

public section.

  constants:
    begin of /USI/CX_BAL_INVALID_INPUT,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '005',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of /USI/CX_BAL_INVALID_INPUT .
  constants:
    begin of DUPLICATE_BUFFER_ENTRY,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'PARAM1',
      attr2 type scx_attrname value 'PARAM2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DUPLICATE_BUFFER_ENTRY .
  constants:
    begin of LOG_IS_EMPTY,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '014',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of LOG_IS_EMPTY .
  constants:
    begin of ITAB_REQUIRED,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ITAB_REQUIRED .
  constants:
    begin of UNSUPPORTED_LINE_TYPE,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNSUPPORTED_LINE_TYPE .
  constants:
    begin of UNSUPPORTED_FIELD_TYPE,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNSUPPORTED_FIELD_TYPE .
  constants:
    begin of TRANSFORMATION_ERROR,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '040',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TRANSFORMATION_ERROR .
  constants:
    begin of UNSUPPORTED_STRUCTURE,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '050',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of UNSUPPORTED_STRUCTURE .

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



CLASS /USI/CX_BAL_INVALID_INPUT IMPLEMENTATION.


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
  IF_T100_MESSAGE~T100KEY = /USI/CX_BAL_INVALID_INPUT .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
