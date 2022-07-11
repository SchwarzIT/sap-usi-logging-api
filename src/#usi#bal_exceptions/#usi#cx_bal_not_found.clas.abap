class /USI/CX_BAL_NOT_FOUND definition
  public
  inheriting from /USI/CX_BAL_ROOT
  create public .

public section.

  constants:
    begin of NO_DB_ENTRIES_FOUND,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '004',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_DB_ENTRIES_FOUND .
  constants:
    begin of TEXTPOOL_NOT_FOUND,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '020',
      attr1 type scx_attrname value 'PARAM1',
      attr2 type scx_attrname value 'PARAM2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TEXTPOOL_NOT_FOUND .
  constants:
    begin of TEXT_NOT_FOUND_IN_TEXTPOOL,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '021',
      attr1 type scx_attrname value 'PARAM1',
      attr2 type scx_attrname value 'PARAM2',
      attr3 type scx_attrname value 'PARAM3',
      attr4 type scx_attrname value 'PARAM4',
    end of TEXT_NOT_FOUND_IN_TEXTPOOL .

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



CLASS /USI/CX_BAL_NOT_FOUND IMPLEMENTATION.


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
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
