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
    begin of DATA_CONT_USE_DDIC_LINE_TYPE,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '006',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DATA_CONT_USE_DDIC_LINE_TYPE .
  constants:
    begin of DUPLICATE_BUFFER_ENTRY,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '001',
      attr1 type scx_attrname value 'PARAM1',
      attr2 type scx_attrname value 'PARAM2',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DUPLICATE_BUFFER_ENTRY .

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