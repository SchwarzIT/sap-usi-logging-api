class /USI/CX_BAL_TYPE_MISMATCH definition
  public
  inheriting from /USI/CX_BAL_ROOT
  create public .

public section.

  constants:
    begin of /USI/CX_BAL_TYPE_MISMATCH,
      msgid type symsgid value '/USI/BAL',
      msgno type symsgno value '003',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of /USI/CX_BAL_TYPE_MISMATCH .

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



CLASS /USI/CX_BAL_TYPE_MISMATCH IMPLEMENTATION.


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
  IF_T100_MESSAGE~T100KEY = /USI/CX_BAL_TYPE_MISMATCH .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
