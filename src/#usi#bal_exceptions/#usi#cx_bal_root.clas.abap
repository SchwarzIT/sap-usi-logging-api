class /USI/CX_BAL_ROOT definition
  public
  inheriting from /USI/CX_EXCEPTION
  abstract
  create public .

public section.

  constants:
    BEGIN OF free_text,
        msgid TYPE symsgid VALUE '/USI/BAL',
        msgno TYPE symsgno VALUE '000',
        attr1 TYPE scx_attrname VALUE 'PARAM1',
        attr2 TYPE scx_attrname VALUE 'PARAM2',
        attr3 TYPE scx_attrname VALUE 'PARAM3',
        attr4 TYPE scx_attrname VALUE 'PARAM4',
      END OF free_text .

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



CLASS /USI/CX_BAL_ROOT IMPLEMENTATION.


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