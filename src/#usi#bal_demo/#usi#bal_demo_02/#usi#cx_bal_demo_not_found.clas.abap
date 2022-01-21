class /USI/CX_BAL_DEMO_NOT_FOUND definition
  public
  inheriting from /USI/CX_BAL_DEMO_ROOT
  final
  create public .

public section.

  constants:
    begin of /USI/CX_BAL_DEMO_NOT_FOUND,
      msgid type symsgid value '/USI/BAL_DEMO_02',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of /USI/CX_BAL_DEMO_NOT_FOUND .
  constants:
    begin of TASK_NOT_FOUND_BY_TEXT,
      msgid type symsgid value '/USI/BAL_DEMO_02',
      msgno type symsgno value '012',
      attr1 type scx_attrname value 'PARAM1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TASK_NOT_FOUND_BY_TEXT .
  constants:
    begin of NO_TASKS_FOUND,
      msgid type symsgid value '/USI/BAL_DEMO_02',
      msgno type symsgno value '013',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_TASKS_FOUND .
  constants:
    begin of TASK_NOT_FOUND_BY_ID,
      msgid type symsgid value '/USI/BAL_DEMO_02',
      msgno type symsgno value '014',
      attr1 type scx_attrname value 'PARAM1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of TASK_NOT_FOUND_BY_ID .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !PARAM1 type SYMSGV optional
      !PARAM2 type SYMSGV optional
      !PARAM3 type SYMSGV optional
      !PARAM4 type SYMSGV optional
      !DETAILS type ref to /USI/IF_EXCEPTION_DETAILS optional .
protected section.
private section.
ENDCLASS.



CLASS /USI/CX_BAL_DEMO_NOT_FOUND IMPLEMENTATION.


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
  IF_T100_MESSAGE~T100KEY = /USI/CX_BAL_DEMO_NOT_FOUND .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
