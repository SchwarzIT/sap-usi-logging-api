class /USI/CX_BAL_DEMO_DUPLICATE definition
  public
  inheriting from /USI/CX_BAL_DEMO_ROOT
  final
  create public .

public section.

  constants:
    BEGIN OF duplicate_task_text,
        msgid TYPE symsgid VALUE '/USI/BAL_DEMO_02',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF duplicate_task_text .
  constants:
    begin of DUPLICATE_TASK_ID,
      msgid type symsgid value '/USI/BAL_DEMO_02',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'PARAM1',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DUPLICATE_TASK_ID .

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



CLASS /USI/CX_BAL_DEMO_DUPLICATE IMPLEMENTATION.


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
