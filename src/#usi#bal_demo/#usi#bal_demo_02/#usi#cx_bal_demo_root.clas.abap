class /USI/CX_BAL_DEMO_ROOT definition
  public
  inheriting from /USI/CX_EXCEPTION
  abstract
  create public .

public section.

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
protected section.
private section.
ENDCLASS.



CLASS /USI/CX_BAL_DEMO_ROOT IMPLEMENTATION.


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
