CLASS /usi/cl_bal_aunit_exception DEFINITION PUBLIC FINAL CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    CLASS cl_aunit_assert DEFINITION LOAD.
    CLASS if_aunit_constants DEFINITION LOAD.

    "! <h1>Aborts current test</h1>
    "!
    "! @parameter i_exception | Exception (Text will be displayed)
    "! @parameter i_quit | Quit what (Test method / class)
    CLASS-METHODS abort_on_unexpected_exception
      IMPORTING
        i_exception TYPE REF TO cx_root
        i_quit      TYPE aunit_flowctrl DEFAULT if_aunit_constants=>class.

    "! <h1>Make current test fail</h1>
    "!
    "! @parameter i_exception | Exception (Text will be displayed)
    "! @parameter i_quit | Quit what (Test method / class)
    CLASS-METHODS fail_on_unexpected_exception
      IMPORTING
        i_exception TYPE REF TO cx_root
        i_quit      TYPE aunit_flowctrl DEFAULT if_aunit_constants=>method.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /usi/cl_bal_aunit_exception IMPLEMENTATION.
  METHOD abort_on_unexpected_exception.
    DATA exception_text TYPE string.

    exception_text = i_exception->get_text( ).
    cl_aunit_assert=>abort( msg    = `Unexpected exception!`
                            detail = exception_text
                            quit   = i_quit ).
  ENDMETHOD.


  METHOD fail_on_unexpected_exception.
    DATA exception_text TYPE string.

    exception_text = i_exception->get_text( ).
    cl_aunit_assert=>fail( msg    = `Unexpected exception!`
                           detail = exception_text
                           quit   = i_quit ).
  ENDMETHOD.
ENDCLASS.
