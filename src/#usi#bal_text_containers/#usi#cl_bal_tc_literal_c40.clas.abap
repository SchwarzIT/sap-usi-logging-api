CLASS /usi/cl_bal_tc_literal_c40 DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_text_container_c40.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_text | Text as literal (Not translatable)
    METHODS constructor
      IMPORTING
        i_text TYPE /usi/if_bal_text_container_c40=>ty_text.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA text TYPE /usi/if_bal_text_container_c40=>ty_text.

ENDCLASS.



CLASS /usi/cl_bal_tc_literal_c40 IMPLEMENTATION.
  METHOD /usi/if_bal_text_container_c40~deserialize.
    DATA: text      TYPE /usi/if_bal_text_container_c40=>ty_text,
          exception TYPE REF TO cx_transformation_error.

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML i_serialized_text_container
          RESULT text = text.

        CREATE OBJECT r_result TYPE /usi/cl_bal_tc_literal_c40
          EXPORTING
            i_text = text.
      CATCH cx_transformation_error INTO exception.
        RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
          EXPORTING
            textid   = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch
            previous = exception.
    ENDTRY.
  ENDMETHOD.


  METHOD /usi/if_bal_text_container_c40~get_classname.
    r_result = '/USI/CL_BAL_TC_LITERAL_C40'.
  ENDMETHOD.


  METHOD /usi/if_bal_text_container_c40~get_text.
    r_result = text.
  ENDMETHOD.


  METHOD /usi/if_bal_text_container_c40~serialize.
    CALL TRANSFORMATION id
      SOURCE text = text
      RESULT XML r_result.
  ENDMETHOD.


  METHOD constructor.
    me->text = i_text.
  ENDMETHOD.
ENDCLASS.
