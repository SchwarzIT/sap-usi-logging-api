CLASS /usi/cl_bal_enum_detail_level DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA: detail_level_1 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY,
                detail_level_2 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY,
                detail_level_3 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY,
                detail_level_4 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY,
                detail_level_5 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY,
                detail_level_6 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY,
                detail_level_7 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY,
                detail_level_8 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY,
                detail_level_9 TYPE REF TO /usi/cl_bal_enum_detail_level READ-ONLY.

    DATA value TYPE ballevel READ-ONLY.

    "! <h1>Create static instances</h1>
    CLASS-METHODS class_constructor.

    "! <h1>Create instances</h1>
    "!
    "! @parameter i_detail_level | Detail level
    METHODS constructor
      IMPORTING i_detail_level TYPE ballevel.

ENDCLASS.


CLASS /usi/cl_bal_enum_detail_level IMPLEMENTATION.
  METHOD class_constructor.
    detail_level_1 = NEW #( i_detail_level = '1' ).
    detail_level_2 = NEW #( i_detail_level = '2' ).
    detail_level_3 = NEW #( i_detail_level = '3' ).
    detail_level_4 = NEW #( i_detail_level = '4' ).
    detail_level_5 = NEW #( i_detail_level = '5' ).
    detail_level_6 = NEW #( i_detail_level = '6' ).
    detail_level_7 = NEW #( i_detail_level = '7' ).
    detail_level_8 = NEW #( i_detail_level = '8' ).
    detail_level_9 = NEW #( i_detail_level = '9' ).
  ENDMETHOD.

  METHOD constructor.
    value = i_detail_level.
  ENDMETHOD.
ENDCLASS.
