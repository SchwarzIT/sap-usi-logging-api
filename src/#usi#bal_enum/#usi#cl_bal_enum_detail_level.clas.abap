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
    METHODS constructor
      IMPORTING
        i_detail_level TYPE ballevel.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /usi/cl_bal_enum_detail_level IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT detail_level_1
      EXPORTING
        i_detail_level = '1'.

    CREATE OBJECT detail_level_2
      EXPORTING
        i_detail_level = '2'.

    CREATE OBJECT detail_level_3
      EXPORTING
        i_detail_level = '3'.

    CREATE OBJECT detail_level_4
      EXPORTING
        i_detail_level = '4'.

    CREATE OBJECT detail_level_5
      EXPORTING
        i_detail_level = '5'.

    CREATE OBJECT detail_level_6
      EXPORTING
        i_detail_level = '6'.

    CREATE OBJECT detail_level_7
      EXPORTING
        i_detail_level = '7'.

    CREATE OBJECT detail_level_8
      EXPORTING
        i_detail_level = '8'.

    CREATE OBJECT detail_level_9
      EXPORTING
        i_detail_level = '9'.
  ENDMETHOD.


  METHOD constructor.
    value = i_detail_level.
  ENDMETHOD.
ENDCLASS.
