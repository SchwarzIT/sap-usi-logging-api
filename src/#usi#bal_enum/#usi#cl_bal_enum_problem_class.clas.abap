CLASS /usi/cl_bal_enum_problem_class DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA: additional_information TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY,
                important              TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY,
                medium                 TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY,
                other                  TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY,
                very_important         TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY.

    DATA value TYPE balprobcl READ-ONLY.

    "! <h1>Create static instances</h1>
    CLASS-METHODS class_constructor.

    "! <h1>Create instances</h1>
    "!
    "! @parameter i_problem_class | Problem Class
    METHODS constructor
      IMPORTING i_problem_class TYPE balprobcl.

ENDCLASS.


CLASS /usi/cl_bal_enum_problem_class IMPLEMENTATION.
  METHOD class_constructor.
    very_important         = NEW #( i_problem_class = '1' ).
    important              = NEW #( i_problem_class = '2' ).
    medium                 = NEW #( i_problem_class = '3' ).
    additional_information = NEW #( i_problem_class = '4' ).
    other                  = NEW #( i_problem_class = ' ' ).
  ENDMETHOD.

  METHOD constructor.
    value = i_problem_class.
  ENDMETHOD.
ENDCLASS.
