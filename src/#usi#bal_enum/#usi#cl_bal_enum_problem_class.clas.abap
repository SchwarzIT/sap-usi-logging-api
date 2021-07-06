CLASS /usi/cl_bal_enum_problem_class DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    CLASS-DATA additional_information TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY .
    CLASS-DATA important TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY .
    CLASS-DATA medium TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY .
    CLASS-DATA other TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY .
    CLASS-DATA very_important TYPE REF TO /usi/cl_bal_enum_problem_class READ-ONLY .
    DATA value TYPE balprobcl READ-ONLY .

    CLASS-METHODS class_constructor .
    METHODS constructor
      IMPORTING
        !i_problem_class TYPE balprobcl .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /USI/CL_BAL_ENUM_PROBLEM_CLASS IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT very_important
      EXPORTING
        i_problem_class = '1'.

    CREATE OBJECT important
      EXPORTING
        i_problem_class = '2'.

    CREATE OBJECT medium
      EXPORTING
        i_problem_class = '3'.

    CREATE OBJECT additional_information
      EXPORTING
        i_problem_class = '4'.

    CREATE OBJECT other
      EXPORTING
        i_problem_class = ' '.
  ENDMETHOD.

  METHOD constructor.
    value = i_problem_class.
  ENDMETHOD.
ENDCLASS.
