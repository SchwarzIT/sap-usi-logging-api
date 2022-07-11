CLASS /usi/cl_bal_logger_bl_factory DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_logger_bl_factory.

    "! <h1>Factory Method (Singleton pattern)</h1>
    "!
    "! @parameter i_cust_eval_factory | Customizing evaluator factory (Access to API-Customizing)
    "! @parameter r_result | Factory instance
    CLASS-METHODS get_instance
      IMPORTING
        i_cust_eval_factory TYPE REF TO /usi/if_bal_cust_eval_factory
      RETURNING
        VALUE(r_result)     TYPE REF TO /usi/if_bal_logger_bl_factory.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_cust_eval_factory | Customizing evaluator factory (Access to API-Customizing)
    METHODS constructor
      IMPORTING
        i_cust_eval_factory TYPE REF TO /usi/if_bal_cust_eval_factory.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA: instance TYPE REF TO /usi/if_bal_logger_bl_factory.

    DATA: cust_eval_factory  TYPE REF TO /usi/if_bal_cust_eval_factory.

ENDCLASS.



CLASS /usi/cl_bal_logger_bl_factory IMPLEMENTATION.
  METHOD get_instance.
    IF instance IS NOT BOUND.
      CREATE OBJECT instance TYPE /usi/cl_bal_logger_bl_factory
        EXPORTING
          i_cust_eval_factory = i_cust_eval_factory.
    ENDIF.

    r_result = instance.
  ENDMETHOD.

  METHOD constructor.
    cust_eval_factory = i_cust_eval_factory.
  ENDMETHOD.

  METHOD /usi/if_bal_logger_bl_factory~get_exception_mapper.
    DATA: cust_evaluator TYPE REF TO /usi/if_bal_ce_cx_mapper,
          classname      TYPE seoclsname,
          create_error   TYPE REF TO cx_sy_create_error.

    cust_evaluator = cust_eval_factory->get_exception_mapper( ).
    classname      = cust_evaluator->get_exception_mapper_classname( i_exception ).

    TRY.
        CREATE OBJECT r_result TYPE (classname)
          EXPORTING
            i_exception = i_exception.
      CATCH cx_sy_create_error INTO create_error.
        classname = cust_evaluator->get_fallback_mapper_classname( ).
        CREATE OBJECT r_result TYPE (classname)
          EXPORTING
            i_exception = i_exception.
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_logger_bl_factory~get_token.
    CREATE OBJECT r_result TYPE /usi/cl_bal_token.
  ENDMETHOD.
ENDCLASS.
