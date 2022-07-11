*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_test DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    DATA: cut   TYPE REF TO /usi/if_bal_logger_state,
          token TYPE REF TO /usi/if_bal_token.

    METHODS setup.

    METHODS test_throws_on_add_exception FOR TESTING.
    METHODS test_throws_on_add_free_text FOR TESTING.
    METHODS test_throws_on_add_message   FOR TESTING.
    METHODS test_throws_on_free          FOR TESTING.
    METHODS test_throws_on_get_token     FOR TESTING.
    METHODS test_throws_on_save          FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test IMPLEMENTATION.
  METHOD setup.
    DATA: cust_eval_factory TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory TYPE REF TO /usi/if_bal_logger_bl_factory.

    cust_eval_factory = /usi/cl_bal_cust_eval_factory=>get_instance( ).
    logger_bl_factory = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    token             = logger_bl_factory->get_token( ).
    CREATE OBJECT cut TYPE /usi/cl_bal_lstate_invalidated.
  ENDMETHOD.

  METHOD test_throws_on_add_exception.
    DATA: input TYPE REF TO cx_sy_conversion_no_number.
    TRY.
        sy-tabix = 1 + 'A'.
      CATCH cx_sy_conversion_no_number INTO input.
        TRY.
            cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                i_message_type  = /usi/cl_bal_enum_message_type=>error
                                i_exception     = input
                                i_log_previous  = abap_false ).
            cl_aunit_assert=>fail( `Dead logger accepts call to ADD_EXCEPTION( )!` ).
          CATCH /usi/cx_bal_root.
            RETURN.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD test_throws_on_add_free_text.
    TRY.
        cut->add_free_text( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                            i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                            i_message_type  = /usi/cl_bal_enum_message_type=>error
                            i_free_text     = 'Should not work' ).
        cl_aunit_assert=>fail( `Dead logger accepts call to ADD_FREE_TEXT( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_throws_on_add_message.
    TRY.
        cut->add_message( i_problem_class  = /usi/cl_bal_enum_problem_class=>very_important
                          i_detail_level   = /usi/cl_bal_enum_detail_level=>detail_level_1
                          i_message_type   = /usi/cl_bal_enum_message_type=>error
                          i_message_class  = '38'
                          i_message_number = '000' ).
        cl_aunit_assert=>fail( `Dead logger accepts call to ADD_MESSAGE( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_throws_on_free.
    TRY.
        cut->free( token ).
        cl_aunit_assert=>fail( `Dead logger accepts call to FREE( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_throws_on_get_token.
    TRY.
        cut->claim_ownership( ).
        cl_aunit_assert=>fail( `Dead logger accepts call to GET_TOKEN( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_throws_on_save.
    TRY.
        cut->save( token ).
        cl_aunit_assert=>fail( `Dead logger accepts call to SAVE( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
