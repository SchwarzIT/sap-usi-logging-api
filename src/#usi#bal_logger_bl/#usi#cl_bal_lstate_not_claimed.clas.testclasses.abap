*"* use this source file for your ABAP unit test classes
CLASS lcl_unit_test DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_logger_state.

    METHODS setup.

    METHODS test_returns_token           FOR TESTING.

    METHODS test_throws_on_add_exception FOR TESTING.
    METHODS test_throws_on_add_free_text FOR TESTING.
    METHODS test_throws_on_add_message   FOR TESTING.
    METHODS test_throws_on_display       FOR TESTING.
    METHODS test_throws_on_free          FOR TESTING.
    METHODS test_throws_on_save          FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test IMPLEMENTATION.
  METHOD setup.
    DATA(bl_factory) = /usi/cl_bal_logger_bl_factory=>get_instance( /usi/cl_bal_cust_eval_factory=>get_instance( ) ).
    cut = NEW /usi/cl_bal_lstate_not_claimed( bl_factory ).
  ENDMETHOD.

  METHOD test_throws_on_add_exception.
    DATA input TYPE REF TO /usi/cx_bal_invalid_input.

    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input.
      CATCH /usi/cx_bal_invalid_input INTO input.
        TRY.
            cut->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                i_detail_level  = /usi/cl_bal_enum_detail_level=>detail_level_1
                                i_message_type  = /usi/cl_bal_enum_message_type=>error
                                i_exception     = input
                                i_log_previous  = abap_false ).
            cl_abap_unit_assert=>fail( `Unclaimed logger accepts call to ADD_EXCEPTION( )!` ).
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
        cl_abap_unit_assert=>fail( `Unclaimed logger accepts call to ADD_FREE_TEXT( )!` ).
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
        cl_abap_unit_assert=>fail( `Unclaimed logger accepts call to ADD_MESSAGE( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_throws_on_display.
    TRY.
        cut->display( ).
        cl_abap_unit_assert=>fail( `Unclaimed logger accepts call to DISPLAY( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_throws_on_free.
    TRY.
        cut->free( VALUE #( ) ).
        cl_abap_unit_assert=>fail( `Unclaimed logger accepts call to FREE( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_returns_token.
    TRY.
        cl_abap_unit_assert=>assert_bound( act = cut->claim_ownership( )
                                           msg = `GET_TOKEN( ) returns null!` ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_throws_on_save.
    TRY.
        cut->save( VALUE #( ) ).
        cl_abap_unit_assert=>fail( `Unclaimed logger accepts call to SAVE( )!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
