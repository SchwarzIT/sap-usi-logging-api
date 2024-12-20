*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Non-abstract subclass of Code under Test (CUT)
" ---------------------------------------------------------------------
CLASS lcl_cut_subclass DEFINITION INHERITING FROM /usi/cl_bal_em_base FINAL FOR TESTING.
  PUBLIC SECTION.
    METHODS /usi/if_bal_exception_mapper~get_t100_message REDEFINITION.

ENDCLASS.


CLASS lcl_cut_subclass IMPLEMENTATION.
  METHOD /usi/if_bal_exception_mapper~get_t100_message.
    RETURN.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" The test
" ---------------------------------------------------------------------
CLASS lcl_unit_test DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short

  PRIVATE SECTION.
    METHODS test_refuse_unbound_exception  FOR TESTING.
    METHODS test_data_container            FOR TESTING.
    METHODS test_data_container_collection FOR TESTING.
    METHODS test_does_not_dump_on_previous FOR TESTING.

    METHODS get_data_container
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_data_container.

    METHODS assert_container_in_collection
      IMPORTING i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col
                i_data_container            TYPE REF TO /usi/if_bal_data_container.

    METHODS get_exception
      IMPORTING i_previous      TYPE REF TO cx_root                   OPTIONAL
                i_details       TYPE REF TO /usi/if_exception_details OPTIONAL
      RETURNING VALUE(r_result) TYPE REF TO /usi/cx_bal_root.
ENDCLASS.


CLASS lcl_unit_test IMPLEMENTATION.
  METHOD test_refuse_unbound_exception.
    DATA unbound_reference TYPE REF TO cx_root.

    TRY.
        NEW lcl_cut_subclass( i_exception = unbound_reference ).
        cl_aunit_assert=>fail( 'Initial references must not be accepted!' ).
      CATCH /usi/cx_bal_root.
        " Expected result - this is exactly, what we wanted!
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_data_container.
    DATA: cut                       TYPE REF TO lcl_cut_subclass,
          data_container            TYPE REF TO /usi/if_bal_data_container,
          data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          exception                 TYPE REF TO /usi/cx_bal_root,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    data_container = get_data_container( ).
    exception = get_exception( i_details = data_container ).

    TRY.
        cut = NEW #( i_exception = exception ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
    data_container_collection = NEW /usi/cl_bal_dc_collection( ).
    cut->/usi/if_bal_exception_mapper~get_data_containers( data_container_collection ).

    assert_container_in_collection( i_data_container_collection = data_container_collection
                                    i_data_container            = data_container ).
  ENDMETHOD.

  METHOD test_data_container_collection.
    DATA: cut                       TYPE REF TO lcl_cut_subclass,
          data_container            TYPE REF TO /usi/if_bal_data_container,
          data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          exception                 TYPE REF TO /usi/cx_bal_root,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    data_container_collection = NEW /usi/cl_bal_dc_collection( ).
    data_container = get_data_container( ).
    data_container_collection->insert( data_container ).
    exception = get_exception( i_details = data_container_collection ).

    TRY.
        cut = NEW #( i_exception = exception ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
    CLEAR data_container_collection.
    data_container_collection = NEW /usi/cl_bal_dc_collection( ).
    cut->/usi/if_bal_exception_mapper~get_data_containers( data_container_collection ).

    assert_container_in_collection( i_data_container_collection = data_container_collection
                                    i_data_container            = data_container ).
  ENDMETHOD.

  METHOD test_does_not_dump_on_previous.
    DATA: data_container_collection TYPE REF TO /usi/cl_bal_dc_collection,
          cut                       TYPE REF TO lcl_cut_subclass,
          exception                 TYPE REF TO cx_root,
          previous                  TYPE REF TO cx_root.

    previous  = get_exception( ).
    exception = get_exception( i_previous = previous ).

    TRY.
        cut = NEW #( i_exception = exception ).
      CATCH /usi/cx_bal_root.
        cl_aunit_assert=>fail( 'Unexpected exception!' ).
    ENDTRY.

    data_container_collection = NEW #( ).
    cut->/usi/if_bal_exception_mapper~get_data_containers( data_container_collection ).
  ENDMETHOD.

  METHOD get_data_container.
    DATA source_code_position TYPE /usi/bal_source_code_position.

    source_code_position-program_name = 'FOO'.
    source_code_position-include_name = 'BAR'.
    source_code_position-source_line  = 42.
    r_result = NEW /usi/cl_bal_dc_src_pos_cx( i_source_code_position = source_code_position ).
  ENDMETHOD.

  METHOD get_exception.
    TRY.
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found
          EXPORTING textid   = /usi/cx_bal_not_found=>no_db_entries_found
                    previous = i_previous
                    details  = i_details.
      CATCH /usi/cx_bal_not_found INTO r_result.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD assert_container_in_collection.
    DATA data_containers TYPE /usi/bal_data_containers.

    FIELD-SYMBOLS <data_container> TYPE REF TO /usi/if_bal_data_container.

    data_containers = i_data_container_collection->get_data_containers( ).
    LOOP AT data_containers ASSIGNING <data_container>.
      IF <data_container> = i_data_container.
        RETURN.
      ENDIF.
    ENDLOOP.
    cl_aunit_assert=>fail( 'Expected data container is missing!' ).
  ENDMETHOD.
ENDCLASS.
