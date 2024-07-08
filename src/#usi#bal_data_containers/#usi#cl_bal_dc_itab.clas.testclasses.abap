" * use this source file for your ABAP unit test classes
" ---------------------------------------------------------------------
"  Unit test: Serialization
" ---------------------------------------------------------------------
CLASS lcl_unit_tests_serialization DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_itab DEFINITION LOCAL FRIENDS lcl_unit_tests_serialization.

CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_input,
             table        TYPE REF TO data,
             fieldcatalog TYPE lvc_t_fcat,
             title        TYPE REF TO /usi/if_bal_text_container_c40,
           END   OF ty_input.

    METHODS test_rejects_invalid_xml       FOR TESTING.

    METHODS test_fieldcat                  FOR TESTING.
    METHODS test_title                     FOR TESTING.
    METHODS test_table_of_ddic_struc       FOR TESTING.
    METHODS test_table_of_non_ddic_struc   FOR TESTING.
    METHODS test_table_of_ddic_elem        FOR TESTING.

    "! For ITABs with non-ddic-line-types the internal fieldcatalog will become part of the serialized data.
    "! During deserialization the internal fieldcatalog will be used to rebuild the line structure.
    "! This will only work if the needed fields of the fieldcatalog had been filled before serializing the data.
    "!
    "! This method tests a non-ddic-line-type containing fields having data types that could become problematic,
    "! if the fieldcatatlog was incomplete.
    METHODS test_rebuild_from_fieldcatalog FOR TESTING.

    METHODS get_deserialized_cut
      IMPORTING i_input_to_serialize TYPE ty_input
      RETURNING VALUE(r_result)      TYPE REF TO /usi/cl_bal_dc_itab.
ENDCLASS.


CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_rejects_invalid_xml.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: cut           TYPE REF TO /usi/cl_bal_dc_itab,
          invalid_input TYPE /usi/bal_xml_string.

    TRY.
        cut ?= /usi/cl_bal_dc_itab=>/usi/if_bal_data_container~deserialize( invalid_input ).
        cl_aunit_assert=>fail( `Input was garbage! Exception expected!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_fieldcat.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE ty_input.

    CREATE DATA input-table TYPE bapirettab.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING  i_structure_name = 'BAPIRET2'
      CHANGING   ct_fieldcat      = input-fieldcatalog
      EXCEPTIONS OTHERS           = 0.

    cut = get_deserialized_cut( input ).

    cl_aunit_assert=>assert_equals( exp = input-fieldcatalog
                                    act = cut->external_fieldcatalog ).
  ENDMETHOD.

  METHOD test_title.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE ty_input.

    DATA: BEGIN OF serialized_title,
            input  TYPE /usi/bal_xml_string,
            output TYPE /usi/bal_xml_string,
          END OF serialized_title.

    CREATE DATA input-table TYPE bapirettab.
    input-title = NEW /usi/cl_bal_tc_literal_c40( i_text = 'Callstack' ).

    cut = get_deserialized_cut( input ).

    serialized_title-input  = input-title->serialize( ).
    serialized_title-output = cut->title->serialize( ).
    cl_aunit_assert=>assert_equals( exp = serialized_title-input
                                    act = serialized_title-output ).
  ENDMETHOD.

  METHOD test_table_of_ddic_struc.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE ty_input.

    FIELD-SYMBOLS: <input_table>  TYPE bapirettab,
                   <input_line>   TYPE bapiret2,
                   <output_table> TYPE STANDARD TABLE.

    CREATE DATA input-table TYPE bapirettab.
    ASSIGN input-table->* TO <input_table>.
    INSERT INITIAL LINE INTO TABLE <input_table> ASSIGNING <input_line>.
    <input_line>-message = `Just a test`.

    cut = get_deserialized_cut( input ).

    ASSIGN cut->internal_table_ref->* TO <output_table>.
    cl_aunit_assert=>assert_equals( exp = <input_table>
                                    act = <output_table> ).
  ENDMETHOD.

  METHOD test_table_of_non_ddic_struc.
    TYPES: BEGIN OF ty_non_ddic_structure,
             field1 TYPE char10,
             field2 TYPE char40,
           END   OF ty_non_ddic_structure,
           ty_non_ddic_itab TYPE HASHED TABLE OF ty_non_ddic_structure WITH UNIQUE KEY field1.

    DATA: cut              TYPE REF TO /usi/cl_bal_dc_itab,
          input            TYPE ty_input,
          input_table_line TYPE ty_non_ddic_structure.

    FIELD-SYMBOLS: <input_table>  TYPE ty_non_ddic_itab,
                   <output_table> TYPE STANDARD TABLE.

    CREATE DATA input-table TYPE ty_non_ddic_itab.
    ASSIGN input-table->* TO <input_table>.
    input_table_line-field1 = 'LINE1'.
    INSERT input_table_line INTO TABLE <input_table>.

    cut = get_deserialized_cut( input ).

    ASSIGN cut->internal_table_ref->* TO <output_table>.
    cl_aunit_assert=>assert_equals( exp = <input_table>
                                    act = <output_table> ).
  ENDMETHOD.

  METHOD test_table_of_ddic_elem.
    DATA: cut          TYPE REF TO /usi/cl_bal_dc_itab,
          input        TYPE ty_input,
          output_table TYPE string_table.

    FIELD-SYMBOLS: <input_table>  TYPE string_table,
                   <output_table> TYPE STANDARD TABLE,
                   <output_line>  TYPE any,
                   <output_field> TYPE string.

    CREATE DATA input-table TYPE string_table.
    ASSIGN input-table->* TO <input_table>.
    INSERT `Just a test` INTO TABLE <input_table>.

    cut = get_deserialized_cut( input ).

    " Revert structure conversion (CUT converts elementary line types to structured line types internally)
    ASSIGN cut->internal_table_ref->* TO <output_table>.
    LOOP AT <output_table> ASSIGNING <output_line>.
      ASSIGN COMPONENT 1 OF STRUCTURE <output_line> TO <output_field>.
      INSERT <output_field> INTO TABLE output_table.
    ENDLOOP.

    cl_aunit_assert=>assert_equals( exp = <input_table>
                                    act = output_table ).
  ENDMETHOD.

  METHOD test_rebuild_from_fieldcatalog.
    TYPES: BEGIN OF ty_line,
             field1 TYPE c LENGTH 40,
             field2 TYPE x LENGTH 8,
             field3 TYPE n LENGTH 10,
             field4 TYPE p LENGTH 5 DECIMALS 2,
           END   OF ty_line,
           ty_table TYPE STANDARD TABLE OF ty_line WITH NON-UNIQUE DEFAULT KEY.

    DATA: cut              TYPE REF TO /usi/cl_bal_dc_itab,
          input            TYPE ty_input,
          input_table_line TYPE ty_line,
          output_table     TYPE ty_table,
          output_line      TYPE ty_line.

    FIELD-SYMBOLS: <input_table>  TYPE ty_table,
                   <output_table> TYPE STANDARD TABLE,
                   <output_line>  TYPE any.

    CREATE DATA input-table TYPE ty_table.
    ASSIGN input-table->* TO <input_table>.
    input_table_line-field1 = 'Just a test'.
    input_table_line-field3 = 42.
    input_table_line-field4 = '123.45'.
    INSERT input_table_line INTO TABLE <input_table>.

    cut = get_deserialized_cut( input ).

    ASSIGN cut->internal_table_ref->* TO <output_table>.
    LOOP AT <output_table> ASSIGNING <output_line>.
      MOVE-CORRESPONDING <output_line> TO output_line.
      INSERT output_line INTO TABLE output_table.
    ENDLOOP.

    cl_aunit_assert=>assert_equals( exp = <input_table>
                                    act = output_table ).
  ENDMETHOD.

  METHOD get_deserialized_cut.
    DATA: cut                       TYPE REF TO /usi/cl_bal_dc_itab,
          serialized_data_container TYPE /usi/bal_xml_string,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    FIELD-SYMBOLS <table> TYPE ANY TABLE.

    ASSIGN i_input_to_serialize-table->* TO <table>.
    cut = NEW #( i_internal_table = <table>
                 i_title          = i_input_to_serialize-title
                 i_fieldcatalog   = i_input_to_serialize-fieldcatalog ).

    TRY.
        serialized_data_container = cut->/usi/if_bal_data_container~serialize( ).
        CLEAR cut.

        r_result ?= /usi/cl_bal_dc_itab=>/usi/if_bal_data_container~deserialize( serialized_data_container ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
CLASS lcl_unit_test_cardinality DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short

  PRIVATE SECTION.
    METHODS assert_is_multi_use FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_cardinality IMPLEMENTATION.
  METHOD assert_is_multi_use.
    DATA actual_result TYPE abap_bool.

    actual_result = /usi/cl_bal_dc_itab=>/usi/if_bal_data_container~is_multiple_use_allowed( ).
    cl_aunit_assert=>assert_equals( exp = abap_true
                                    act = actual_result ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Classname
" ---------------------------------------------------------------------
CLASS lcl_unit_test_classname DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short

  PRIVATE SECTION.
    METHODS assert_returns_right_classname FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_classname IMPLEMENTATION.
  METHOD assert_returns_right_classname.
    DATA: cut_description TYPE REF TO /usi/cl_bal_aunit_cut_descr_cl,
          actual_result   TYPE classname.

    actual_result   = /usi/cl_bal_dc_itab=>/usi/if_bal_data_container~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
CLASS lcl_unit_test_table_line_types DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short

  PRIVATE SECTION.
    METHODS test_accepts_ddic_structure  FOR TESTING.
    METHODS test_accepts_non_ddic_struct FOR TESTING.
    METHODS test_accepts_elementary      FOR TESTING.
    METHODS test_rejects_table_of_tables FOR TESTING.
    METHODS test_rejects_table_of_orefs  FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_table_line_types IMPLEMENTATION.
  METHOD test_accepts_ddic_structure.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA: cut                  TYPE REF TO /usi/cl_bal_dc_itab,
          input                TYPE abap_callstack,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut = NEW #( i_internal_table = input ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_accepts_non_ddic_struct.
    TYPES: BEGIN OF ty_non_ddic_table_line,
             mandt TYPE mandt,
           END   OF ty_non_ddic_table_line,
           ty_non_ddic_table TYPE STANDARD TABLE OF ty_non_ddic_table_line WITH NON-UNIQUE DEFAULT KEY.

    DATA: cut                  TYPE REF TO /usi/cl_bal_dc_itab,
          input                TYPE ty_non_ddic_table,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut = NEW #( i_internal_table = input ).

        cut->/usi/if_bal_data_container~serialize( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_accepts_elementary.
    DATA: cut                  TYPE REF TO /usi/cl_bal_dc_itab,
          input                TYPE string_table,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        cut = NEW #( i_internal_table = input ).

        cut->/usi/if_bal_data_container~serialize( ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_rejects_table_of_tables.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE STANDARD TABLE OF bapirettab WITH NON-UNIQUE DEFAULT KEY.

    TRY.
        cut = NEW #( i_internal_table = input ).

        cut->/usi/if_bal_data_container~serialize( ).

        cl_aunit_assert=>fail( `The class should only accept itabs with structured or elementary line types!` ).
      CATCH /usi/cx_bal_root.
        " We expected that
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_rejects_table_of_orefs.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE STANDARD TABLE OF REF TO cl_gui_alv_grid WITH NON-UNIQUE DEFAULT KEY.

    TRY.
        cut = NEW #( i_internal_table = input ).

        cut->/usi/if_bal_data_container~serialize( ).

        cl_aunit_assert=>fail( `The class should only accept itabs with structured or elementary line types!` ).
      CATCH /usi/cx_bal_root.
        " We expected that
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
