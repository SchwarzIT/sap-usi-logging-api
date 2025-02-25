" * use this source file for your ABAP unit test classes

CLASS lcl_unit_test_cut_spy DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_itab DEFINITION LOCAL FRIENDS lcl_unit_test_cut_spy.

" ---------------------------------------------------------------------
"  Unit test: Spy to get access to private attributes of CUT
" ---------------------------------------------------------------------
CLASS lcl_unit_test_cut_spy DEFINITION FINAL CREATE PUBLIC FOR TESTING.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_cut TYPE REF TO /usi/cl_bal_dc_itab.

    METHODS get_internal_table
      RETURNING VALUE(r_result) TYPE REF TO data.

    METHODS get_external_fieldcatalog
      RETURNING VALUE(r_result) TYPE lvc_t_fcat.

    METHODS get_title
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_text_container_c40.

  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/cl_bal_dc_itab.

ENDCLASS.


CLASS lcl_unit_test_cut_spy IMPLEMENTATION.
  METHOD constructor.
    cut = i_cut.
  ENDMETHOD.

  METHOD get_internal_table.
    r_result = cut->internal_table_ref.
  ENDMETHOD.

  METHOD get_external_fieldcatalog.
    r_result = cut->external_fieldcatalog.
  ENDMETHOD.

  METHOD get_title.
    r_result = cut->title.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Serialization
" ---------------------------------------------------------------------
CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_input,
             table        TYPE REF TO data,
             fieldcatalog TYPE lvc_t_fcat,
             title        TYPE REF TO /usi/if_bal_text_container_c40,
           END   OF ty_input.

    METHODS test_rejects_invalid_xml       FOR TESTING.

    METHODS test_external_fieldcat         FOR TESTING.
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

    METHODS test_table_of_ref_to_dtel      FOR TESTING.
    METHODS test_table_of_ref_to_structure FOR TESTING.
    METHODS test_nested_structures         FOR TESTING.
    METHODS test_fatal_fieldname_collision FOR TESTING.
    METHODS test_first_fieldname_collision FOR TESTING.

    "! <h1>Test included structures</h1>
    "!
    "! <p>The ITAB container will normalize the line type of the passed
    "! table and might "flatten" its structure. That means, that structured
    "! or included types will be resolved into their respective fields which
    "! alters the structure of the target line type.</p>
    "!
    "! <p>Assignments like "&lt;TARGET_TABLE&gt; = &lt;SOURCE_TABLE&gt;" might cause
    "! short dumps after that since the target line type might not be compatible
    "! anymore.</p>
    "!
    "! <p>This test uses a problematic input line type to test this special case.</p>
    METHODS test_included_structures       FOR TESTING.

    METHODS get_deserialized_cut
      IMPORTING i_input_to_serialize TYPE ty_input
      RETURNING VALUE(r_result)      TYPE REF TO /usi/cl_bal_dc_itab.
ENDCLASS.


CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_rejects_invalid_xml.
    TRY.
        /usi/cl_bal_dc_itab=>/usi/if_bal_data_container~deserialize( |Invalid input! XML expected!| ).
        cl_aunit_assert=>fail( `Input was garbage! Exception expected!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_external_fieldcat.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE ty_input.

    CREATE DATA input-table TYPE bapirettab.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING  i_structure_name = 'BAPIRET2'
      CHANGING   ct_fieldcat      = input-fieldcatalog
      EXCEPTIONS OTHERS           = 0.

    cut = get_deserialized_cut( input ).

    cl_aunit_assert=>assert_equals( exp = input-fieldcatalog
                                    act = NEW lcl_unit_test_cut_spy( cut )->get_external_fieldcatalog( ) ).
  ENDMETHOD.

  METHOD test_title.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE ty_input.

    DATA: BEGIN OF serialized_title,
            input  TYPE /usi/bal_xml_string,
            output TYPE /usi/bal_xml_string,
          END OF serialized_title.

    input = VALUE #( table = NEW bapirettab( )
                     title = NEW /usi/cl_bal_tc_literal_c40( i_text = 'Callstack' ) ).

    cut = get_deserialized_cut( input ).

    serialized_title-input  = input-title->serialize( ).
    serialized_title-output = NEW lcl_unit_test_cut_spy( cut )->get_title( )->serialize( ).
    cl_aunit_assert=>assert_equals( exp = serialized_title-input
                                    act = serialized_title-output ).
  ENDMETHOD.

  METHOD test_table_of_ddic_struc.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE ty_input.

    input = VALUE #( table = NEW bapirettab( ( message = `Just a test` ) ) ).
    ASSIGN input-table->* TO FIELD-SYMBOL(<expected_result>).

    cut = get_deserialized_cut( input ).
    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO FIELD-SYMBOL(<actual_result>).

    cl_aunit_assert=>assert_equals( exp = <expected_result>
                                    act = <actual_result> ).
  ENDMETHOD.

  METHOD test_table_of_non_ddic_struc.
    TYPES: BEGIN OF ty_non_ddic_structure,
             field1 TYPE char10,
             field2 TYPE char40,
           END   OF ty_non_ddic_structure,
           ty_non_ddic_itab TYPE HASHED TABLE OF ty_non_ddic_structure WITH UNIQUE KEY field1.

    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE ty_input.

    input = VALUE #( table = NEW ty_non_ddic_itab( ( field1 = 'LINE1' ) ) ).
    ASSIGN input-table->* TO FIELD-SYMBOL(<expected_result>).

    cut = get_deserialized_cut( input ).
    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO FIELD-SYMBOL(<actual_result>).

    cl_aunit_assert=>assert_equals( exp = <expected_result>
                                    act = <actual_result> ).
  ENDMETHOD.

  METHOD test_table_of_ddic_elem.
    DATA: cut              TYPE REF TO /usi/cl_bal_dc_itab,
          input            TYPE ty_input,
          converted_result TYPE string_table.

    FIELD-SYMBOLS <actual_result> TYPE STANDARD TABLE.

    input = VALUE #( table = NEW string_table( ( `Just a test` ) ) ).
    ASSIGN input-table->* TO FIELD-SYMBOL(<expected_result>).

    cut = get_deserialized_cut( input ).

    " Revert structure conversion (CUT converts elementary line types to structured line types internally)
    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO <actual_result>.
    LOOP AT <actual_result> ASSIGNING FIELD-SYMBOL(<actual_result_line>).
      ASSIGN COMPONENT 1 OF STRUCTURE <actual_result_line> TO FIELD-SYMBOL(<actual_result_field>).
      INSERT <actual_result_field> INTO TABLE converted_result.
    ENDLOOP.

    cl_aunit_assert=>assert_equals( exp = <expected_result>
                                    act = converted_result ).
  ENDMETHOD.

  METHOD test_rebuild_from_fieldcatalog.
    TYPES: BEGIN OF ty_line,
             field1 TYPE c LENGTH 40,
             field2 TYPE x LENGTH 8,
             field3 TYPE n LENGTH 10,
             field4 TYPE p LENGTH 5 DECIMALS 2,
           END   OF ty_line,
           ty_table TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY.

    DATA: cut                   TYPE REF TO /usi/cl_bal_dc_itab,
          input                 TYPE ty_input,
          converted_result      TYPE ty_table,
          converted_result_line TYPE ty_line.

    FIELD-SYMBOLS: <expected_result>    TYPE ty_table,
                   <actual_result>      TYPE STANDARD TABLE,
                   <actual_result_line> TYPE any.

    input = VALUE #( table = NEW ty_table( ( field1 = 'Just a test'
                                             field3 = 42
                                             field4 = '123.45' ) ) ).
    ASSIGN input-table->* TO <expected_result>.

    cut = get_deserialized_cut( input ).

    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO <actual_result>.
    LOOP AT <actual_result> ASSIGNING <actual_result_line>.
      MOVE-CORRESPONDING <actual_result_line> TO converted_result_line.
      INSERT converted_result_line INTO TABLE converted_result.
    ENDLOOP.

    cl_aunit_assert=>assert_equals( exp = <expected_result>
                                    act = converted_result ).
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

  METHOD test_table_of_ref_to_dtel.
    TYPES ty_table TYPE STANDARD TABLE OF REF TO char10 WITH EMPTY KEY.

    CONSTANTS c_test_value TYPE char10 VALUE 'Test'.

    DATA expected_result TYPE REF TO data.

    FIELD-SYMBOLS <expected_result> TYPE STANDARD TABLE.

    DATA(input) = VALUE ty_input( table = NEW ty_table( ( NEW #( c_test_value ) ) ) ).
    DATA(cut) = get_deserialized_cut( input ).

    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO FIELD-SYMBOL(<actual_result>).

    " Convert input to standard table with elementary line type - just like CUT is supposed to
    CREATE DATA expected_result LIKE <actual_result>.
    ASSIGN expected_result->* TO <expected_result>.
    INSERT INITIAL LINE INTO TABLE <expected_result> ASSIGNING FIELD-SYMBOL(<expected_result_line>).
    ASSIGN COMPONENT 1 OF STRUCTURE <expected_result_line> TO FIELD-SYMBOL(<expected_result_field>).
    <expected_result_field> = c_test_value.

    cl_abap_unit_assert=>assert_equals( exp = <expected_result>
                                        act = <actual_result> ).
  ENDMETHOD.

  METHOD test_table_of_ref_to_structure.
    TYPES: BEGIN OF ty_nested_structure_1,
             field_01 TYPE c LENGTH 10,
           END   OF ty_nested_structure_1.

    TYPES: BEGIN OF ty_nested_structure_2,
             field_01 TYPE c LENGTH 10,
           END   OF ty_nested_structure_2.

    TYPES: BEGIN OF ty_line_type,
             field_01 TYPE c LENGTH 10,
             field_02 TYPE c LENGTH 10,
             struc_01 TYPE ty_nested_structure_1,
             struc_02 TYPE ty_nested_structure_2,
           END   OF ty_line_type,
           ty_table_type TYPE STANDARD TABLE OF REF TO ty_line_type WITH EMPTY KEY.

    DATA expected_result TYPE REF TO data.

    FIELD-SYMBOLS <expected_result> TYPE STANDARD TABLE.

    DATA(itab_line) = VALUE ty_line_type( field_01 = 'VALUE_01'
                                          field_02 = 'VALUE_02'
                                          struc_01 = VALUE #( field_01 = 'VALUE_03' )
                                          struc_02 = VALUE #( field_01 = 'VALUE_04' ) ).

    DATA(input) = VALUE ty_input( table = NEW ty_table_type( ( NEW #( itab_line ) ) ) ).
    DATA(cut) = get_deserialized_cut( input ).

    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO FIELD-SYMBOL(<actual_result>).

    " Convert input to standard table with structured line type - just like CUT is supposed to
    CREATE DATA expected_result LIKE <actual_result>.
    ASSIGN expected_result->* TO <expected_result>.
    APPEND INITIAL LINE TO <expected_result> ASSIGNING FIELD-SYMBOL(<expected_result_line>).
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE itab_line TO FIELD-SYMBOL(<source_field>).
      ASSIGN COMPONENT sy-index OF STRUCTURE <expected_result_line> TO FIELD-SYMBOL(<target_field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      <target_field> = <source_field>.
    ENDDO.

    cl_abap_unit_assert=>assert_equals( exp = <expected_result>
                                        act = <actual_result> ).
  ENDMETHOD.

  METHOD test_nested_structures.
    TYPES: BEGIN OF ty_nested_structure,
             very_long_fieldname_for_test_9 TYPE c LENGTH 10,
           END   OF ty_nested_structure.

    TYPES: BEGIN OF ty_line_type,
             field_01 TYPE c LENGTH 10,
             field_02 TYPE c LENGTH 10,
             struc_01 TYPE ty_nested_structure,
             struc_02 TYPE ty_nested_structure,
             struc_03 TYPE ty_nested_structure,
           END   OF ty_line_type,
           ty_table_type TYPE STANDARD TABLE OF ty_line_type WITH EMPTY KEY.

    DATA expected_result TYPE REF TO data.

    FIELD-SYMBOLS <expected_result> TYPE STANDARD TABLE.

    DATA(itab_line) = VALUE ty_line_type( field_01 = 'VALUE_01'
                                          field_02 = 'VALUE_02'
                                          struc_01 = VALUE #( very_long_fieldname_for_test_9 = 'VALUE_03' )
                                          struc_02 = VALUE #( very_long_fieldname_for_test_9 = 'VALUE_04' )
                                          struc_03 = VALUE #( very_long_fieldname_for_test_9 = 'VALUE_05' ) ).

    DATA(input) = VALUE ty_input( table = NEW ty_table_type( ( itab_line ) ) ).
    DATA(cut) = get_deserialized_cut( input ).

    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO FIELD-SYMBOL(<actual_result>).

    " Convert input to standard table with structured line type - just like CUT is supposed to
    CREATE DATA expected_result LIKE <actual_result>.
    ASSIGN expected_result->* TO <expected_result>.
    APPEND INITIAL LINE TO <expected_result> ASSIGNING FIELD-SYMBOL(<expected_result_line>).
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE itab_line TO FIELD-SYMBOL(<source_field>).
      ASSIGN COMPONENT sy-index OF STRUCTURE <expected_result_line> TO FIELD-SYMBOL(<target_field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      <target_field> = <source_field>.
    ENDDO.

    cl_abap_unit_assert=>assert_equals( exp = <expected_result>
                                        act = <actual_result> ).
  ENDMETHOD.

  METHOD test_fatal_fieldname_collision.
    " Fieldnames _MUST_NOT_ start with a digit
    "   - 1st occurrence of this nested structure will be handled as expected
    "   - Additional occurrences cannot be handled by CUT since the name collision
    "     would be solved by adding 1 to the numeric part of the fieldname which
    "     would result in a forbidden fieldname
    "     => 2nd - nth occurrence are expected to be ignored
    TYPES: BEGIN OF ty_nested_structure,
             _99999999999999999999999999999 TYPE char10,
           END   OF ty_nested_structure.

    TYPES: BEGIN OF ty_line_type,
             good_structure TYPE ty_nested_structure,
             bad_structure  TYPE ty_nested_structure,
           END   OF ty_line_type,
           ty_table_type TYPE STANDARD TABLE OF ty_line_type WITH EMPTY KEY.

    CONSTANTS c_field_value TYPE c LENGTH 10 VALUE 'TEST'.

    DATA expected_result TYPE REF TO data.

    FIELD-SYMBOLS <expected_result> TYPE STANDARD TABLE.

    DATA(itab_line) = VALUE ty_line_type( good_structure = VALUE #( _99999999999999999999999999999 = c_field_value )
                                          bad_structure  = VALUE #( _99999999999999999999999999999 = c_field_value ) ).

    DATA(input) = VALUE ty_input( table = NEW ty_table_type( ( itab_line ) ) ).
    DATA(cut) = get_deserialized_cut( input ).

    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO FIELD-SYMBOL(<actual_result>).

    " Convert input to standard table with structured line type - just like CUT is supposed to
    CREATE DATA expected_result LIKE <actual_result>.
    ASSIGN expected_result->* TO <expected_result>.
    APPEND INITIAL LINE TO <expected_result> ASSIGNING FIELD-SYMBOL(<expected_result_line>).
    ASSIGN COMPONENT 1 OF STRUCTURE <expected_result_line> TO FIELD-SYMBOL(<expected_result_field>).
    <expected_result_field> = c_field_value.

    cl_abap_unit_assert=>assert_equals( exp = <expected_result>
                                        act = <actual_result> ).
  ENDMETHOD.

  METHOD test_first_fieldname_collision.
    TYPES: BEGIN OF ty_nested_structure,
             field TYPE char10,
           END   OF ty_nested_structure.

    TYPES: BEGIN OF ty_line_type,
             struc_01 TYPE ty_nested_structure,
             struc_02 TYPE ty_nested_structure,
           END   OF ty_line_type,
           ty_table_type TYPE STANDARD TABLE OF ty_line_type WITH EMPTY KEY.

    DATA expected_result TYPE REF TO data.

    FIELD-SYMBOLS <expected_result> TYPE STANDARD TABLE.

    DATA(itab_line) = VALUE ty_line_type( struc_01 = VALUE #( field = 'Value_01' )
                                          struc_02 = VALUE #( field = 'Value_02' ) ).

    DATA(input) = VALUE ty_input( table = NEW ty_table_type( ( itab_line ) ) ).
    DATA(cut) = get_deserialized_cut( input ).

    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO FIELD-SYMBOL(<actual_result>).

    " Convert input to standard table with structured line type - just like CUT is supposed to
    CREATE DATA expected_result LIKE <actual_result>.
    ASSIGN expected_result->* TO <expected_result>.
    APPEND INITIAL LINE TO <expected_result> ASSIGNING FIELD-SYMBOL(<expected_result_line>).
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE itab_line TO FIELD-SYMBOL(<source_field>).
      ASSIGN COMPONENT sy-index OF STRUCTURE <expected_result_line> TO FIELD-SYMBOL(<target_field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      <target_field> = <source_field>.
    ENDDO.

    cl_abap_unit_assert=>assert_equals( exp = <expected_result>
                                        act = <actual_result> ).
  ENDMETHOD.

  METHOD test_included_structures.
    TYPES: BEGIN OF ty_included_structure,
             int4_field  TYPE int4,
             binary_data TYPE indx_clust,
           END   OF ty_included_structure.

    TYPES  BEGIN OF ty_line_type.
             INCLUDE TYPE ty_included_structure.
    TYPES:   char_field TYPE c LENGTH 10,
           END OF ty_line_type.

    TYPES ty_table_type TYPE HASHED TABLE OF ty_line_type WITH UNIQUE KEY int4_field.

    DATA: cut                   TYPE REF TO /usi/cl_bal_dc_itab,
          input                 TYPE ty_input,
          converted_result      TYPE ty_table_type,
          converted_result_line TYPE ty_line_type.

    FIELD-SYMBOLS <actual_result> TYPE table.

    input = VALUE #( table = NEW ty_table_type( ( int4_field = 1 ) ) ).
    ASSIGN input-table->* TO FIELD-SYMBOL(<expected_result>).

    cut = get_deserialized_cut( input ).
    DATA(actual_result) = NEW lcl_unit_test_cut_spy( cut )->get_internal_table( ).
    ASSIGN actual_result->* TO <actual_result>.

    LOOP AT <actual_result> ASSIGNING FIELD-SYMBOL(<actual_result_line>).
      MOVE-CORRESPONDING <actual_result_line> TO converted_result_line.
      INSERT converted_result_line INTO TABLE converted_result.
    ENDLOOP.

    cl_aunit_assert=>assert_equals( exp = <expected_result>
                                    act = converted_result ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
CLASS lcl_unit_test_cardinality DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
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
CLASS lcl_unit_test_classname DEFINITION FINAL CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
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
CLASS lcl_unit_test_table_line_types DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_accepts_ddic_structure  FOR TESTING.
    METHODS test_accepts_non_ddic_struct FOR TESTING.
    METHODS test_accepts_elementary      FOR TESTING.
    METHODS test_rejects_table_of_tables FOR TESTING.
    METHODS test_rejects_table_of_orefs  FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_table_line_types IMPLEMENTATION.
  METHOD test_accepts_ddic_structure.
    DATA: input                TYPE abap_callstack,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        NEW /usi/cl_bal_dc_itab( i_internal_table = input ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_accepts_non_ddic_struct.
    TYPES: BEGIN OF ty_non_ddic_table_line,
             mandt TYPE mandt,
           END   OF ty_non_ddic_table_line,
           ty_non_ddic_table TYPE STANDARD TABLE OF ty_non_ddic_table_line WITH EMPTY KEY.

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
          input TYPE STANDARD TABLE OF bapirettab WITH EMPTY KEY.

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
          input TYPE STANDARD TABLE OF REF TO cl_gui_alv_grid WITH EMPTY KEY.

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
