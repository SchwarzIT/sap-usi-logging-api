*"* use this source file for your ABAP unit test classes
*--------------------------------------------------------------------*
* Unit test: Serialization
*--------------------------------------------------------------------*
CLASS lcl_unit_tests_serialization DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_itab DEFINITION LOCAL FRIENDS lcl_unit_tests_serialization.

CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS test_deserialize_bad_xml   FOR TESTING.
    METHODS test_deserialize_valid_xml FOR TESTING.
ENDCLASS.

CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_xml.
    DATA: cut           TYPE REF TO /usi/cl_bal_dc_callstack,
          invalid_input TYPE /usi/bal_xml_string.

    TRY.
        cut ?= /usi/cl_bal_dc_itab=>/usi/if_bal_data_container~deserialize( invalid_input ).
        cl_aunit_assert=>fail( `Input was garbage! Exception expected!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_deserialize_valid_xml.
    DATA: cut           TYPE REF TO /usi/cl_bal_dc_itab,
          fieldcat_line TYPE lvc_s_fcat,
          BEGIN OF input,
            table    TYPE abap_callstack,
            fieldcat TYPE lvc_t_fcat,
            title    TYPE REF TO /usi/if_bal_text_container_c40,
          END   OF input,
          serialized_data_container TYPE /usi/bal_xml_string,
          serialized_title_in       TYPE /usi/bal_xml_string,
          serialized_title_out      TYPE /usi/bal_xml_string,
          unexpected_exception      TYPE REF TO /usi/cx_bal_root.

    FIELD-SYMBOLS: <output_fieldcat> TYPE /usi/cl_bal_dc_itab=>ty_fieldcatalog,
                   <output_table>    TYPE any.

    " Set test input
    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = input-table.

    fieldcat_line-fieldname = 'TEST'.
    INSERT fieldcat_line INTO TABLE input-fieldcat.

    CREATE OBJECT input-title TYPE /usi/cl_bal_tc_literal_c40
      EXPORTING
        i_text = 'Callstack'.

    " serialize & deserialize
    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_internal_table = input-table
            i_title          = input-title
            i_fieldcatalog   = input-fieldcat.

        serialized_data_container = cut->/usi/if_bal_data_container~serialize( ).
        CLEAR cut.
        cut ?= /usi/cl_bal_dc_itab=>/usi/if_bal_data_container~deserialize( serialized_data_container ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.

    " compare
    ASSIGN cut->internal_table_ref->* TO <output_table>.
    cl_aunit_assert=>assert_equals(
      act = <output_table>
      exp = input-table
    ).

    READ TABLE  cut->fieldcatalog_table
      ASSIGNING <output_fieldcat>
      WITH KEY  name = cut->fieldcatalog_names-external.
    IF sy-subrc EQ 0.
      cl_aunit_assert=>assert_equals(
        act = <output_fieldcat>-fieldcatalog
        exp = input-fieldcat
      ).
    ELSE.
      cl_aunit_assert=>fail( `Field catalog was lost!` ).
    ENDIF.

    serialized_title_in  = input-title->serialize( ).
    serialized_title_out = cut->title->serialize( ).
    cl_aunit_assert=>assert_equals(
      act = serialized_title_out
      exp = serialized_title_in
    ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Cardinality
*--------------------------------------------------------------------*
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
    cl_aunit_assert=>assert_equals(
      exp = abap_true
      act = actual_result
    ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Classname
*--------------------------------------------------------------------*
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

*--------------------------------------------------------------------*
* Unit test: Cardinality
*--------------------------------------------------------------------*
CLASS lcl_unit_test_input_validation DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS test_good_table       FOR TESTING.
    METHODS test_non_ddic_table   FOR TESTING.
    METHODS test_wrong_line_type  FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_input_validation IMPLEMENTATION.
  METHOD test_good_table.
    DATA: cut                  TYPE REF TO /usi/cl_bal_dc_itab,
          input                TYPE abap_callstack,
          unexpected_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_internal_table = input.
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_non_ddic_table.
    TYPES: BEGIN OF ty_non_ddic_table_line,
             mandt TYPE mandt,
           END   OF ty_non_ddic_table_line,
           ty_non_ddic_table TYPE STANDARD TABLE OF ty_non_ddic_table_line WITH NON-UNIQUE DEFAULT KEY.

    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE ty_non_ddic_table.

    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_internal_table = input.

        cut->/usi/if_bal_data_container~serialize( ).

        cl_aunit_assert=>fail( `The class is not supposed to support non-ddic tables!` ).
      CATCH /usi/cx_bal_root.
        " We expected that
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_wrong_line_type.
    DATA: cut   TYPE REF TO /usi/cl_bal_dc_itab,
          input TYPE string_table.

    TRY.
        CREATE OBJECT cut
          EXPORTING
            i_internal_table = input.

        cut->/usi/if_bal_data_container~serialize( ).

        cl_aunit_assert=>fail( `The class is not supposed to support tables with non-structured line types!` ).
      CATCH /usi/cx_bal_root.
        " We expected that
        RETURN.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
