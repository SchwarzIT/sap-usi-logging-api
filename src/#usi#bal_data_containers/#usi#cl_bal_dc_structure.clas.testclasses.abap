*"* use this source file for your ABAP unit test classes

CLASS lcl_private_attribute_reader DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_structure DEFINITION LOCAL FRIENDS lcl_private_attribute_reader.

CLASS lcl_private_attribute_reader DEFINITION.
  PUBLIC SECTION.
    TYPES ty_alv_output TYPE STANDARD TABLE OF /usi/bal_fieldname_and_value WITH NON-UNIQUE DEFAULT KEY.

    CLASS-METHODS get_alv_output
      IMPORTING
        i_cut           TYPE REF TO /usi/cl_bal_dc_structure
      RETURNING
        VALUE(r_result) TYPE ty_alv_output
      RAISING
        /usi/cx_bal_root.
ENDCLASS.

CLASS lcl_private_attribute_reader IMPLEMENTATION.
  METHOD get_alv_output.
    i_cut->build_alv_output( ).
    r_result = i_cut->alv_output->*.
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Serialization
*--------------------------------------------------------------------*
CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS test_deserialize_bad_xml   FOR TESTING.

    METHODS test_deserialize_valid_xml FOR TESTING
      RAISING
        /usi/cx_bal_root.

    METHODS test_ignores_object_references FOR TESTING
      RAISING
        /usi/cx_bal_root.
ENDCLASS.

CLASS lcl_unit_tests_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_xml.
    TRY.
        /usi/cl_bal_dc_structure=>/usi/if_bal_data_container~deserialize( `Garbage input - should fail.` ).
        cl_aunit_assert=>fail( 'Input was garbage! Exception expected!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_deserialize_valid_xml.
    TYPES: BEGIN OF ty_test_structure,
             char10 TYPE c LENGTH 10,
             numc04 TYPE n LENGTH 4,
             int4   TYPE int4,
             string TYPE string,
           END   OF ty_test_structure.

    DATA(input) = VALUE ty_test_structure( char10 = 'Test'
                                           numc04 = 42
                                           int4   = 7
                                           string = 'abcdef' ).
    DATA(exp) = VALUE lcl_private_attribute_reader=>ty_alv_output( ( fieldname = 'CHAR10'
                                                                     value     = input-char10 )
                                                                   ( fieldname = 'NUMC04'
                                                                     value     = input-numc04 )
                                                                   ( fieldname = 'INT4'
                                                                     value     = input-int4 )
                                                                   ( fieldname = 'STRING'
                                                                     value     = input-string ) ).

    DATA(cut) = NEW /usi/cl_bal_dc_structure( input ).
    DATA(act) = lcl_private_attribute_reader=>get_alv_output( cut ).

    cl_aunit_assert=>assert_equals( exp = exp
                                    act = act ).
  ENDMETHOD.

  METHOD test_ignores_object_references.
    DATA: BEGIN OF structure,
            field_01 TYPE c LENGTH 10 VALUE 'Test',
            field_02 TYPE REF TO cl_gui_alv_grid,
          END   OF structure.

    DATA(exp) = VALUE lcl_private_attribute_reader=>ty_alv_output( ( fieldname = 'FIELD_01'
                                                                     value     = structure-field_01 ) ).

    DATA(cut) = NEW /usi/cl_bal_dc_structure( structure ).
    DATA(act) = lcl_private_attribute_reader=>get_alv_output( cut ).

    cl_aunit_assert=>assert_equals( exp = exp
                                    act = act ).
  ENDMETHOD.
ENDCLASS.

*--------------------------------------------------------------------*
* Unit test: Cardinality
*--------------------------------------------------------------------*
CLASS lcl_unit_test_cardinality DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short
  PRIVATE SECTION.
    METHODS assert_is_single_use FOR TESTING.
ENDCLASS.

CLASS lcl_unit_test_cardinality IMPLEMENTATION.
  METHOD assert_is_single_use.
    DATA actual_result TYPE abap_bool.

    actual_result = /usi/cl_bal_dc_structure=>/usi/if_bal_data_container~is_multiple_use_allowed( ).
    cl_aunit_assert=>assert_equals( exp = abap_true
                                    act = actual_result ).
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

    actual_result   = /usi/cl_bal_dc_structure=>/usi/if_bal_data_container~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
