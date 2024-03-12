*"* use this source file for your ABAP unit test classes

CLASS lcl_private_attribute_reader DEFINITION DEFERRED.
CLASS /usi/cl_bal_dc_structure DEFINITION LOCAL FRIENDS lcl_private_attribute_reader.

CLASS lcl_private_attribute_reader DEFINITION.
  PUBLIC SECTION.
    TYPES ty_alv_output TYPE STANDARD TABLE OF /usi/bal_fieldname_and_value WITH NON-UNIQUE DEFAULT KEY.

    CLASS-METHODS get_alv_output
      IMPORTING i_cut           TYPE REF TO /usi/cl_bal_dc_structure
      RETURNING VALUE(r_result) TYPE ty_alv_output
      RAISING   /usi/cx_bal_root.
ENDCLASS.


CLASS lcl_private_attribute_reader IMPLEMENTATION.
  METHOD get_alv_output.
    i_cut->build_alv_output( ).
    r_result = i_cut->alv_output->*.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Serialization
" ---------------------------------------------------------------------
CLASS lcl_unit_tests_serialization DEFINITION FINAL FOR TESTING.
  "#AU Risk_Level Harmless
  "#AU Duration   Short

  PRIVATE SECTION.
    METHODS test_deserialize_bad_xml       FOR TESTING.

    METHODS test_deserialize_valid_xml     FOR TESTING RAISING /usi/cx_bal_root.

    METHODS test_ignores_object_references FOR TESTING RAISING /usi/cx_bal_root.

    METHODS test_throws_on_empty_result    FOR TESTING RAISING /usi/cx_bal_root.

    METHODS test_resolves_included_struc   FOR TESTING RAISING /usi/cx_bal_root.

    METHODS test_resolves_include_w_suffix FOR TESTING RAISING /usi/cx_bal_root.

    METHODS test_resolves_multiple_suffix  FOR TESTING RAISING /usi/cx_bal_root.

    METHODS test_resolves_structured_field FOR TESTING RAISING /usi/cx_bal_root.

    METHODS test_resolves_recursively      FOR TESTING RAISING /usi/cx_bal_root.

    METHODS test_keeps_deep_struc_values   FOR TESTING RAISING /usi/cx_bal_root.

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

    DATA(valid_xml) = NEW /usi/cl_bal_dc_structure( input )->/usi/if_bal_data_container~serialize( ).

    DATA(cut) = CAST /usi/cl_bal_dc_structure(
                        /usi/cl_bal_dc_structure=>/usi/if_bal_data_container~deserialize( valid_xml ) ).
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

  METHOD test_throws_on_empty_result.
    DATA: BEGIN OF structure,
            to_be_ignored_field TYPE REF TO cl_gui_alv_grid,
          END   OF structure.

    DATA(cut) = NEW /usi/cl_bal_dc_structure( structure ).

    TRY.
        cut->/usi/if_bal_data_container~serialize( ).
        cl_aunit_assert=>fail( 'Input contained no relevant fields! Exception expected!' ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_resolves_included_struc.
    TYPES: BEGIN OF ty_included_structure,
             field_01 TYPE char10,
           END   OF ty_included_structure.

    TYPES BEGIN OF ty_main_structure.
            INCLUDE TYPE ty_included_structure.
    TYPES END   OF ty_main_structure.

    DATA(given) = VALUE ty_main_structure( field_01 = 'GIVEN' ).

    DATA(exp) = VALUE lcl_private_attribute_reader=>ty_alv_output( ( fieldname = 'FIELD_01'
                                                                     value     = given-field_01 ) ).

    DATA(cut) = NEW /usi/cl_bal_dc_structure( given ).
    DATA(act) = lcl_private_attribute_reader=>get_alv_output( cut ).

    cl_aunit_assert=>assert_equals( exp = exp
                                    act = act ).
  ENDMETHOD.

  METHOD test_resolves_include_w_suffix.
    TYPES: BEGIN OF ty_included_structure,
             field_01 TYPE char10,
           END   OF ty_included_structure.

    TYPES BEGIN OF ty_main_structure.
            INCLUDE TYPE ty_included_structure AS included_structure RENAMING WITH SUFFIX _suffix.
    TYPES END   OF ty_main_structure.

    DATA(given) = VALUE ty_main_structure( field_01_suffix = 'GIVEN' ).

    DATA(exp) = VALUE lcl_private_attribute_reader=>ty_alv_output( ( fieldname = 'FIELD_01_SUFFIX'
                                                                     value     = given-field_01_suffix ) ).

    DATA(cut) = NEW /usi/cl_bal_dc_structure( given ).
    DATA(act) = lcl_private_attribute_reader=>get_alv_output( cut ).

    cl_aunit_assert=>assert_equals( exp = exp
                                    act = act ).
  ENDMETHOD.

  METHOD test_resolves_multiple_suffix.
    TYPES: BEGIN OF ty_structure_on_4th_level,
             field_01 TYPE char10,
           END   OF ty_structure_on_4th_level.

    TYPES BEGIN OF ty_structure_on_3rd_level.
            INCLUDE TYPE ty_structure_on_4th_level AS included_structure RENAMING WITH SUFFIX _s4.
    TYPES END   OF ty_structure_on_3rd_level.

    TYPES BEGIN OF ty_structure_on_2nd_level.
            INCLUDE TYPE ty_structure_on_3rd_level AS included_structure RENAMING WITH SUFFIX _s3.
    TYPES END   OF ty_structure_on_2nd_level.

    TYPES BEGIN OF ty_structure_on_1st_level.
            INCLUDE TYPE ty_structure_on_2nd_level AS included_structure RENAMING WITH SUFFIX _s2.
    TYPES END   OF ty_structure_on_1st_level.

    DATA(given) = VALUE ty_structure_on_1st_level( field_01_s4_s3_s2 = 'GIVEN' ).

    DATA(exp) = VALUE lcl_private_attribute_reader=>ty_alv_output( ( fieldname = 'FIELD_01_S4_S3_S2'
                                                                     value     = given-field_01_s4_s3_s2 ) ).

    DATA(cut) = NEW /usi/cl_bal_dc_structure( given ).
    DATA(act) = lcl_private_attribute_reader=>get_alv_output( cut ).

    cl_aunit_assert=>assert_equals( exp = exp
                                    act = act ).
  ENDMETHOD.

  METHOD test_resolves_structured_field.
    TYPES: BEGIN OF ty_used_structure,
             field_01 TYPE char10,
           END   OF ty_used_structure.

    TYPES: BEGIN OF ty_main_structure,
             structured_field TYPE ty_used_structure,
           END   OF ty_main_structure.

    DATA(given) = VALUE ty_main_structure( structured_field = VALUE #( field_01 = 'GIVEN' ) ).

    DATA(exp) = VALUE lcl_private_attribute_reader=>ty_alv_output( ( fieldname = 'FIELD_01'
                                                                     value     = given-structured_field-field_01 ) ).

    DATA(cut) = NEW /usi/cl_bal_dc_structure( given ).
    DATA(act) = lcl_private_attribute_reader=>get_alv_output( cut ).

    cl_aunit_assert=>assert_equals( exp = exp
                                    act = act ).
  ENDMETHOD.

  METHOD test_resolves_recursively.
    TYPES: BEGIN OF ty_used_structure,
             field_01 TYPE char10,
           END   OF ty_used_structure.

    TYPES: BEGIN OF ty_included_structure,
             structured_field TYPE ty_used_structure,
           END   OF ty_included_structure.

    TYPES BEGIN OF ty_main_structure.
            INCLUDE TYPE ty_included_structure.
    TYPES END   OF ty_main_structure.

    DATA(given) = VALUE ty_main_structure( structured_field = VALUE #( field_01 = 'GIVEN' ) ).

    DATA(exp) = VALUE lcl_private_attribute_reader=>ty_alv_output( ( fieldname = 'FIELD_01'
                                                                     value     = given-structured_field-field_01 ) ).

    DATA(cut) = NEW /usi/cl_bal_dc_structure( given ).
    DATA(act) = lcl_private_attribute_reader=>get_alv_output( cut ).

    cl_aunit_assert=>assert_equals( exp = exp
                                    act = act ).
  ENDMETHOD.

  METHOD test_keeps_deep_struc_values.
    TYPES: BEGIN OF ty_deep_structure,
             field_01 TYPE char10,
             field_02 TYPE string,
           END   OF  ty_deep_structure.

    DATA(exp) = VALUE lcl_private_attribute_reader=>ty_alv_output( ( fieldname = 'FIELD_01'
                                                                     value     = 'THIS DATA' )
                                                                   ( fieldname = 'FIELD_02'
                                                                     value     = 'MUST NOT BE LOST!' ) ).

    DATA(cut) = NEW /usi/cl_bal_dc_structure( VALUE ty_deep_structure( field_01 = 'THIS DATA'
                                                                       field_02 = 'MUST NOT BE LOST!' ) ).
    DATA(act) = lcl_private_attribute_reader=>get_alv_output( cut ).

    cl_aunit_assert=>assert_equals( exp = exp
                                    act = act ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
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

    actual_result   = /usi/cl_bal_dc_structure=>/usi/if_bal_data_container~get_classname( ).
    cut_description = /usi/cl_bal_aunit_cut_descr_cl=>get_instance( ).
    cut_description->assert_classname_equals( actual_result ).
  ENDMETHOD.
ENDCLASS.
