*"* use this source file for your ABAP unit test classes

" ---------------------------------------------------------------------
" Test double: Single use container [0-1]
" ---------------------------------------------------------------------
CLASS lcl_single_use_container DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_container.

    METHODS constructor
      IMPORTING i_serialized_data_container TYPE /usi/bal_xml_string.

  PRIVATE SECTION.
    DATA serialized_data_container TYPE /usi/bal_xml_string.
ENDCLASS.


CLASS lcl_single_use_container IMPLEMENTATION.
  METHOD constructor.
    serialized_data_container = i_serialized_data_container.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_false.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~deserialize.
    r_result = NEW lcl_single_use_container( i_serialized_data_container ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_classname.
    r_result = 'LCL_SINGLE_USE_CONTAINER'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_description.
    r_result = 'Dummy description'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    r_result = serialized_data_container.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Test double: Multi use container [0-n]
" ---------------------------------------------------------------------
CLASS lcl_multi_use_container DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_container.

    METHODS constructor
      IMPORTING i_serialized_data_container TYPE /usi/bal_xml_string.

  PRIVATE SECTION.
    DATA serialized_data_container TYPE /usi/bal_xml_string.
ENDCLASS.


CLASS lcl_multi_use_container IMPLEMENTATION.
  METHOD constructor.
    serialized_data_container = i_serialized_data_container.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_true.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~deserialize.
    r_result = NEW lcl_multi_use_container( i_serialized_data_container ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_classname.
    r_result = 'LCL_MULTI_USE_CONTAINER'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_description.
    r_result = 'Dummy description'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    r_result = serialized_data_container.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Test double: defect data container
" ---------------------------------------------------------------------
CLASS lcl_defect_container DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_container.

    CLASS-METHODS class_constructor.

    CLASS-METHODS set_return_invalid_classname
      IMPORTING i_return_invalid_classname TYPE abap_bool.

    CLASS-METHODS set_throw_on_deserialize
      IMPORTING i_throw_on_deserialize TYPE abap_bool.

    CLASS-METHODS set_throw_on_serialize
      IMPORTING i_throw_on_serialize TYPE abap_bool.

    CLASS-METHODS reset_to_defaults.

    METHODS constructor
      IMPORTING i_serialized_data_container TYPE /usi/bal_xml_string.

  PRIVATE SECTION.
    CLASS-DATA: my_classname         TYPE /usi/bal_data_cont_classname,
                throw_on_deserialize TYPE abap_bool,
                throw_on_serialize   TYPE abap_bool.

    DATA serialized_data_container TYPE /usi/bal_xml_string.
ENDCLASS.


CLASS lcl_defect_container IMPLEMENTATION.
  METHOD class_constructor.
    reset_to_defaults( ).
  ENDMETHOD.

  METHOD reset_to_defaults.
    set_return_invalid_classname( abap_false ).
    set_throw_on_deserialize( abap_false ).
    set_throw_on_serialize( abap_false ).
  ENDMETHOD.

  METHOD set_return_invalid_classname.
    CONSTANTS: BEGIN OF classnames,
                 right TYPE /usi/bal_data_cont_classname VALUE 'LCL_DEFECT_CONTAINER',
                 wrong TYPE /usi/bal_data_cont_classname VALUE 'LCL_UNKNOWN_CLASS',
               END   OF classnames.

    IF i_return_invalid_classname = abap_true.
      my_classname = classnames-wrong.
    ELSE.
      my_classname = classnames-right.
    ENDIF.
  ENDMETHOD.

  METHOD set_throw_on_deserialize.
    throw_on_deserialize = i_throw_on_deserialize.
  ENDMETHOD.

  METHOD set_throw_on_serialize.
    throw_on_serialize = i_throw_on_serialize.
  ENDMETHOD.

  METHOD constructor.
    serialized_data_container = i_serialized_data_container.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_true.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~deserialize.
    IF throw_on_deserialize = abap_false.
      r_result = NEW lcl_defect_container( i_serialized_data_container ).
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
        EXPORTING textid = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch.
    ENDIF.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_classname.
    r_result = my_classname.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_description.
    r_result = 'Dummy description'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    IF throw_on_serialize = abap_false.
      r_result = serialized_data_container.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
CLASS lcl_unit_test_cardinality DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_data_container_col.

    METHODS setup.

    METHODS test_single_use_restriction    FOR TESTING.
    METHODS test_no_duplicates_restriction FOR TESTING.
    METHODS test_multi_use_is_working      FOR TESTING.

    METHODS get_single_use_container
      IMPORTING i_serialized_data_container TYPE /usi/bal_xml_string
      RETURNING VALUE(r_result)             TYPE REF TO /usi/if_bal_data_container.

    METHODS get_multi_use_container
      IMPORTING i_serialized_data_container TYPE /usi/bal_xml_string
      RETURNING VALUE(r_result)             TYPE REF TO /usi/if_bal_data_container.

    METHODS assert_has_container
      IMPORTING i_data_container TYPE REF TO /usi/if_bal_data_container.

    METHODS assert_not_has_container
      IMPORTING i_data_container TYPE REF TO /usi/if_bal_data_container.

    METHODS has_container
      IMPORTING i_data_container TYPE REF TO /usi/if_bal_data_container
      RETURNING VALUE(r_result)  TYPE abap_bool.
ENDCLASS.


CLASS lcl_unit_test_cardinality IMPLEMENTATION.
  METHOD setup.
    cut = NEW /usi/cl_bal_dc_collection( ).
  ENDMETHOD.

  METHOD test_single_use_restriction.
    DATA(data_container_1) = get_single_use_container( 'Data_1' ).
    DATA(data_container_2) = get_single_use_container( 'Data_2' ).

    cut->insert( data_container_1 ).
    cut->insert( data_container_2 ).

    assert_has_container( data_container_1 ).
    assert_not_has_container( data_container_2 ).
  ENDMETHOD.

  METHOD test_no_duplicates_restriction.
    CONSTANTS the_very_same_data TYPE /usi/bal_xml_string VALUE 'The very same data'.

    DATA: data_container TYPE REF TO /usi/if_bal_data_container,
          duplicate      TYPE REF TO /usi/if_bal_data_container.

    data_container = get_multi_use_container( the_very_same_data ).
    duplicate      = get_multi_use_container( the_very_same_data ).

    cut->insert( data_container ).
    cut->insert( duplicate ).

    assert_has_container( data_container ).
    assert_not_has_container( duplicate ).
  ENDMETHOD.

  METHOD test_multi_use_is_working.
    DATA(data_container_1) = get_multi_use_container( 'Data_1' ).
    DATA(data_container_2) = get_multi_use_container( 'Data_2' ).

    cut->insert( data_container_1 ).
    cut->insert( data_container_2 ).

    assert_has_container( data_container_1 ).
    assert_has_container( data_container_2 ).
  ENDMETHOD.

  METHOD get_single_use_container.
    r_result = NEW lcl_single_use_container( i_serialized_data_container ).
  ENDMETHOD.

  METHOD get_multi_use_container.
    r_result = NEW lcl_multi_use_container( i_serialized_data_container ).
  ENDMETHOD.

  METHOD assert_has_container.
    IF has_container( i_data_container ) <> abap_true.
      cl_abap_unit_assert=>fail( msg    = `Expected container is missing!`
                                 detail = i_data_container->get_classname( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_not_has_container.
    IF has_container( i_data_container ) = abap_true.
      cl_abap_unit_assert=>fail( msg    = `Unexpected container found!`
                                 detail = i_data_container->get_classname( ) ).
    ENDIF.
  ENDMETHOD.

  METHOD has_container.
    DATA(data_containers) = cut->get_data_containers( ).
    r_result = boolc( line_exists( data_containers[ table_line = i_data_container ] ) ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test: Cardinality
" ---------------------------------------------------------------------
CLASS lcl_unit_test_serialization DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_flattened_data_container,
             data_container_classname  TYPE /usi/bal_data_cont_classname,
             serialized_data_container TYPE /usi/bal_xml_string,
           END   OF ty_flattened_data_container,
           ty_flattened_data_containers TYPE SORTED TABLE OF ty_flattened_data_container
                                        WITH UNIQUE KEY data_container_classname serialized_data_container.

    METHODS test_serialize_deserialize     FOR TESTING.
    METHODS test_deserialize_total_garbage FOR TESTING.
    METHODS test_deserialize_unknown_class FOR TESTING.
    METHODS test_deserialize_bad_cont_data FOR TESTING.
    METHODS test_empty_collection          FOR TESTING.

    METHODS serialize_and_deserialize_coll
      IMPORTING i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col
      RETURNING VALUE(r_result)             TYPE REF TO /usi/if_bal_data_container_col.

    METHODS flatten_data_container
      IMPORTING i_data_container TYPE REF TO /usi/if_bal_data_container
      RETURNING VALUE(r_result)  TYPE ty_flattened_data_container.

    METHODS flatten_data_cont_coll
      IMPORTING i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col
      RETURNING VALUE(r_result)             TYPE ty_flattened_data_containers.

    METHODS get_bad_data_container
      IMPORTING i_return_invalid_classname TYPE abap_bool DEFAULT abap_false
                i_throw_on_deserialize     TYPE abap_bool DEFAULT abap_false
      RETURNING VALUE(r_result)            TYPE REF TO lcl_defect_container.

    METHODS get_good_data_container
      IMPORTING i_serialized_data_container TYPE /usi/bal_xml_string
      RETURNING VALUE(r_result)             TYPE REF TO /usi/if_bal_data_container.
ENDCLASS.


CLASS lcl_unit_test_serialization IMPLEMENTATION.
  METHOD test_deserialize_bad_cont_data.
    DATA(cut) = CAST /usi/if_bal_data_container_col( NEW /usi/cl_bal_dc_collection( ) ).
    DATA(good_data_container) = get_good_data_container( `data` ).
    cut->insert( good_data_container ).
    DATA(expected_result) = flatten_data_cont_coll( cut ).

    " Deserialize( ) should skip bad_container due to errors
    DATA(bad_data_container) = get_bad_data_container( i_throw_on_deserialize = abap_true ).
    cut->insert( bad_data_container ).
    cut = serialize_and_deserialize_coll( cut ).

    DATA(actual_result) = flatten_data_cont_coll( cut ).
    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = actual_result
                                        msg = `Data altered during serialization/deserialization!` ).
  ENDMETHOD.

  METHOD test_deserialize_unknown_class.
    DATA(cut) = CAST /usi/if_bal_data_container_col( NEW /usi/cl_bal_dc_collection( ) ).
    DATA(good_data_container) = get_good_data_container( `data` ).
    cut->insert( good_data_container ).
    DATA(expected_result) = flatten_data_cont_coll( cut ).

    " Deserialize( ) should skip bad_container due to errors
    DATA(bad_data_container) = get_bad_data_container( i_return_invalid_classname = abap_true ).
    cut->insert( bad_data_container ).
    cut = serialize_and_deserialize_coll( cut ).

    DATA(actual_result) = flatten_data_cont_coll( cut ).
    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = actual_result
                                        msg = `Data altered during serialization/deserialization!` ).
  ENDMETHOD.

  METHOD test_deserialize_total_garbage.
    TRY.
        /usi/cl_bal_dc_collection=>/usi/if_bal_data_container_col~deserialize( `Garbage, that cannot be parsed.` ).
        cl_abap_unit_assert=>fail( `Errors on collection level should raise an exception!` ).
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD test_serialize_deserialize.
    DATA(data_container_1) = get_good_data_container( `First container` ).
    DATA(data_container_2) = get_good_data_container( `Second container` ).
    DATA(data_container_3) = get_good_data_container( `Third container` ).

    DATA(cut) = CAST /usi/if_bal_data_container_col( NEW /usi/cl_bal_dc_collection( ) ).
    cut->insert( data_container_1 ).
    cut->insert( data_container_2 ).
    cut->insert( data_container_3 ).
    DATA(expected_result) = flatten_data_cont_coll( cut ).

    cut = serialize_and_deserialize_coll( cut ).

    DATA(actual_result) = flatten_data_cont_coll( cut ).
    cl_abap_unit_assert=>assert_equals( exp = expected_result
                                        act = actual_result
                                        msg = `Data altered during serialization/deserialization!` ).
  ENDMETHOD.

  METHOD test_empty_collection.
    DATA(cut) = CAST /usi/if_bal_data_container_col( NEW /usi/cl_bal_dc_collection( ) ).
    cut = serialize_and_deserialize_coll( cut ).
    DATA(actual_result) = flatten_data_cont_coll( cut ).

    cl_abap_unit_assert=>assert_initial( act = actual_result
                                         msg = `Empty collection returned containers!` ).
  ENDMETHOD.

  METHOD get_bad_data_container.
    r_result = NEW #( i_serialized_data_container = `Data` ).
    r_result->set_return_invalid_classname( i_return_invalid_classname ).
    r_result->set_throw_on_deserialize( i_throw_on_deserialize ).
  ENDMETHOD.

  METHOD get_good_data_container.
    r_result = NEW lcl_multi_use_container( i_serialized_data_container = i_serialized_data_container ).
  ENDMETHOD.

  METHOD flatten_data_cont_coll.
    LOOP AT i_data_container_collection->get_data_containers( ) ASSIGNING FIELD-SYMBOL(<data_container>).
      INSERT flatten_data_container( <data_container> ) INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD flatten_data_container.
    r_result-data_container_classname = i_data_container->get_classname( ).
    TRY.
        r_result-serialized_data_container = i_data_container->serialize( ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.

  METHOD serialize_and_deserialize_coll.
    DATA(serialized_data_cont_coll) = i_data_container_collection->serialize( ).
    TRY.
        r_result = /usi/cl_bal_dc_collection=>/usi/if_bal_data_container_col~deserialize( serialized_data_cont_coll ).
      CATCH /usi/cx_bal_root INTO DATA(unexpected_exception).
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test
" ---------------------------------------------------------------------
CLASS lcl_unit_test_convenience DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_insert_returns_itself FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_convenience IMPLEMENTATION.
  METHOD test_insert_returns_itself.
    DATA(cut) = CAST /usi/if_bal_data_container_col( NEW /usi/cl_bal_dc_collection( ) ).
    DATA(actual_result) = cut->insert( NEW lcl_single_use_container( 'Data_1' ) ).
    cl_abap_unit_assert=>assert_equals( exp = cut
                                        act = actual_result
                                        msg = 'Insert( ) has to return the collection itself (me)!' ).
  ENDMETHOD.
ENDCLASS.


" ---------------------------------------------------------------------
" Unit test
" ---------------------------------------------------------------------
CLASS lcl_unit_test_bad_data_cont DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    DATA cut TYPE REF TO /usi/if_bal_data_container_col.

    METHODS setup.
    METHODS test_insert_bad_data_container FOR TESTING.
    METHODS test_serialize_bad_data_cont   FOR TESTING.
ENDCLASS.


CLASS lcl_unit_test_bad_data_cont IMPLEMENTATION.
  METHOD setup.
    cut = NEW /usi/cl_bal_dc_collection( ).
    lcl_defect_container=>reset_to_defaults( ).
  ENDMETHOD.

  METHOD test_insert_bad_data_container.
    DATA(bad_data_container) = NEW lcl_defect_container( `data` ).
    bad_data_container->set_throw_on_serialize( abap_true ).
    cut->insert( bad_data_container ).

    cl_abap_unit_assert=>assert_false( act = cut->has_data_containers( )
                                       msg = `Invalid container was added` ).
  ENDMETHOD.

  METHOD test_serialize_bad_data_cont.
    DATA unexpected_exception TYPE REF TO /usi/cx_bal_root.

    DATA(bad_data_container) = NEW lcl_defect_container( `data` ).
    cut->insert( bad_data_container ).
    cl_abap_unit_assert=>assert_true( act = cut->has_data_containers( )
                                      msg = `Container should have been added` ).

    bad_data_container->set_throw_on_serialize( abap_true ).
    TRY.
        cut = cut->deserialize( cut->serialize( ) ).
      CATCH /usi/cx_bal_root INTO unexpected_exception.
        /usi/cl_bal_aunit_exception=>fail_on_unexpected_exception( unexpected_exception ).
    ENDTRY.
    cl_abap_unit_assert=>assert_false( act = cut->has_data_containers( )
                                       msg = `Container should have been dropped due to serialization errors` ).
  ENDMETHOD.
ENDCLASS.
