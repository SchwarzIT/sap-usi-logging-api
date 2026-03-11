*"* use this source file for your ABAP unit test classes
CLASS lcl_cut_creator DEFINITION DEFERRED.
CLASS /usi/cl_bal_factory DEFINITION LOCAL FRIENDS lcl_cut_creator.

"! <h1>Test Double: Customizing DAO for Sub Log Behavior</h1>
CLASS lcl_cust_dao_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_sub_log_behav.

    METHODS set_mock_data
      IMPORTING i_mock_data TYPE /usi/if_bal_cd_sub_log_behav=>ty_records.

  PRIVATE SECTION.
    DATA mock_data TYPE /usi/if_bal_cd_sub_log_behav=>ty_records.

ENDCLASS.


CLASS lcl_cust_dao_double IMPLEMENTATION.
  METHOD set_mock_data.
    mock_data = i_mock_data.
  ENDMETHOD.

  METHOD /usi/if_bal_cd_sub_log_behav~get_records.
    LOOP AT mock_data REFERENCE INTO DATA(result_line) WHERE     log_object IN i_log_object_range
                                                             AND sub_object IN i_sub_object_range.
      INSERT result_line->* INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


"! <h1>Test Double: Customizing DAO Factory</h1>
"!
"! <p>Returns test double for sub log behavior &
"! real DAO-objects for anything else.</p>
CLASS lcl_cust_dao_factory_double DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cust_dao_factory.

    METHODS constructor
      IMPORTING i_sub_log_behavior_double TYPE REF TO /usi/if_bal_cd_sub_log_behav.

  PRIVATE SECTION.
    DATA: real_factory            TYPE REF TO /usi/if_bal_cust_dao_factory,
          sub_log_behavior_double TYPE REF TO /usi/if_bal_cd_sub_log_behav.

ENDCLASS.


CLASS lcl_cust_dao_factory_double IMPLEMENTATION.
  METHOD constructor.
    real_factory            = NEW /usi/cl_bal_cust_dao_factory( ).
    sub_log_behavior_double = i_sub_log_behavior_double.
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_data_containers.
    r_result = real_factory->get_data_containers( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_exception_mapper.
    r_result = real_factory->get_exception_mapper( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_log_level_by_client.
    r_result = real_factory->get_log_level_by_client( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_log_level_by_log_object.
    r_result = real_factory->get_log_level_by_log_object( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_log_level_by_user.
    r_result = real_factory->get_log_level_by_user( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_retention_parameters.
    r_result = real_factory->get_retention_parameters( ).
  ENDMETHOD.

  METHOD /usi/if_bal_cust_dao_factory~get_sub_log_behavior.
    r_result = sub_log_behavior_double.
  ENDMETHOD.
ENDCLASS.


"! <h1>CUT-Creator</h1>
"!
"! <p>Creates code under test & injects test double</p>
CLASS lcl_cut_creator DEFINITION FINAL FOR TESTING.
  PUBLIC SECTION.
    METHODS create_cut
      EXPORTING e_cut        TYPE REF TO /usi/if_bal_factory
                e_dao_double TYPE REF TO lcl_cust_dao_double.
ENDCLASS.


CLASS lcl_cut_creator IMPLEMENTATION.
  METHOD create_cut.
    e_dao_double             = NEW lcl_cust_dao_double( ).
    DATA(cust_dao_factory)   = NEW lcl_cust_dao_factory_double( e_dao_double ).
    DATA(cust_eval_factory)  = NEW /usi/cl_bal_cust_eval_factory( cust_dao_factory ).

    DATA(logger_bl_factory)  = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
    DATA(logger_dao_factory) = /usi/cl_bal_logger_dao_factory=>get_instance( ).

    e_cut = NEW /usi/cl_bal_factory( i_cust_eval_factory  = cust_eval_factory
                                     i_logger_bl_factory  = logger_bl_factory
                                     i_logger_dao_factory = logger_dao_factory ).
  ENDMETHOD.
ENDCLASS.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Test class: Sub Log Behavior
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_test_sub_log_behavior DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_log_object_and_sub_object,
             log_object TYPE balobj_d,
             sub_object TYPE balsubobj,
           END   OF ty_log_object_and_sub_object.

    TYPES: BEGIN OF ty_sub_object,
             sub_object TYPE balsubobj,
           END   OF ty_sub_object,
           ty_sub_objects TYPE SORTED TABLE OF ty_sub_object WITH UNIQUE KEY sub_object,

           BEGIN OF ty_log_object,
             log_object  TYPE balobj_d,
             sub_objects TYPE ty_sub_objects,
           END   OF ty_log_object,
           ty_log_objects TYPE SORTED TABLE OF ty_log_object WITH UNIQUE KEY log_object.

    CLASS-DATA log_objects TYPE ty_log_objects.

    DATA: cut        TYPE REF TO /usi/if_bal_factory,
          dao_double TYPE REF TO lcl_cust_dao_double.

    CLASS-METHODS class_setup.

    METHODS setup.

    " Value 0: Create new logger in any case
    METHODS test_reuse_none_for_full_match FOR TESTING.
    METHODS test_reuse_none_for_obj_match  FOR TESTING.
    METHODS test_reuse_none_for_no_match   FOR TESTING.

    " Value 3: Reuse existing logger, if Log Object & Subobject match
    METHODS test_reuse_full_for_full_match FOR TESTING.
    METHODS test_reuse_full_for_obj_match  FOR TESTING.
    METHODS test_reuse_full_for_no_match   FOR TESTING.

    " Value 6: Reuse existing logger, if Log Object matches
    METHODS test_reuse_obj_for_full_match  FOR TESTING.
    METHODS test_reuse_obj_for_obj_match   FOR TESTING.
    METHODS test_reuse_obj_for_no_match    FOR TESTING.

    " Value 9: Reuse existing logger in any case
    METHODS test_reuse_any_for_full_match  FOR TESTING.
    METHODS test_reuse_any_for_obj_match   FOR TESTING.
    METHODS test_reuse_any_for_no_match    FOR TESTING.

    METHODS get_log_objects_for_full_match
      EXPORTING e_log_object_1 TYPE ty_log_object_and_sub_object
                e_log_object_2 TYPE ty_log_object_and_sub_object.

    METHODS get_log_objects_for_obj_match
      EXPORTING e_log_object_1 TYPE ty_log_object_and_sub_object
                e_log_object_2 TYPE ty_log_object_and_sub_object.

    METHODS get_log_objects_for_no_match
      EXPORTING e_log_object_1 TYPE ty_log_object_and_sub_object
                e_log_object_2 TYPE ty_log_object_and_sub_object.

    METHODS assert_instance_is_reused
      IMPORTING i_log_object_1 TYPE ty_log_object_and_sub_object
                i_log_object_2 TYPE ty_log_object_and_sub_object.

    METHODS assert_instance_is_not_reused
      IMPORTING i_log_object_1 TYPE ty_log_object_and_sub_object
                i_log_object_2 TYPE ty_log_object_and_sub_object.

ENDCLASS.


CLASS lcl_test_sub_log_behavior IMPLEMENTATION.
  METHOD class_setup.
    DATA objects TYPE STANDARD TABLE OF balobj WITH EMPTY KEY.

    SELECT object FROM balsub
      INTO TABLE objects
      UP TO 2 ROWS
      GROUP BY object
      HAVING COUNT(*) >= 2
      ORDER BY object.

    LOOP AT objects REFERENCE INTO DATA(object).
      INSERT VALUE #( log_object = object->object )
             INTO TABLE log_objects
             REFERENCE INTO DATA(result_line).

      SELECT subobject FROM balsub
        UP TO 2 ROWS
        INTO TABLE result_line->sub_objects
        WHERE object = result_line->log_object
        ORDER BY subobject.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_log_objects_for_full_match.
    e_log_object_1 = VALUE #( log_object = log_objects[ 1 ]-log_object
                              sub_object = log_objects[ 1 ]-sub_objects[ 1 ]-sub_object ).

    e_log_object_2 = e_log_object_1.
  ENDMETHOD.

  METHOD get_log_objects_for_obj_match.
    e_log_object_1 = VALUE #( log_object = log_objects[ 1 ]-log_object
                              sub_object = log_objects[ 1 ]-sub_objects[ 1 ]-sub_object ).

    e_log_object_2 = VALUE #( log_object = log_objects[ 1 ]-log_object
                              sub_object = log_objects[ 1 ]-sub_objects[ 2 ]-sub_object ).
  ENDMETHOD.

  METHOD get_log_objects_for_no_match.
    e_log_object_1 = VALUE #( log_object = log_objects[ 1 ]-log_object
                              sub_object = log_objects[ 1 ]-sub_objects[ 1 ]-sub_object ).

    e_log_object_2 = VALUE #( log_object = log_objects[ 2 ]-log_object
                              sub_object = log_objects[ 2 ]-sub_objects[ 1 ]-sub_object ).
  ENDMETHOD.

  METHOD setup.
    NEW lcl_cut_creator( )->create_cut( IMPORTING e_cut        = cut
                                                  e_dao_double = dao_double ).
  ENDMETHOD.

  METHOD test_reuse_none_for_full_match.
    get_log_objects_for_full_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                              e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data( VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>create_new_logger->value ) ) ).

    assert_instance_is_not_reused( i_log_object_1 = log_object_1
                                   i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_none_for_obj_match.
    get_log_objects_for_obj_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                             e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data( VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>create_new_logger->value ) ) ).

    assert_instance_is_not_reused( i_log_object_1 = log_object_1
                                   i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_none_for_no_match.
    get_log_objects_for_no_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                            e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data( VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>create_new_logger->value ) ) ).

    assert_instance_is_not_reused( i_log_object_1 = log_object_1
                                   i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_full_for_full_match.
    get_log_objects_for_full_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                              e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data(
        VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_if_sub_object_matches->value ) ) ).

    assert_instance_is_reused( i_log_object_1 = log_object_1
                               i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_full_for_obj_match.
    get_log_objects_for_obj_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                             e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data(
        VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_if_sub_object_matches->value ) ) ).

    assert_instance_is_not_reused( i_log_object_1 = log_object_1
                                   i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_full_for_no_match.
    get_log_objects_for_no_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                            e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data(
        VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_if_sub_object_matches->value ) ) ).

    assert_instance_is_not_reused( i_log_object_1 = log_object_1
                                   i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_obj_for_full_match.
    get_log_objects_for_full_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                              e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data(
        VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_if_log_object_matches->value ) ) ).

    assert_instance_is_reused( i_log_object_1 = log_object_1
                               i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_obj_for_obj_match.
    get_log_objects_for_obj_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                             e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data(
        VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_if_log_object_matches->value ) ) ).

    assert_instance_is_reused( i_log_object_1 = log_object_1
                               i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_obj_for_no_match.
    get_log_objects_for_no_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                            e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data(
        VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_if_log_object_matches->value ) ) ).

    assert_instance_is_not_reused( i_log_object_1 = log_object_1
                                   i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_any_for_full_match.
    get_log_objects_for_full_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                              e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data( VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_existing_logger->value ) ) ).

    assert_instance_is_reused( i_log_object_1 = log_object_1
                               i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_any_for_obj_match.
    get_log_objects_for_obj_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                             e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data( VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_existing_logger->value ) ) ).

    assert_instance_is_reused( i_log_object_1 = log_object_1
                               i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD test_reuse_any_for_no_match.
    get_log_objects_for_no_match( IMPORTING e_log_object_1 = DATA(log_object_1)
                                            e_log_object_2 = DATA(log_object_2) ).

    dao_double->set_mock_data( VALUE #( ( behavior = /usi/cl_bal_enum_sub_log_behav=>reuse_existing_logger->value ) ) ).

    assert_instance_is_reused( i_log_object_1 = log_object_1
                               i_log_object_2 = log_object_2 ).
  ENDMETHOD.

  METHOD assert_instance_is_reused.
    DATA(first_logger)  = cut->create_new_logger( i_log_object = i_log_object_1-log_object
                                                  i_sub_object = i_log_object_1-sub_object ).
    DATA(second_logger) = cut->create_new_logger( i_log_object = i_log_object_2-log_object
                                                  i_sub_object = i_log_object_2-sub_object ).
    IF first_logger <> second_logger.
      cl_abap_unit_assert=>fail( msg = 'The existing logger should have been reused' ).
    ENDIF.
  ENDMETHOD.

  METHOD assert_instance_is_not_reused.
    DATA(first_logger)  = cut->create_new_logger( i_log_object = i_log_object_1-log_object
                                                  i_sub_object = i_log_object_1-sub_object ).
    DATA(second_logger) = cut->create_new_logger( i_log_object = i_log_object_2-log_object
                                                  i_sub_object = i_log_object_2-sub_object ).
    IF first_logger = second_logger.
      cl_abap_unit_assert=>fail( msg = 'A new logger should have been created' ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
