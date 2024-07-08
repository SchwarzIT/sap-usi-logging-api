CLASS /usi/cl_bal_factory DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_factory.

    "! <h1>Factory Method (Singleton pattern)</h1>
    "!
    "! @parameter r_result | Factory instance
    CLASS-METHODS get_instance
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_factory.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_cust_eval_factory  | Customizing evaluator factory (Access to API-Customizing)
    "! @parameter i_logger_bl_factory  | BL-Factory (Creates objects, that only the API should create)
    "! @parameter i_logger_dao_factory | DAO-Factory (Persistency Layer)
    METHODS constructor
      IMPORTING i_cust_eval_factory  TYPE REF TO /usi/if_bal_cust_eval_factory
                i_logger_bl_factory  TYPE REF TO /usi/if_bal_logger_bl_factory
                i_logger_dao_factory TYPE REF TO /usi/if_bal_logger_dao_factory.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF fallback_log_object,
                 log_object TYPE balobj_d  VALUE '/USI/BAL',
                 sub_object TYPE balsubobj VALUE 'WRONG_API_CALLS',
               END   OF fallback_log_object.

    CLASS-DATA instance TYPE REF TO /usi/if_bal_factory.

    DATA: cust_eval_factory  TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory  TYPE REF TO /usi/if_bal_logger_bl_factory,
          logger_dao_factory TYPE REF TO /usi/if_bal_logger_dao_factory,
          logger             TYPE REF TO /usi/if_bal_logger.

    METHODS get_data_container_classnames
      IMPORTING i_log_object    TYPE balobj_d
                i_sub_object    TYPE balsubobj
                i_log_level     TYPE REF TO /usi/cl_bal_enum_log_level
      RETURNING VALUE(r_result) TYPE /usi/bal_data_cont_classnames.

    METHODS get_retention_parameters
      IMPORTING i_log_object    TYPE balobj_d
                i_sub_object    TYPE balsubobj
                i_log_level     TYPE REF TO /usi/cl_bal_enum_log_level
      RETURNING VALUE(r_result) TYPE /usi/bal_retention_parameters.

    METHODS create_logger_internal
      IMPORTING i_log_object    TYPE balobj_d
                i_sub_object    TYPE balsubobj
                i_external_id   TYPE balnrext
      RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_logger
      RAISING   /usi/cx_bal_root.

    METHODS get_log_level
      IMPORTING i_log_object    TYPE balobj_d
                i_sub_object    TYPE balsubobj OPTIONAL
      RETURNING VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level.

    METHODS get_auto_save_package_size
      IMPORTING i_log_object    TYPE balobj_d
                i_sub_object    TYPE balsubobj OPTIONAL
      RETURNING VALUE(r_result) TYPE /usi/bal_auto_save_pckg_size.

    METHODS on_log_writer_invalidation
      FOR EVENT instance_invalidated OF /usi/if_bal_logger.

ENDCLASS.


CLASS /usi/cl_bal_factory IMPLEMENTATION.
  METHOD /usi/if_bal_factory~create_new_logger.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    IF logger IS NOT BOUND.
      TRY.
          logger = create_logger_internal( i_log_object  = i_log_object
                                           i_sub_object  = i_sub_object
                                           i_external_id = i_external_id ).

        CATCH /usi/cx_bal_root INTO exception.
          " Most likely an invalid combination of log object and subobject => Retry using fallbacks
          exception_text = exception->get_text( ).
          ASSERT ID /usi/bal_log_writer
                 FIELDS exception_text
                 CONDITION exception IS NOT BOUND.

          TRY.
              logger = create_logger_internal( i_log_object  = fallback_log_object-log_object
                                               i_sub_object  = fallback_log_object-sub_object
                                               i_external_id = i_external_id ).

            CATCH /usi/cx_bal_root INTO exception.
              " THIS SHOULD _NEVER_ HAPPEN! (If it still happens, the API is broken)
              exception_text = exception->get_text( ).
              ASSERT FIELDS exception_text
                     CONDITION exception IS NOT BOUND.

          ENDTRY.
      ENDTRY.
    ENDIF.

    r_result = logger.
  ENDMETHOD.

  METHOD /usi/if_bal_factory~get_existing_logger.
    ASSERT ID /usi/bal_log_writer
           CONDITION logger IS BOUND.

    IF logger IS BOUND.
      r_result = logger.
    ELSE.
      r_result = /usi/if_bal_factory~create_new_logger( i_log_object = fallback_log_object-log_object
                                                        i_sub_object = fallback_log_object-sub_object ).
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    cust_eval_factory  = i_cust_eval_factory.
    logger_bl_factory  = i_logger_bl_factory.
    logger_dao_factory = i_logger_dao_factory.
  ENDMETHOD.

  METHOD create_logger_internal.
    DATA: auto_save_package_size    TYPE /usi/bal_auto_save_pckg_size,
          data_container_classnames TYPE /usi/bal_data_cont_classnames,
          data_container_coll_dao   TYPE REF TO /usi/if_bal_data_cont_coll_dao,
          log_dao                   TYPE REF TO /usi/if_bal_log_dao,
          log_level                 TYPE REF TO /usi/cl_bal_enum_log_level,
          retention_parameters      TYPE /usi/bal_retention_parameters.

    log_level = get_log_level( i_log_object = i_log_object
                               i_sub_object = i_sub_object ).

    auto_save_package_size = get_auto_save_package_size( i_log_object = i_log_object
                                                         i_sub_object = i_sub_object ).

    retention_parameters = get_retention_parameters( i_log_object = i_log_object
                                                     i_sub_object = i_sub_object
                                                     i_log_level  = log_level ).

    data_container_classnames = get_data_container_classnames( i_log_object = i_log_object
                                                               i_sub_object = i_sub_object
                                                               i_log_level  = log_level ).

    log_dao = logger_dao_factory->get_log( i_log_object           = i_log_object
                                           i_sub_object           = i_sub_object
                                           i_external_id          = i_external_id
                                           i_retention_parameters = retention_parameters ).

    data_container_coll_dao = logger_dao_factory->get_data_container_collection( ).

    r_result = NEW /usi/cl_bal_logger( i_factory                  = logger_bl_factory
                                       i_relevant_data_containers = data_container_classnames
                                       i_log_level                = log_level
                                       i_auto_save_pckg_size      = auto_save_package_size
                                       i_log_dao                  = log_dao
                                       i_data_cont_coll_dao       = data_container_coll_dao ).

    SET HANDLER on_log_writer_invalidation FOR r_result.
  ENDMETHOD.

  METHOD get_auto_save_package_size.
    CONSTANTS c_auto_save_disabled TYPE /usi/bal_auto_save_pckg_size VALUE 0.

    DATA: cust_evaluator_by_log_object TYPE REF TO /usi/if_bal_ce_log_lv_by_obj,
          cust_evaluator_by_client     TYPE REF TO /usi/if_bal_ce_log_lv_by_clnt,
          cust_evaluator_by_user       TYPE REF TO /usi/if_bal_ce_log_lv_by_user,
          package_size_by_log_object   TYPE /usi/bal_auto_save_pckg_size,
          package_size_by_client       TYPE /usi/bal_auto_save_pckg_size,
          package_size_by_user         TYPE /usi/bal_auto_save_pckg_size,
          smallest_package_size        TYPE /usi/bal_auto_save_pckg_size.

    cust_evaluator_by_log_object = cust_eval_factory->get_log_level_by_log_object( ).
    package_size_by_log_object   = cust_evaluator_by_log_object->get_auto_save_package_size(
                                       i_log_object = i_log_object
                                       i_sub_object = i_sub_object ).

    cust_evaluator_by_client = cust_eval_factory->get_log_level_by_client( ).
    package_size_by_client   = cust_evaluator_by_client->get_auto_save_package_size( i_log_object = i_log_object
                                                                                     i_sub_object = i_sub_object ).

    cust_evaluator_by_user = cust_eval_factory->get_log_level_by_user( ).
    package_size_by_user   = cust_evaluator_by_user->get_auto_save_package_size( i_log_object = i_log_object
                                                                                 i_sub_object = i_sub_object ).

    IF     package_size_by_user <> c_auto_save_disabled
       AND (    package_size_by_client = c_auto_save_disabled
             OR package_size_by_user   < package_size_by_client ).
      smallest_package_size = package_size_by_user.
    ELSE.
      smallest_package_size = package_size_by_client.
    ENDIF.

    IF     package_size_by_log_object <> c_auto_save_disabled
       AND (    smallest_package_size      = c_auto_save_disabled
             OR package_size_by_log_object < smallest_package_size ).
      smallest_package_size = package_size_by_log_object.
    ENDIF.

    r_result = smallest_package_size.
  ENDMETHOD.

  METHOD get_data_container_classnames.
    DATA customizing_evaluator TYPE REF TO /usi/if_bal_ce_data_containers.

    customizing_evaluator = cust_eval_factory->get_data_containers( ).
    r_result              = customizing_evaluator->get_relevant_plugin_classnames( i_log_object = i_log_object
                                                                                   i_sub_object = i_sub_object
                                                                                   i_log_level  = i_log_level ).
  ENDMETHOD.

  METHOD get_instance.
    DATA: cust_eval_factory  TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory  TYPE REF TO /usi/if_bal_logger_bl_factory,
          logger_dao_factory TYPE REF TO /usi/if_bal_logger_dao_factory.

    IF instance IS NOT BOUND.
      cust_eval_factory  = /usi/cl_bal_cust_eval_factory=>get_instance( ).
      logger_bl_factory  = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
      logger_dao_factory = /usi/cl_bal_logger_dao_factory=>get_instance( ).

      instance = NEW /usi/cl_bal_factory( i_cust_eval_factory  = cust_eval_factory
                                          i_logger_bl_factory  = logger_bl_factory
                                          i_logger_dao_factory = logger_dao_factory ).
    ENDIF.

    r_result = instance.
  ENDMETHOD.

  METHOD get_log_level.
    DATA: cust_evaluator_by_log_object TYPE REF TO /usi/if_bal_ce_log_lv_by_obj,
          cust_evaluator_by_client     TYPE REF TO /usi/if_bal_ce_log_lv_by_clnt,
          cust_evaluator_by_user       TYPE REF TO /usi/if_bal_ce_log_lv_by_user,
          log_level_by_log_object      TYPE REF TO /usi/cl_bal_enum_log_level,
          log_level_by_client          TYPE REF TO /usi/cl_bal_enum_log_level,
          log_level_by_user            TYPE REF TO /usi/cl_bal_enum_log_level,
          highest_log_level            TYPE REF TO /usi/cl_bal_enum_log_level.

    cust_evaluator_by_log_object = cust_eval_factory->get_log_level_by_log_object( ).
    log_level_by_log_object      = cust_evaluator_by_log_object->get_log_level( i_log_object = i_log_object
                                                                                i_sub_object = i_sub_object ).

    cust_evaluator_by_client = cust_eval_factory->get_log_level_by_client( ).
    log_level_by_client      = cust_evaluator_by_client->get_log_level( i_log_object = i_log_object
                                                                        i_sub_object = i_sub_object ).

    cust_evaluator_by_user = cust_eval_factory->get_log_level_by_user( ).
    log_level_by_user      = cust_evaluator_by_user->get_log_level( i_log_object = i_log_object
                                                                    i_sub_object = i_sub_object ).

    IF log_level_by_client->is_higher_than( log_level_by_user ) = abap_true.
      highest_log_level = log_level_by_client.
    ELSE.
      highest_log_level = log_level_by_user.
    ENDIF.
    IF log_level_by_log_object->is_higher_than( highest_log_level ) = abap_true.
      highest_log_level = log_level_by_log_object.
    ENDIF.

    r_result = highest_log_level.
  ENDMETHOD.

  METHOD get_retention_parameters.
    DATA customizing_evaluator TYPE REF TO /usi/if_bal_ce_retention.

    customizing_evaluator = cust_eval_factory->get_retention_parameters( ).
    r_result              = customizing_evaluator->get_parameters( i_log_object = i_log_object
                                                                   i_sub_object = i_sub_object
                                                                   i_log_level  = i_log_level ).
  ENDMETHOD.

  METHOD on_log_writer_invalidation.
    CLEAR logger.
  ENDMETHOD.
ENDCLASS.
