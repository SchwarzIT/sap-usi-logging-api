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
    TYPES: BEGIN OF ty_logger_settings,
             auto_save_pckg_size      TYPE /usi/bal_auto_save_pckg_size,
             log_level                TYPE REF TO /usi/cl_bal_enum_log_level,
             relevant_data_containers TYPE /usi/bal_data_cont_classnames,
             retention_parameters     TYPE /usi/bal_retention_parameters,
           END   OF ty_logger_settings.

    TYPES: BEGIN OF ty_logger,
             logger   TYPE REF TO /usi/cl_bal_logger,
             settings TYPE ty_logger_settings,
           END   OF ty_logger,
           ty_loggers TYPE STANDARD TABLE OF ty_logger WITH EMPTY KEY.

    CONSTANTS: BEGIN OF fallback_log_object,
                 log_object TYPE balobj_d  VALUE '/USI/BAL',
                 sub_object TYPE balsubobj VALUE 'WRONG_API_CALLS',
               END   OF fallback_log_object.

    CLASS-DATA singleton TYPE REF TO /usi/if_bal_factory.

    DATA: cust_eval_factory  TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory  TYPE REF TO /usi/if_bal_logger_bl_factory,
          logger_dao_factory TYPE REF TO /usi/if_bal_logger_dao_factory,
          loggers            TYPE ty_loggers.

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

    METHODS get_logger_settings_by_log_obj
      IMPORTING i_log_object    TYPE balobj_d
                i_sub_object    TYPE balsubobj
      RETURNING VALUE(r_result) TYPE ty_logger_settings.

    METHODS get_effective_logger_settings
      IMPORTING i_parent_settings TYPE ty_logger_settings
                i_child_settings  TYPE ty_logger_settings
      RETURNING VALUE(r_result)   TYPE ty_logger_settings.

    METHODS get_log_level
      IMPORTING i_log_object    TYPE balobj_d
                i_sub_object    TYPE balsubobj OPTIONAL
      RETURNING VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level.

    METHODS get_auto_save_package_size
      IMPORTING i_log_object    TYPE balobj_d
                i_sub_object    TYPE balsubobj OPTIONAL
      RETURNING VALUE(r_result) TYPE /usi/bal_auto_save_pckg_size.

    METHODS on_log_writer_invalidation
      FOR EVENT instance_invalidated OF /usi/if_bal_logger
      IMPORTING sender.

ENDCLASS.


CLASS /usi/cl_bal_factory IMPLEMENTATION.
  METHOD get_instance.
    DATA: cust_eval_factory  TYPE REF TO /usi/if_bal_cust_eval_factory,
          logger_bl_factory  TYPE REF TO /usi/if_bal_logger_bl_factory,
          logger_dao_factory TYPE REF TO /usi/if_bal_logger_dao_factory.

    IF singleton IS NOT BOUND.
      cust_eval_factory  = /usi/cl_bal_cust_eval_factory=>get_instance( ).
      logger_bl_factory  = /usi/cl_bal_logger_bl_factory=>get_instance( cust_eval_factory ).
      logger_dao_factory = /usi/cl_bal_logger_dao_factory=>get_instance( ).

      singleton = NEW /usi/cl_bal_factory( i_cust_eval_factory  = cust_eval_factory
                                           i_logger_bl_factory  = logger_bl_factory
                                           i_logger_dao_factory = logger_dao_factory ).
    ENDIF.

    r_result = singleton.
  ENDMETHOD.

  METHOD constructor.
    cust_eval_factory  = i_cust_eval_factory.
    logger_bl_factory  = i_logger_bl_factory.
    logger_dao_factory = i_logger_dao_factory.
  ENDMETHOD.

  METHOD /usi/if_bal_factory~create_new_logger.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    TRY.
        r_result = create_logger_internal( i_log_object  = i_log_object
                                           i_sub_object  = i_sub_object
                                           i_external_id = i_external_id ).

      CATCH /usi/cx_bal_root INTO exception.
        " Most likely an invalid combination of log object and subobject => Retry using fallbacks
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION exception IS NOT BOUND.

        TRY.
            r_result = create_logger_internal( i_log_object  = fallback_log_object-log_object
                                               i_sub_object  = fallback_log_object-sub_object
                                               i_external_id = i_external_id ).

          CATCH /usi/cx_bal_root INTO exception.
            " THIS SHOULD _NEVER_ HAPPEN! (If it still happens, the fallback customizing is missing)
            exception_text = exception->get_text( ).
            ASSERT FIELDS exception_text
                   CONDITION exception IS NOT BOUND.

        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD /usi/if_bal_factory~get_existing_logger.
    ASSERT ID /usi/bal_log_writer
           CONDITION loggers IS NOT INITIAL.

    IF loggers IS NOT INITIAL.
      r_result = loggers[ lines( loggers ) ]-logger.
    ELSE.
      r_result = /usi/if_bal_factory~create_new_logger( i_log_object = fallback_log_object-log_object
                                                        i_sub_object = fallback_log_object-sub_object ).
    ENDIF.
  ENDMETHOD.

  METHOD create_logger_internal.
    IF loggers IS NOT INITIAL.
      DATA(parent) = VALUE #( loggers[ lines( loggers ) ] ).
    ENDIF.

    DATA(settings) = get_effective_logger_settings(
                         i_parent_settings = parent-settings
                         i_child_settings  = get_logger_settings_by_log_obj( i_log_object = i_log_object
                                                                             i_sub_object = i_sub_object ) ).

    r_result
        = NEW /usi/cl_bal_logger(
                  i_factory                  = logger_bl_factory
                  i_parent_logger            = parent-logger
                  i_relevant_data_containers = settings-relevant_data_containers
                  i_log_level                = settings-log_level
                  i_auto_save_pckg_size      = settings-auto_save_pckg_size
                  i_log_dao                  = logger_dao_factory->get_log(
                                                   i_log_object           = i_log_object
                                                   i_sub_object           = i_sub_object
                                                   i_external_id          = i_external_id
                                                   i_retention_parameters = settings-retention_parameters )
                  i_data_cont_coll_dao       = logger_dao_factory->get_data_container_collection( ) ).

    SET HANDLER on_log_writer_invalidation FOR r_result.

    APPEND VALUE #( logger   = CAST #( r_result )
                    settings = settings ) TO loggers.
  ENDMETHOD.

  METHOD get_logger_settings_by_log_obj.
    DATA(log_level) = get_log_level( i_log_object = i_log_object
                                     i_sub_object = i_sub_object ).

    r_result = VALUE #( auto_save_pckg_size      = get_auto_save_package_size( i_log_object = i_log_object
                                                                               i_sub_object = i_sub_object )
                        log_level                = log_level
                        relevant_data_containers = get_data_container_classnames( i_log_object = i_log_object
                                                                                  i_sub_object = i_sub_object
                                                                                  i_log_level  = log_level )
                        retention_parameters     = get_retention_parameters( i_log_object = i_log_object
                                                                             i_sub_object = i_sub_object
                                                                             i_log_level  = log_level ) ).
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

  METHOD get_retention_parameters.
    DATA customizing_evaluator TYPE REF TO /usi/if_bal_ce_retention.

    customizing_evaluator = cust_eval_factory->get_retention_parameters( ).
    r_result              = customizing_evaluator->get_parameters( i_log_object = i_log_object
                                                                   i_sub_object = i_sub_object
                                                                   i_log_level  = i_log_level ).
  ENDMETHOD.

  METHOD get_effective_logger_settings.
    r_result-auto_save_pckg_size      = COND #( WHEN i_parent_settings-auto_save_pckg_size > 0
                                                 AND i_parent_settings-auto_save_pckg_size
                                                     < i_child_settings-auto_save_pckg_size
                                                THEN i_parent_settings-auto_save_pckg_size
                                                ELSE i_child_settings-auto_save_pckg_size ).

    r_result-log_level                = COND #( WHEN i_parent_settings-log_level IS BOUND
                                                 AND i_parent_settings-log_level->is_higher_than(
                                                        i_child_settings-log_level )
                                                THEN i_parent_settings-log_level
                                                ELSE i_child_settings-log_level ).

    r_result-retention_parameters     = VALUE #(
        retention_time  = COND #( WHEN i_parent_settings-retention_parameters-retention_time
                                       > i_child_settings-retention_parameters-retention_time
                                  THEN i_parent_settings-retention_parameters-retention_time
                                  ELSE i_child_settings-retention_parameters-retention_time )
        no_early_delete = xsdbool(    i_parent_settings-retention_parameters-no_early_delete = abap_true
                                   OR i_child_settings-retention_parameters-no_early_delete  = abap_true ) ).

    r_result-relevant_data_containers = i_child_settings-relevant_data_containers.
    LOOP AT i_parent_settings-relevant_data_containers REFERENCE INTO DATA(data_container_name).
      INSERT data_container_name->* INTO TABLE r_result-relevant_data_containers.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_log_writer_invalidation.
    DELETE loggers WHERE logger = sender.
  ENDMETHOD.
ENDCLASS.
