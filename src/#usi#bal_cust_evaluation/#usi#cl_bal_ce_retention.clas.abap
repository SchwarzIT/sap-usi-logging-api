CLASS /usi/cl_bal_ce_retention DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_ce_retention.

    "! Constructor
    "!
    "! @parameter i_customizing_dao | DAO-Object
    METHODS constructor
      IMPORTING
        i_customizing_dao TYPE REF TO /usi/if_bal_cd_retention.

    "! Returns fallback retention parameters, if no customizing was maintained (See documentation)
    "!
    "! @parameter r_result | Fallback retention parameters
    METHODS get_fallback
      RETURNING
        VALUE(r_result) TYPE /usi/bal_retention_parameters.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA customizing_dao TYPE REF TO /usi/if_bal_cd_retention.

ENDCLASS.



CLASS /usi/cl_bal_ce_retention IMPLEMENTATION.
  METHOD /usi/if_bal_ce_retention~get_parameters.
    DATA: customizing_entries     TYPE /usi/if_bal_cd_retention=>ty_records,
          log_object_range_helper TYPE REF TO /usi/cl_bal_log_object_range,
          sub_object_range_helper TYPE REF TO /usi/cl_bal_sub_object_range.

    FIELD-SYMBOLS <customizing_entry> TYPE /usi/if_bal_cd_retention=>ty_record.

    log_object_range_helper = NEW #( ).
    log_object_range_helper->insert_line( i_log_object ).
    log_object_range_helper->insert_line( space ).

    sub_object_range_helper = NEW #( ).
    sub_object_range_helper->insert_line( i_sub_object ).
    sub_object_range_helper->insert_line( space ).

    TRY.
        customizing_entries = customizing_dao->get_records( i_log_level        = i_log_level->value
                                                            i_log_object_range = log_object_range_helper->range
                                                            i_sub_object_range = sub_object_range_helper->range ).
      CATCH /usi/cx_bal_root.
        CLEAR customizing_entries.
    ENDTRY.

    IF customizing_entries IS NOT INITIAL.
      SORT customizing_entries BY log_object DESCENDING
                                  sub_object DESCENDING.

      ASSIGN customizing_entries[ 1 ] TO <customizing_entry>.

      r_result = <customizing_entry>-retention_parameters.
    ELSE.
      r_result = get_fallback( ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    customizing_dao = i_customizing_dao.
  ENDMETHOD.


  METHOD get_fallback.
    CONSTANTS number_of_days TYPE /usi/bal_retention_time VALUE 14.

    r_result-retention_time  = number_of_days.
    r_result-no_early_delete = abap_false.
  ENDMETHOD.
ENDCLASS.
