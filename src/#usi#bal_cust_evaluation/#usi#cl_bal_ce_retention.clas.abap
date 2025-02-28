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
    TRY.
        DATA(customizing_entries) = customizing_dao->get_records( i_log_level        = i_log_level->value
                                                                  i_log_object_range = VALUE #( sign   = 'I'
                                                                                                option = 'EQ'
                                                                                                ( low = i_log_object )
                                                                                                ( low = space ) )
                                                                  i_sub_object_range = VALUE #( sign   = 'I'
                                                                                                option = 'EQ'
                                                                                                ( low = i_sub_object )
                                                                                                ( low = space ) ) ).
      CATCH /usi/cx_bal_root.
        CLEAR customizing_entries.
    ENDTRY.

    IF customizing_entries IS NOT INITIAL.
      SORT customizing_entries BY log_object DESCENDING
                                  sub_object DESCENDING.

      r_result = customizing_entries[ 1 ]-retention_parameters.
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
