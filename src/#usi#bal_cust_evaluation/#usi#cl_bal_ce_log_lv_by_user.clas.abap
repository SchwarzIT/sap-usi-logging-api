CLASS /usi/cl_bal_ce_log_lv_by_user DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_ce_log_lv_by_user.

    "! Constructor
    "!
    "! @parameter i_customizing_dao | DAO-Object
    METHODS constructor
      IMPORTING
        i_customizing_dao TYPE REF TO /usi/if_bal_cd_log_lv_by_user.

    "! Returns the fallback log level, if no customizing was maintained
    "!
    "! @parameter r_result | Fallback log level
    METHODS get_fallback_log_level
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level.

    "! Returns the fallback autosave package size, if no customizing was maintained
    "!
    "! @parameter r_result | Fallback autosave package size
    METHODS get_fallback_auto_save
      RETURNING
        VALUE(r_result) TYPE /usi/bal_auto_save_pckg_size.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA customizing_dao TYPE REF TO /usi/if_bal_cd_log_lv_by_user.

    METHODS get_customizing_record
      IMPORTING
        i_user_name    TYPE xubname
        i_log_object   TYPE balobj_d
        i_sub_object   TYPE balsubobj
      RETURNING
        VALUE(r_result) TYPE /usi/if_bal_cd_log_lv_by_user=>ty_record
      RAISING
        /usi/cx_bal_root.

ENDCLASS.



CLASS /usi/cl_bal_ce_log_lv_by_user IMPLEMENTATION.
  METHOD /usi/if_bal_ce_log_lv_by_user~get_auto_save_package_size.
    DATA customizing_record TYPE /usi/if_bal_cd_log_lv_by_user=>ty_record.

    TRY.
        customizing_record = get_customizing_record( i_user_name  = i_user_name
                                                     i_log_object = i_log_object
                                                     i_sub_object = i_sub_object ).

        IF customizing_record-auto_save = abap_true.
          r_result = 1.
        ELSE.
          r_result = 0.
        ENDIF.
      CATCH /usi/cx_bal_root.
        r_result = get_fallback_auto_save( ).
    ENDTRY.
  ENDMETHOD.


  METHOD /usi/if_bal_ce_log_lv_by_user~get_log_level.
    DATA customizing_record TYPE /usi/if_bal_cd_log_lv_by_user=>ty_record.

    TRY.
        customizing_record = get_customizing_record( i_user_name  = i_user_name
                                                     i_log_object = i_log_object
                                                     i_sub_object = i_sub_object ).
      CATCH /usi/cx_bal_root.
        r_result = get_fallback_log_level( ).
        RETURN.
    ENDTRY.

    TRY.
        r_result = /usi/cl_bal_enum_log_level=>get_by_value( customizing_record-log_level ).
      CATCH /usi/cx_bal_root.
        r_result = get_fallback_log_level( ).
    ENDTRY.
  ENDMETHOD.


  METHOD constructor.
    customizing_dao = i_customizing_dao.
  ENDMETHOD.

  METHOD get_customizing_record.
    DATA(customizing_entries) = customizing_dao->get_records( i_user_name        = i_user_name
                                                              i_endda_range      = VALUE #( ( sign   = 'I'
                                                                                              option = 'GE'
                                                                                              low    = sy-datum ) )
                                                              i_log_object_range = VALUE #( sign   = 'I'
                                                                                            option = 'EQ'
                                                                                            ( low = i_log_object )
                                                                                            ( low = space ) )
                                                              i_sub_object_range = VALUE #( sign   = 'I'
                                                                                            option = 'EQ'
                                                                                            ( low = i_sub_object )
                                                                                            ( low = space ) ) ).

    SORT customizing_entries BY log_object DESCENDING
                                sub_object DESCENDING
                                endda      DESCENDING.

    READ TABLE customizing_entries INTO r_result INDEX 1.
  ENDMETHOD.


  METHOD get_fallback_auto_save.
    r_result = 0.
  ENDMETHOD.


  METHOD get_fallback_log_level.
    r_result = /usi/cl_bal_enum_log_level=>nothing.
  ENDMETHOD.
ENDCLASS.
