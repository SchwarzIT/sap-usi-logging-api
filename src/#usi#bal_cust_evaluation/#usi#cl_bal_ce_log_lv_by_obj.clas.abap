CLASS /usi/cl_bal_ce_log_lv_by_obj DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_ce_log_lv_by_obj.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_customizing_dao | DAO-Object
    METHODS constructor
      IMPORTING
        i_customizing_dao TYPE REF TO /usi/if_bal_cd_log_lv_by_obj.

    "! <h1>Returns the fallback log-level if no customizing is maintained</h1>
    "!
    "! @parameter r_result | Fallback log-level
    METHODS get_fallback_log_level
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level.

    "! <h1>Returns the fallback auto-save-package-size if no customizing is maintained</h1>
    "!
    "! @parameter r_result | Fallback auto-save-package-size
    METHODS get_fallback_auto_save
      RETURNING
        VALUE(r_result) TYPE /usi/bal_auto_save_pckg_size.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA customizing_dao TYPE REF TO /usi/if_bal_cd_log_lv_by_obj.

    METHODS get_customizing_record
      IMPORTING
        i_log_object   TYPE balobj_d
        i_sub_object   TYPE balsubobj
      RETURNING
        VALUE(r_result) TYPE /usi/if_bal_cd_log_lv_by_obj=>ty_record
      RAISING
        /usi/cx_bal_root.

ENDCLASS.



CLASS /usi/cl_bal_ce_log_lv_by_obj IMPLEMENTATION.
  METHOD /usi/if_bal_ce_log_lv_by_obj~get_auto_save_package_size.
    DATA customizing_record TYPE /usi/if_bal_cd_log_lv_by_obj=>ty_record.

    TRY.
        customizing_record = get_customizing_record( i_log_object = i_log_object
                                                     i_sub_object = i_sub_object ).
        r_result = customizing_record-auto_save_package_size.
      CATCH /usi/cx_bal_root.
        r_result = get_fallback_auto_save( ).
    ENDTRY.
  ENDMETHOD.


  METHOD /usi/if_bal_ce_log_lv_by_obj~get_log_level.
    DATA customizing_record TYPE /usi/if_bal_cd_log_lv_by_obj=>ty_record.

    TRY.
        customizing_record = get_customizing_record( i_log_object = i_log_object
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
    DATA: customizing_entries     TYPE /usi/if_bal_cd_log_lv_by_obj=>ty_records,
          log_object_range_helper TYPE REF TO /usi/cl_bal_log_object_range,
          sub_object_range_helper TYPE REF TO /usi/cl_bal_sub_object_range.

    log_object_range_helper = NEW #( ).
    log_object_range_helper->insert_line( i_log_object ).
    log_object_range_helper->insert_line( space ).

    sub_object_range_helper = NEW #( ).
    sub_object_range_helper->insert_line( i_sub_object ).
    sub_object_range_helper->insert_line( space ).

    customizing_entries = customizing_dao->get_records( i_log_object_range = log_object_range_helper->range
                                                        i_sub_object_range = sub_object_range_helper->range ).

    SORT customizing_entries BY log_object DESCENDING
                                sub_object DESCENDING.

    READ TABLE customizing_entries INTO r_result INDEX 1.
  ENDMETHOD.


  METHOD get_fallback_auto_save.
    r_result = 0.
  ENDMETHOD.


  METHOD get_fallback_log_level.
    r_result = /usi/cl_bal_enum_log_level=>nothing.
  ENDMETHOD.
ENDCLASS.
