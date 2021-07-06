CLASS /usi/cl_bal_ce_log_lv_by_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /usi/if_bal_ce_log_lv_by_user .

    METHODS constructor
      IMPORTING
        !i_customizing_dao TYPE REF TO /usi/if_bal_cd_log_lv_by_user.
    METHODS get_fallback
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA customizing_dao TYPE REF TO /usi/if_bal_cd_log_lv_by_user .
ENDCLASS.



CLASS /usi/cl_bal_ce_log_lv_by_user IMPLEMENTATION.
  METHOD /usi/if_bal_ce_log_lv_by_user~get_log_level.
    DATA: customizing_entries     TYPE /usi/if_bal_cd_log_lv_by_user=>ty_records,
          log_object_range_helper TYPE REF TO /usi/cl_bal_log_object_range,
          sub_object_range_helper TYPE REF TO /usi/cl_bal_sub_object_range,
          endda_range             TYPE /usi/bal_date_range,
          endda_range_line        TYPE /usi/bal_date_range_line.

    FIELD-SYMBOLS <customizing_entry> TYPE /usi/if_bal_cd_log_lv_by_user=>ty_record.

    CREATE OBJECT log_object_range_helper.
    log_object_range_helper->insert_line( i_log_object ).
    log_object_range_helper->insert_line( space ).

    CREATE OBJECT sub_object_range_helper.
    sub_object_range_helper->insert_line( i_sub_object ).
    sub_object_range_helper->insert_line( space ).

    endda_range_line-sign   = 'I'.
    endda_range_line-option = 'GE'.
    endda_range_line-low    = sy-datum.
    INSERT endda_range_line INTO TABLE endda_range.

    TRY.
        customizing_entries = customizing_dao->get_records(
                                i_user_name         = i_user_name
                                i_endda_range       = endda_range
                                i_log_object_range  = log_object_range_helper->range
                                i_sub_object_range  = sub_object_range_helper->range
                              ).
      CATCH /usi/cx_bal_root.
        CLEAR customizing_entries.
    ENDTRY.

    IF customizing_entries IS NOT INITIAL.
      SORT customizing_entries BY log_object DESCENDING
                                  sub_object DESCENDING
                                  endda      DESCENDING.

      READ TABLE customizing_entries ASSIGNING <customizing_entry> INDEX 1.

      TRY.
          r_result = /usi/cl_bal_enum_log_level=>get_by_value( <customizing_entry>-log_level ).
        CATCH /usi/cx_bal_root.
          r_result = get_fallback( ).
      ENDTRY.
    ELSE.
      r_result = get_fallback( ).
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    customizing_dao = i_customizing_dao.
  ENDMETHOD.

  METHOD get_fallback.
    r_result = /usi/cl_bal_enum_log_level=>nothing.
  ENDMETHOD.
ENDCLASS.
