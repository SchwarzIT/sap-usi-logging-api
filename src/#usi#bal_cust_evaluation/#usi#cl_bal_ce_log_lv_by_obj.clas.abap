CLASS /usi/cl_bal_ce_log_lv_by_obj DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /usi/if_bal_ce_log_lv_by_obj .

    METHODS constructor
      IMPORTING
        !i_customizing_dao TYPE REF TO /usi/if_bal_cd_log_lv_by_obj.
    METHODS get_fallback
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_enum_log_level .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA customizing_dao TYPE REF TO /usi/if_bal_cd_log_lv_by_obj .
ENDCLASS.

CLASS /usi/cl_bal_ce_log_lv_by_obj IMPLEMENTATION.
  METHOD constructor.
    customizing_dao = i_customizing_dao.
  ENDMETHOD.

  METHOD /usi/if_bal_ce_log_lv_by_obj~get_log_level.
    DATA: customizing_entries     TYPE /usi/if_bal_cd_log_lv_by_obj=>ty_records,
          log_object_range_helper TYPE REF TO /usi/cl_bal_log_object_range,
          sub_object_range_helper TYPE REF TO /usi/cl_bal_sub_object_range.

    FIELD-SYMBOLS <customizing_entry> TYPE /usi/if_bal_cd_log_lv_by_obj=>ty_record.

    CREATE OBJECT log_object_range_helper.
    log_object_range_helper->insert_line( i_log_object ).
    log_object_range_helper->insert_line( space ).

    CREATE OBJECT sub_object_range_helper.
    sub_object_range_helper->insert_line( i_sub_object ).
    sub_object_range_helper->insert_line( space ).

    TRY.
        customizing_entries = customizing_dao->get_records(
                                i_log_object_range  = log_object_range_helper->range
                                i_sub_object_range  = sub_object_range_helper->range
                              ).
      CATCH /usi/cx_bal_root.
        CLEAR customizing_entries.
    ENDTRY.

    IF customizing_entries IS NOT INITIAL.
      SORT customizing_entries BY log_object DESCENDING
                                  sub_object DESCENDING.

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

  METHOD get_fallback.
    r_result = /usi/cl_bal_enum_log_level=>nothing.
  ENDMETHOD.
ENDCLASS.
