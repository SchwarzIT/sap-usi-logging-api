CLASS /usi/cl_bal_cd_log_lv_by_user DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_log_lv_by_user.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /USI/CL_BAL_CD_LOG_LV_BY_USER IMPLEMENTATION.
  METHOD /usi/if_bal_cd_log_lv_by_user~get_records.
    SELECT uname
           endda
           log_object
           sub_object
           log_level
           auto_save
      FROM /usi/bal_lv_user
      INTO TABLE r_result
      WHERE uname      EQ i_user_name
        AND endda      IN i_endda_range
        AND log_object IN i_log_object_range
        AND sub_object IN i_sub_object_range.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
