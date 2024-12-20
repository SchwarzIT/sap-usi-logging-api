CLASS /usi/cl_bal_cd_retention DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_retention.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS /USI/CL_BAL_CD_RETENTION IMPLEMENTATION.
  METHOD /usi/if_bal_cd_retention~get_records.
    SELECT log_level
           log_object
           sub_object
           retention_time
           no_early_delete
      FROM /usi/bal_lv_rtim
      INTO TABLE r_result
      WHERE log_level   = i_log_level
        AND log_object IN i_log_object_range
        AND sub_object IN i_sub_object_range.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
