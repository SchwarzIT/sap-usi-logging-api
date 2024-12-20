CLASS /usi/cl_bal_cd_log_lv_by_clnt DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_cd_log_lv_by_clnt.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /usi/cl_bal_cd_log_lv_by_clnt IMPLEMENTATION.
  METHOD /usi/if_bal_cd_log_lv_by_clnt~get_records.
    SELECT log_object
           sub_object
           endda
           log_level
           auto_save
      FROM /usi/bal_lv_clnt
      INTO TABLE r_result
      WHERE log_object IN i_log_object_range
        AND sub_object IN i_sub_object_range
        AND endda      IN i_endda_range.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
