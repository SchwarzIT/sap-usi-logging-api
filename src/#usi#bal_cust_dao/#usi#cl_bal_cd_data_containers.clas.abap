CLASS /usi/cl_bal_cd_data_containers DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /usi/if_bal_cd_data_containers .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /USI/CL_BAL_CD_DATA_CONTAINERS IMPLEMENTATION.
  METHOD /usi/if_bal_cd_data_containers~get_records.
    SELECT  classname
            log_object
            sub_object
            min_log_level
      FROM  /usi/bal_lv_data
      INTO  TABLE r_result
      WHERE log_object IN i_log_object_range
        AND sub_object IN i_sub_object_range.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
