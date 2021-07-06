CLASS /usi/cl_bal_cd_cx_mapper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /usi/if_bal_cd_cx_mapper .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS /USI/CL_BAL_CD_CX_MAPPER IMPLEMENTATION.
  METHOD /usi/if_bal_cd_cx_mapper~get_records.
    SELECT *
      FROM /usi/bal_cx_map
      INTO TABLE r_result.

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
