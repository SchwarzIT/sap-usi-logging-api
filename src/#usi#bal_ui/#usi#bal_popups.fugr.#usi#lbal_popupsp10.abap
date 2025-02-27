CLASS lcl_data_cont_coll_buffer IMPLEMENTATION.
  METHOD constructor.
    data_container_collections = i_data_container_collections.
  ENDMETHOD.

  METHOD get_data_container_collection.
    TRY.
        r_result = data_container_collections[ message_number = i_message_number ]-data_container_collection.
      CATCH cx_sy_itab_line_not_found INTO DATA(not_found).
        RAISE EXCEPTION TYPE /usi/cx_bal_not_found
          EXPORTING textid   = /usi/cx_bal_not_found=>generic_not_found
                    previous = not_found.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
