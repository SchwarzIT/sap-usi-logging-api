CLASS /usi/cl_bal_data_cont_coll_dao DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_cont_coll_dao.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: ty_db_record  TYPE /usi/bal_data,
           ty_db_records TYPE SORTED TABLE OF ty_db_record WITH UNIQUE KEY lognumber msgnumber record_number.

    CONSTANTS db_connection TYPE dbcon_name VALUE 'R/3*SAP_2TH_CONNECT_APPL_LOG'.

    DATA db_records TYPE ty_db_records.

    METHODS convert_xml_to_db
      IMPORTING
        i_log_number                TYPE balognr
        i_message_number            TYPE /usi/bal_message_number
        i_serialized_data_cont_coll TYPE /usi/bal_xml_string
      RETURNING
        VALUE(r_result)              TYPE ty_db_records.

    METHODS convert_db_to_xml
      IMPORTING
        i_db_records   TYPE ty_db_records
      RETURNING
        VALUE(r_result) TYPE /usi/bal_xml_string.
ENDCLASS.



CLASS /usi/cl_bal_data_cont_coll_dao IMPLEMENTATION.
  METHOD /usi/if_bal_data_cont_coll_dao~delete_collections.
    TYPES: BEGIN OF ty_database_key,
             mandt         TYPE mandt,
             lognumber     TYPE balognr,
             msgnumber     TYPE /usi/bal_message_number,
             record_number TYPE indx_srtf2,
           END   OF ty_database_key,
           ty_database_keys TYPE STANDARD TABLE OF ty_database_key WITH NON-UNIQUE DEFAULT KEY.

    DATA database_keys TYPE ty_database_keys.

    IF i_log_numbers IS NOT INITIAL.
      SELECT mandt lognumber msgnumber record_number
        FROM /usi/bal_data
        INTO TABLE database_keys
        FOR ALL ENTRIES IN i_log_numbers
        WHERE lognumber EQ i_log_numbers-table_line.

      DELETE /usi/bal_data FROM TABLE database_keys.
    ENDIF.
  ENDMETHOD.


  METHOD /usi/if_bal_data_cont_coll_dao~get_collection.
    DATA db_records TYPE ty_db_records.

    SELECT mandt
           lognumber
           msgnumber
           record_number
           used_bytes
           data
      FROM /usi/bal_data
      INTO CORRESPONDING FIELDS OF TABLE db_records
      CONNECTION (db_connection)
      WHERE lognumber EQ i_log_number
        AND msgnumber EQ i_message_number.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING
          textid = /usi/cx_bal_not_found=>no_db_entries_found.
    ENDIF.

    r_result = convert_db_to_xml( db_records ).
  ENDMETHOD.


  METHOD /usi/if_bal_data_cont_coll_dao~insert_collection_into_buffer.
    DATA: new_records         TYPE ty_db_records,
          message_parameter_1 TYPE symsgv,
          message_parameter_2 TYPE symsgv.

    FIELD-SYMBOLS <new_record> TYPE ty_db_record.

    IF i_serialized_data_cont_coll IS INITIAL.
      RETURN.
    ENDIF.

    new_records = convert_xml_to_db( i_log_number                = i_log_number
                                     i_message_number            = i_message_number
                                     i_serialized_data_cont_coll = i_serialized_data_cont_coll ).

    LOOP AT new_records ASSIGNING <new_record>.
      INSERT <new_record> INTO TABLE db_records.
      IF sy-subrc NE 0.
        WRITE i_log_number     TO message_parameter_1 LEFT-JUSTIFIED NO-ZERO.
        WRITE i_message_number TO message_parameter_2 LEFT-JUSTIFIED NO-ZERO.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING
            textid = /usi/cx_bal_invalid_input=>duplicate_buffer_entry
            param1 = message_parameter_1
            param2 = message_parameter_2.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD /usi/if_bal_data_cont_coll_dao~save_buffer_to_db.
    DATA message_parameter_1 TYPE symsgv.

    IF db_records IS INITIAL.
      RETURN.
    ENDIF.

    INSERT /usi/bal_data
      CONNECTION (db_connection)
      FROM TABLE db_records.
    IF sy-subrc EQ 0.
      COMMIT CONNECTION (db_connection).
      CLEAR db_records.
    ELSE.
      WRITE sy-subrc TO message_parameter_1 LEFT-JUSTIFIED NO-ZERO.
      RAISE EXCEPTION TYPE /usi/cx_bal_db_error
        EXPORTING
          textid = /usi/cx_bal_db_error=>/usi/cx_bal_db_error
          param1 = message_parameter_1.
    ENDIF.
  ENDMETHOD.


  METHOD convert_db_to_xml.
    DATA binary_data TYPE xstring.
    FIELD-SYMBOLS <db_record> TYPE ty_db_record.

    LOOP AT i_db_records ASSIGNING <db_record>.
      CONCATENATE binary_data <db_record>-data(<db_record>-used_bytes) INTO binary_data IN BYTE MODE.
    ENDLOOP.

    IMPORT serialized_data_cont_coll TO r_result FROM DATA BUFFER binary_data.
  ENDMETHOD.


  METHOD convert_xml_to_db.
    DATA: db_record        TYPE ty_db_record,
          binary_data      TYPE xstring,
          bytes_per_record TYPE int4.

    bytes_per_record = xstrlen( db_record-data ).

    EXPORT serialized_data_cont_coll FROM i_serialized_data_cont_coll TO DATA BUFFER binary_data COMPRESSION ON.
    WHILE binary_data IS NOT INITIAL.
      db_record-record_number = sy-index.
      db_record-lognumber     = i_log_number.
      db_record-msgnumber     = i_message_number.
      db_record-data          = binary_data.
      IF xstrlen( binary_data ) GE bytes_per_record.
        db_record-used_bytes = bytes_per_record.
      ELSE.
        db_record-used_bytes = xstrlen( binary_data ).
      ENDIF.
      INSERT db_record INTO TABLE r_result.

      SHIFT binary_data LEFT BY bytes_per_record PLACES IN BYTE MODE.
    ENDWHILE.
  ENDMETHOD.
ENDCLASS.
