*----------------------------------------------------------------------*
* Title   | Delete orphan log data                                     *
*----------------------------------------------------------------------*
* Purpose | The logging API enhances SAPs application log by so-called *
*         | data containers, that can store virtually any kind of      *
*         | data.                                                      *
*         |                                                            *
*         | The data will be stored in the database table              *
*         | /USI/BAL_DATA and will no longer be needed, as soon, as    *
*         | the log gets deleted.                                      *
*         |                                                            *
*         | We offer a solution to automatically delete data           *
*         | containers when the log is deleted. This must be           *
*         | integrated into the SAP standard deletion logic via an     *
*         | enhancement. Please refer to the documentation. The        *
*         | necessary steps are described there.                       *
*         |                                                            *
*         | In case you forgot to implement the enhancement and need   *
*         | to get rid of orphan data containers, that refer to        *
*         | already deleted logs, you can use this report to delete    *
*         | the now obsolete data.                                     *
*         |                                                            *
*         | CAUTION: As this API is backwards compatible to            *
*         |          SAP_BASIS 700, new SQL-features, that would have  *
*         |          improved the performance of this report could not *
*         |          be used.                                          *
*         |                                                            *
*         |          This report is _NOT_ a replacement for the        *
*         |          enhancements mentioned above!                     *
*         |                                                            *
*         |          It is an inperformant emergency solution in case  *
*         |          the implementation of the enhancements was        *
*         |          forgotten.                                        *
*----------------------------------------------------------------------*
REPORT /usi/bal_delete_orphan_log_dat.

FORM delete_orphan_log_data.

  CONSTANTS: max_package_size TYPE int4 VALUE 1000000.

  DATA: orphan_log_numbers    TYPE bal_t_logn,
        factory               TYPE REF TO /usi/if_bal_logger_dao_factory,
        dao_object            TYPE REF TO /usi/if_bal_data_cont_coll_dao,
        total_entries_deleted TYPE int4.

  factory    = /usi/cl_bal_logger_dao_factory=>get_instance( ).
  dao_object = factory->get_data_container_collection( ).

  DO.
    PERFORM get_orphan_log_numbers
      USING max_package_size
      CHANGING orphan_log_numbers.

    IF orphan_log_numbers IS NOT INITIAL.
      dao_object->delete_collections( orphan_log_numbers ).
      CALL FUNCTION 'ABAP4_COMMIT_WORK'.

      total_entries_deleted = total_entries_deleted + lines( orphan_log_numbers ).
    ENDIF.

    IF lines( orphan_log_numbers ) LT max_package_size.
      EXIT.
    ENDIF.
  ENDDO.

  MESSAGE s023(/usi/bal) WITH total_entries_deleted.
ENDFORM.

FORM get_orphan_log_numbers
  USING i_max_package_size TYPE int4
  CHANGING c_orphan_log_numbers TYPE bal_t_logn.

  TYPES: ty_log_numbers TYPE SORTED TABLE OF balognr WITH UNIQUE KEY table_line.

  DATA: data_container_log_numbers TYPE ty_log_numbers,
        log_header_log_numbers     TYPE ty_log_numbers.

  FIELD-SYMBOLS <log_number> TYPE balognr.

  SELECT DISTINCT lognumber
    FROM /usi/bal_data
    PACKAGE SIZE i_max_package_size
    INTO TABLE data_container_log_numbers.

    IF data_container_log_numbers IS NOT INITIAL.
      SELECT lognumber
        FROM balhdr
        INTO TABLE log_header_log_numbers
        FOR ALL ENTRIES IN data_container_log_numbers
        WHERE lognumber EQ data_container_log_numbers-table_line.

      LOOP AT data_container_log_numbers ASSIGNING <log_number>.
        READ TABLE log_header_log_numbers
          TRANSPORTING NO FIELDS
          WITH TABLE KEY table_line = <log_number>.
        IF sy-subrc NE 0.
          INSERT <log_number> INTO TABLE c_orphan_log_numbers.
          IF lines( c_orphan_log_numbers ) EQ i_max_package_size.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lines( c_orphan_log_numbers ) EQ i_max_package_size.
      EXIT.
    ENDIF.
  ENDSELECT.
ENDFORM.

START-OF-SELECTION.
  /usi/cl_auth=>check_tcode( ).
  PERFORM delete_orphan_log_data.
