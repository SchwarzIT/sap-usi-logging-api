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
*         | NOTE: This report is _NOT_ a replacement for the           *
*         |       enhancements mentioned above!                        *
*         |                                                            *
*         |       It is a rather inperformant emergency solution in    *
*         |       case the implementation of the enhancements was      *
*         |       forgotten.                                           *
*----------------------------------------------------------------------*
REPORT /usi/bal_delete_orphan_log_dat.

CLASS lcl_report DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS run.

  PRIVATE SECTION.
    CONSTANTS c_package_size TYPE int4 VALUE 1000000.

    METHODS delete_orphan_log_data.

    METHODS get_orphan_log_numbers
      RETURNING
        VALUE(r_result) TYPE bal_t_logn.

    METHODS delete_message_details
      IMPORTING
        i_lognumbers TYPE bal_t_logn.

ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD run.
    DATA report_instance TYPE REF TO lcl_report.

    CREATE OBJECT report_instance.
    report_instance->delete_orphan_log_data( ).
  ENDMETHOD.

  METHOD delete_orphan_log_data.
    DATA: orphan_log_numbers    TYPE bal_t_logn,
          total_entries_deleted TYPE int4.

    DO.
      orphan_log_numbers = get_orphan_log_numbers( ).
      delete_message_details( orphan_log_numbers ).

      total_entries_deleted = total_entries_deleted + lines( orphan_log_numbers ).

      IF lines( orphan_log_numbers ) LT c_package_size.
        EXIT.
      ENDIF.
    ENDDO.

    MESSAGE s023(/usi/bal) WITH total_entries_deleted.
  ENDMETHOD.

  METHOD get_orphan_log_numbers.
    SELECT DISTINCT lognumber
      FROM /usi/bal_data
      INTO TABLE r_result
      UP TO c_package_size ROWS
      WHERE NOT EXISTS ( SELECT * FROM balhdr WHERE lognumber EQ /usi/bal_data~lognumber )
      ORDER BY lognumber.
  ENDMETHOD.

  METHOD delete_message_details.
    DATA: factory    TYPE REF TO /usi/if_bal_logger_dao_factory,
          dao_object TYPE REF TO /usi/if_bal_data_cont_coll_dao.

    IF i_lognumbers IS INITIAL.
      RETURN.
    ENDIF.

    factory    = /usi/cl_bal_logger_dao_factory=>get_instance( ).
    dao_object = factory->get_data_container_collection( ).
    dao_object->delete_collections( i_lognumbers ).
    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  /usi/cl_auth=>check_tcode( ).
  lcl_report=>run( ).
