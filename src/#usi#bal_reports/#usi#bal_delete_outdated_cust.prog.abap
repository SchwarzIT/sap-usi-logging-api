*----------------------------------------------------------------------*
* Title   | Delete outdated customizing                                *
*----------------------------------------------------------------------*
* Purpose | Table /usi/bal_lv_user can be used to increase the         *
*         | log level for a user on demand to get a more verbose log.  *
*         | The table can be maintained directly in the productive     *
*         | system as a current  setting.                              *
*         |                                                            *
*         | Since a higher log level leads to more log data, the log   *
*         | level should only be increased temporarily. In order to    *
*         | enforce that, the field ENDDA has been added to the table  *
*         | and the table maintenance view has been enhanced by a      *
*         | validation, that will refuse enddates, that are more than  *
*         | 14 days in the future.                                     *
*         |                                                            *
*         | This report will delete all outdated entries in table      *
*         | /usi/bal_lv_user.                                          *
*----------------------------------------------------------------------*
REPORT /usi/bal_delete_outdated_cust.

TYPE-POOLS: abap.

CLASS lcl_report DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA singleton TYPE REF TO lcl_report.
    CLASS-METHODS class_constructor.

    METHODS run
      IMPORTING
        i_test_mode TYPE abap_bool.

  PRIVATE SECTION.
    METHODS run_test.
    METHODS run_productive.
ENDCLASS.

CLASS lcl_report IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT singleton.
  ENDMETHOD.

  METHOD run.
    IF i_test_mode EQ abap_true.
      run_test( ).
    ELSE.
      run_productive( ).
    ENDIF.
  ENDMETHOD.

  METHOD run_test.
    DATA: entries_to_delete TYPE int4.

    SELECT  COUNT( * )
      FROM  /usi/bal_lv_user
      INTO  entries_to_delete
      WHERE endda LT sy-datum.

    MESSAGE s025(/usi/bal) WITH entries_to_delete.
  ENDMETHOD.

  METHOD run_productive.
    DATA: deleted_entries TYPE int4.

    DELETE FROM /usi/bal_lv_user WHERE endda LT sy-datum.
    IF sy-subrc EQ 0.
      deleted_entries = sy-dbcnt.
      CALL FUNCTION 'ABAP4_COMMIT_WORK'.
    ENDIF.

    MESSAGE s023(/usi/bal) WITH deleted_entries.
  ENDMETHOD.
ENDCLASS.

PARAMETERS:
  testmode TYPE xfeld.

START-OF-SELECTION.
  /usi/cl_auth=>check_tcode( ).
  lcl_report=>singleton->run( testmode ).
