*----------------------------------------------------------------------*
* Title   | Delete outdated customizing                                *
*----------------------------------------------------------------------*
* Purpose | The tables /usi/bal_lv_clnt and /usi/bal_lv_user can be    *
*         | used to increase the log level for a client or user on     *
*         | demand to get a more verbose log.                          *
*         |                                                            *
*         | The tables can be maintained directly in the productive    *
*         | system as a current  setting.                              *
*         |                                                            *
*         | Since a higher log level leads to more log data, the log   *
*         | level should only be increased temporarily. In order to    *
*         | enforce that, the field ENDDA has been added to the tables *
*         | and the table maintenance view has been enhanced by a      *
*         | validation, that will refuse enddates, that are too far in *
*         | the future.                                                *
*         |                                                            *
*         | This report will delete all outdated entries from the      *
*         | tables.                                                    *
*----------------------------------------------------------------------*
REPORT /usi/bal_delete_outdated_cust.

TYPE-POOLS: abap.

CLASS lcl_report DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA singleton TYPE REF TO lcl_report READ-ONLY.
    CLASS-METHODS class_constructor.

    METHODS run
      IMPORTING
        i_test_mode TYPE abap_bool.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_table_names,
                 client TYPE tabname VALUE '/USI/BAL_LV_CLNT',
                 user   TYPE tabname VALUE '/USI/BAL_LV_USER',
               END   OF c_table_names.

    METHODS run_test.
    METHODS run_productive.

    METHODS write_and_display_message_text
      IMPORTING
        i_message_text TYPE bapi_msg.

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
    DATA: message_text            TYPE bapi_msg,
          outdated_client_entries TYPE int4,
          outdated_user_entries   TYPE int4.

    SELECT COUNT( * )
      FROM /usi/bal_lv_clnt
      INTO outdated_client_entries
      WHERE endda LT sy-datum.
    MESSAGE s025(/usi/bal) WITH outdated_client_entries c_table_names-client INTO message_text.
    write_and_display_message_text( message_text ).

    SELECT COUNT( * )
      FROM /usi/bal_lv_user
      INTO outdated_user_entries
      WHERE endda LT sy-datum.
    MESSAGE s025(/usi/bal) WITH outdated_user_entries c_table_names-user INTO message_text.
    write_and_display_message_text( message_text ).
  ENDMETHOD.

  METHOD run_productive.
    DATA: message_text            TYPE bapi_msg,
          outdated_client_entries TYPE int4,
          outdated_user_entries   TYPE int4.

    DELETE FROM /usi/bal_lv_clnt WHERE endda LT sy-datum.
    IF sy-subrc EQ 0.
      outdated_client_entries = sy-dbcnt.
      CALL FUNCTION 'ABAP4_COMMIT_WORK'.
    ENDIF.
    MESSAGE s026(/usi/bal) WITH outdated_client_entries c_table_names-client INTO message_text.
    write_and_display_message_text( message_text ).

    DELETE FROM /usi/bal_lv_user WHERE endda LT sy-datum.
    IF sy-subrc EQ 0.
      outdated_user_entries = sy-dbcnt.
      CALL FUNCTION 'ABAP4_COMMIT_WORK'.
    ENDIF.
    MESSAGE s026(/usi/bal) WITH outdated_user_entries c_table_names-user INTO message_text.
    write_and_display_message_text( message_text ).
  ENDMETHOD.

  METHOD write_and_display_message_text.
    WRITE AT /1: i_message_text.
    MESSAGE i_message_text TYPE 'S'.
  ENDMETHOD.
ENDCLASS.

PARAMETERS:
  testmode TYPE xfeld.

START-OF-SELECTION.
  /usi/cl_auth=>check_tcode( ).
  lcl_report=>singleton->run( testmode ).
