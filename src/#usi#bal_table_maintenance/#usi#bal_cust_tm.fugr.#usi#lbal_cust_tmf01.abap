*----------------------------------------------------------------------*
***INCLUDE /USI/LBAL_CUST_TMF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form CHECK_END_DATE
*&---------------------------------------------------------------------*
*& Check end date of table
*&---------------------------------------------------------------------*
FORM check_end_date
  USING    i_maximum_number_of_days TYPE /usi/if_bal_cust_validity=>ty_maximum_validity_in_days
  CHANGING c_end_date               TYPE endda.

  DATA maximum_allowed_date TYPE endda.

  IF c_end_date < sy-datum.
    c_end_date = sy-datum.
    MESSAGE w024(/usi/bal).

  ELSE.
    maximum_allowed_date = sy-datum + i_maximum_number_of_days.
    IF c_end_date > maximum_allowed_date.
      c_end_date = maximum_allowed_date.
      MESSAGE w022(/usi/bal) WITH i_maximum_number_of_days.
    ENDIF.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_END_DATE_CLNT
*&---------------------------------------------------------------------*
*& Check end date of /usi/bal_lv_clnt-entry
*&---------------------------------------------------------------------*
FORM check_end_date_clnt.
  DATA: badi                   TYPE REF TO /usi/bal_cust_validity_clnt,
        maximum_number_of_days TYPE /usi/if_bal_cust_validity=>ty_maximum_validity_in_days.

  GET BADI badi
    FILTERS
      log_object = /usi/bal_lv_clnt-log_object
      sub_object = /usi/bal_lv_clnt-sub_object.

  CALL BADI badi->get_maximum_validity_in_days
    RECEIVING
      r_result = maximum_number_of_days.

  PERFORM check_end_date USING    maximum_number_of_days
                         CHANGING /usi/bal_lv_clnt-endda.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_END_DATE_USER
*&---------------------------------------------------------------------*
*& Check end date of /usi/bal_lv_user-entry
*&---------------------------------------------------------------------*
FORM check_end_date_user.
  DATA: badi                   TYPE REF TO /usi/bal_cust_validity_user,
        maximum_number_of_days TYPE /usi/if_bal_cust_validity=>ty_maximum_validity_in_days.

  GET BADI badi
    FILTERS
      log_object = /usi/bal_lv_clnt-log_object
      sub_object = /usi/bal_lv_clnt-sub_object.

  CALL BADI badi->get_maximum_validity_in_days
    RECEIVING
      r_result = maximum_number_of_days.

  PERFORM check_end_date USING    maximum_number_of_days
                         CHANGING /usi/bal_lv_user-endda.

ENDFORM.
