*----------------------------------------------------------------------*
***INCLUDE /USI/LBAL_CUST_TMF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_END_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_end_date.
  CONSTANTS maximum_number_of_days TYPE int1 VALUE 14.
  DATA maximum_allowed_date TYPE endda.

  IF /usi/bal_lv_user-endda LT sy-datum.
    /usi/bal_lv_user-endda = sy-datum.
    MESSAGE w024(/usi/bal).

  ELSE.
    maximum_allowed_date = sy-datum + maximum_number_of_days.
    IF /usi/bal_lv_user-endda GT maximum_allowed_date.
      /usi/bal_lv_user-endda = maximum_allowed_date.
      MESSAGE w022(/usi/bal) WITH maximum_number_of_days.
    ENDIF.

  ENDIF.
ENDFORM.
