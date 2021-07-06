INTERFACE /usi/if_bal_logger
  PUBLIC .

  TYPE-POOLS abap .

  EVENTS instance_invalidated .

  METHODS add_exception
    IMPORTING
      !i_problem_class   TYPE REF TO /usi/cl_bal_enum_problem_class DEFAULT /usi/cl_bal_enum_problem_class=>other
      !i_detail_level    TYPE REF TO /usi/cl_bal_enum_detail_level DEFAULT /usi/cl_bal_enum_detail_level=>detail_level_1
      !i_message_type    TYPE REF TO /usi/cl_bal_enum_message_type DEFAULT /usi/cl_bal_enum_message_type=>error
      !i_exception       TYPE REF TO cx_root
      !i_log_previous    TYPE abap_bool DEFAULT abap_true
      !i_details         TYPE REF TO /usi/if_bal_message_details OPTIONAL
      !i_message_context TYPE bal_s_cont OPTIONAL .
  METHODS add_free_text
    IMPORTING
      !i_problem_class   TYPE REF TO /usi/cl_bal_enum_problem_class DEFAULT /usi/cl_bal_enum_problem_class=>other
      !i_detail_level    TYPE REF TO /usi/cl_bal_enum_detail_level DEFAULT /usi/cl_bal_enum_detail_level=>detail_level_1
      !i_message_type    TYPE REF TO /usi/cl_bal_enum_message_type DEFAULT /usi/cl_bal_enum_message_type=>error
      !i_free_text       TYPE csequence
      !i_details         TYPE REF TO /usi/if_bal_message_details OPTIONAL
      !i_message_context TYPE bal_s_cont OPTIONAL .
  METHODS add_message
    IMPORTING
      !i_problem_class      TYPE REF TO /usi/cl_bal_enum_problem_class DEFAULT /usi/cl_bal_enum_problem_class=>other
      !i_detail_level       TYPE REF TO /usi/cl_bal_enum_detail_level DEFAULT /usi/cl_bal_enum_detail_level=>detail_level_1
      !i_message_type       TYPE REF TO /usi/cl_bal_enum_message_type DEFAULT /usi/cl_bal_enum_message_type=>error
      !i_message_class      TYPE symsgid
      !i_message_number     TYPE symsgno
      !i_message_variable_1 TYPE simple OPTIONAL
      !i_message_variable_2 TYPE simple OPTIONAL
      !i_message_variable_3 TYPE simple OPTIONAL
      !i_message_variable_4 TYPE simple OPTIONAL
      !i_details            TYPE REF TO /usi/if_bal_message_details OPTIONAL
      !i_message_context    TYPE bal_s_cont OPTIONAL .
  METHODS claim_ownership
    RETURNING
      VALUE(r_result) TYPE REF TO /usi/if_bal_token .
  METHODS free
    IMPORTING
      !i_token TYPE REF TO /usi/if_bal_token .
  METHODS save
    IMPORTING
      !i_token TYPE REF TO /usi/if_bal_token .
ENDINTERFACE.
