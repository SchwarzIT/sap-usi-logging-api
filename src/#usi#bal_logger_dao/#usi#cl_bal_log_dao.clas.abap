CLASS /usi/cl_bal_log_dao DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_log_dao.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_log_object           | Log Object (Defined in SLG0)
    "! @parameter i_sub_object           | Sub Object (Defined in SLG0)
    "! @parameter i_external_id          | External ID of the log (Filter in SLG1; use processed objects ID if feasible)
    "! @parameter i_retention_parameters | Retention parameters
    "! @parameter i_context              | Context structure
    "! @parameter i_params               | Parameters for standard API
    "! @raising   /usi/cx_bal_root       | Error in standard API
    METHODS constructor
      IMPORTING i_log_object           TYPE balobj_d
                i_sub_object           TYPE balsubobj  OPTIONAL
                i_external_id          TYPE balnrext   OPTIONAL
                i_retention_parameters TYPE /usi/bal_retention_parameters
                i_context              TYPE bal_s_cont OPTIONAL
                i_params               TYPE bal_s_parm OPTIONAL
      RAISING   /usi/cx_bal_root.

  PRIVATE SECTION.
    DATA log_number TYPE balognr.

    METHODS get_log_header
      IMPORTING i_log_object           TYPE balobj_d
                i_sub_object           TYPE balsubobj
                i_external_id          TYPE balnrext
                i_retention_parameters TYPE /usi/bal_retention_parameters
                i_context              TYPE bal_s_cont
                i_params               TYPE bal_s_parm
      RETURNING VALUE(r_result)        TYPE bal_s_log.

    METHODS create_log
      IMPORTING i_log_header    TYPE bal_s_log
      RETURNING VALUE(r_result) TYPE balloghndl
      RAISING   /usi/cx_bal_root.

    METHODS get_data_container_rc_and_msg
      IMPORTING VALUE(i_return_code) TYPE sysubrc
      RETURNING VALUE(r_result)      TYPE REF TO /usi/if_bal_data_container.

ENDCLASS.


CLASS /usi/cl_bal_log_dao IMPLEMENTATION.
  METHOD /usi/if_bal_log_dao~add_message.
    DATA data_container TYPE REF TO /usi/if_bal_data_container.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING  i_log_handle     = /usi/if_bal_log_dao~log_handle
                 i_s_msg          = i_message
      EXCEPTIONS log_not_found    = 1
                 msg_inconsistent = 2
                 log_is_full      = 3
                 OTHERS           = 4.

    IF sy-subrc <> 0.
      data_container = get_data_container_rc_and_msg( sy-subrc ).
      RAISE EXCEPTION TYPE /usi/cx_bal_external_api_error
        EXPORTING textid  = /usi/cx_bal_external_api_error=>message_not_logged
                  details = data_container.
    ENDIF.
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~free.
    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING  i_log_handle = /usi/if_bal_log_dao~log_handle
      EXCEPTIONS OTHERS       = 0.
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~get_log_number.
    IF log_number IS NOT INITIAL.
      r_result = log_number.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_external_api_error
        EXPORTING textid = /usi/cx_bal_external_api_error=>save_log_first.
    ENDIF.
  ENDMETHOD.

  METHOD /usi/if_bal_log_dao~save.
    DATA: data_container TYPE REF TO /usi/if_bal_data_container,
          log_numbers    TYPE bal_t_lgnm.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING  i_t_log_handle       = VALUE bal_t_logh( ( /usi/if_bal_log_dao~log_handle ) )
                 i_2th_connection     = abap_true
                 i_2th_connect_commit = abap_true
      IMPORTING  e_new_lognumbers     = log_numbers
      EXCEPTIONS log_not_found        = 1
                 save_not_allowed     = 2
                 numbering_error      = 3
                 OTHERS               = 4.

    IF sy-subrc <> 0.
      data_container = get_data_container_rc_and_msg( sy-subrc ).
      RAISE EXCEPTION TYPE /usi/cx_bal_external_api_error
        EXPORTING textid  = /usi/cx_bal_external_api_error=>log_save_error
                  details = data_container.
    ELSEIF     log_number  IS INITIAL
           AND log_numbers IS NOT INITIAL.
      TRY.
          log_number = log_numbers[ extnumber  = /usi/if_bal_log_dao~log_header-extnumber
                                    log_handle = /usi/if_bal_log_dao~log_handle ]-lognumber.
        CATCH cx_sy_itab_line_not_found.
          CLEAR log_number.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    IF i_log_object IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>initial_log_object.
    ENDIF.

    /usi/if_bal_log_dao~log_header = get_log_header( i_log_object           = i_log_object
                                                     i_sub_object           = i_sub_object
                                                     i_external_id          = i_external_id
                                                     i_retention_parameters = i_retention_parameters
                                                     i_context              = i_context
                                                     i_params               = i_params ).
    /usi/if_bal_log_dao~log_handle = create_log( /usi/if_bal_log_dao~log_header ).
  ENDMETHOD.

  METHOD create_log.
    DATA data_container TYPE REF TO /usi/if_bal_data_container.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING  i_s_log                 = i_log_header
      IMPORTING  e_log_handle            = r_result
      EXCEPTIONS log_header_inconsistent = 1
                 OTHERS                  = 2.
    IF sy-subrc <> 0.
      data_container = get_data_container_rc_and_msg( sy-subrc ).
      RAISE EXCEPTION TYPE /usi/cx_bal_external_api_error
        EXPORTING textid  = /usi/cx_bal_external_api_error=>log_create_error
                  details = data_container.
    ENDIF.
  ENDMETHOD.

  METHOD get_data_container_rc_and_msg.
    DATA message TYPE symsg.

    MOVE-CORRESPONDING sy TO message.

    r_result = NEW /usi/cl_bal_dc_retcode_and_msg( i_message     = message
                                                   i_return_code = i_return_code ).
  ENDMETHOD.

  METHOD get_log_header.
    r_result-extnumber  = i_external_id.
    r_result-object     = i_log_object.
    r_result-subobject  = i_sub_object.

    r_result-aldate     = sy-datum.
    r_result-altime     = sy-uzeit.
    r_result-aluser     = sy-uname.
    r_result-altcode    = sy-tcode.
    r_result-alprog     = sy-repid.

    r_result-aldate_del = sy-datum + i_retention_parameters-retention_time.
    r_result-del_before = i_retention_parameters-no_early_delete.

    r_result-context    = i_context.
    r_result-params     = i_params.
  ENDMETHOD.
ENDCLASS.
