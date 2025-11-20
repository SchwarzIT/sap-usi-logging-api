CLASS /usi/cl_bal_dc_parent_log DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_message_details.
    INTERFACES /usi/if_exception_details.
    INTERFACES /usi/if_bal_data_container.
    INTERFACES /usi/if_bal_data_container_nav.

    ALIASES get_classname FOR /usi/if_bal_data_container~get_classname.

    "! Constructor
    "!
    "! @parameter i_log_handle | Log handle of parent log
    METHODS constructor
      IMPORTING i_log_handle type balloghndl.

  PRIVATE SECTION.
    DATA log_handle type balloghndl.

ENDCLASS.


CLASS /usi/cl_bal_dc_parent_log IMPLEMENTATION.
  METHOD constructor.
    log_handle = i_log_handle.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container_nav~navigate.
    DATA log_handles TYPE bal_t_logh.

    log_handles = VALUE #( ( log_handle ) ).

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING  i_t_log_handle                 = log_handles
                 i_handle_from_specified_client = abap_true
      EXCEPTIONS no_logs_specified              = 1                " No logs specified
                 log_not_found                  = 2                " Log not found
                 log_already_loaded             = 3                " Log is already loaded
                 OTHERS                         = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING  i_t_log_handle       = log_handles
                 i_s_log_filter       = VALUE bal_s_lfil( log_handle = VALUE #( ( sign   = 'I'
                                                                                  option = 'EQ'
                                                                                  low    = log_handle ) ) )
                 i_amodal             = abap_true
      EXCEPTIONS profile_inconsistent = 1
                 internal_error       = 2
                 no_data_available    = 3
                 no_authority         = 4
                 OTHERS               = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~deserialize.
    DATA log_handle type balloghndl.

    NEW /usi/cl_bal_serializer( )->deserialize_field( EXPORTING i_serialized_data = i_serialized_data_container
                                                                i_name            = 'LOG_HANDLE'
                                                      CHANGING  c_data            = log_handle ).

    r_result = NEW /usi/cl_bal_dc_parent_log( log_handle ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_classname.
    r_result = '/USI/CL_BAL_DC_PARENT_LOG'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_description.
    r_result = TEXT-des.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_false.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    r_result = NEW /usi/cl_bal_serializer( )->serialize_field_as_json( i_data = log_handle
                                                                       i_name = 'LOG_HANDLE' ).
  ENDMETHOD.
ENDCLASS.
