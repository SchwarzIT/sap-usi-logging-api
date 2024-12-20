CLASS /usi/cl_bal_dc_retcode_and_msg DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_message_details.
    INTERFACES /usi/if_bal_data_container.
    INTERFACES /usi/if_exception_details.
    INTERFACES /usi/if_bal_data_container_rnd.

    ALIASES get_classname FOR /usi/if_bal_data_container~get_classname.

    "! Constructor
    "!
    "! @parameter i_message     | The Message
    "! @parameter i_return_code | The return code
    METHODS constructor
      IMPORTING i_message            TYPE symsg
                VALUE(i_return_code) TYPE sysubrc.

  PRIVATE SECTION.
    TYPES ty_alv_output TYPE STANDARD TABLE OF /usi/bal_fieldname_and_value WITH EMPTY KEY.

    DATA: BEGIN OF alv_data,
            fieldcat TYPE lvc_t_fcat,
            output   TYPE ty_alv_output,
          END   OF alv_data,
          message     TYPE symsg,
          return_code TYPE sysubrc.

    METHODS get_alv_output_table
      RETURNING VALUE(r_result) TYPE ty_alv_output.

ENDCLASS.


CLASS /usi/cl_bal_dc_retcode_and_msg IMPLEMENTATION.
  METHOD /usi/if_bal_data_container_rnd~render.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING  i_structure_name = '/USI/BAL_FIELDNAME_AND_VALUE'
      CHANGING   ct_fieldcat      = alv_data-fieldcat
      EXCEPTIONS OTHERS           = 0.

    alv_data-output = get_alv_output_table( ).

    DATA(alv_grid) = NEW cl_gui_alv_grid( i_parent = i_container ).

    alv_grid->set_table_for_first_display(
      EXPORTING is_layout            = VALUE #( zebra      = abap_true
                                                cwidth_opt = abap_true )
                it_toolbar_excluding = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) )
      CHANGING  it_outtab            = alv_data-output
                it_fieldcatalog      = alv_data-fieldcat ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~deserialize.
    DATA: parameters  TYPE abap_trans_resbind_tab,
          message     TYPE symsg,
          return_code TYPE sysubrc.

    parameters = VALUE #( ( name  = 'MESSAGE'
                            value = REF #( message ) )
                          ( name  = 'RETURN_CODE'
                            value = REF #( return_code ) ) ).

    NEW /usi/cl_bal_serializer( )->deserialize_fields( i_serialized_data = i_serialized_data_container
                                                       i_parameters      = parameters ).

    r_result = NEW /usi/cl_bal_dc_retcode_and_msg( i_message     = message
                                                   i_return_code = return_code ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_classname.
    r_result = '/USI/CL_BAL_DC_RETCODE_AND_MSG'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_description.
    r_result = TEXT-des.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_false.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    DATA parameters TYPE abap_trans_srcbind_tab.

    parameters = VALUE #( ( name  = 'MESSAGE'
                            value = REF #( message ) )
                          ( name  = 'RETURN_CODE'
                            value = REF #( return_code ) ) ).

    r_result = NEW /usi/cl_bal_serializer( )->serialize_fields_as_json( parameters ).
  ENDMETHOD.

  METHOD constructor.
    message     = i_message.
    return_code = i_return_code.
  ENDMETHOD.

  METHOD get_alv_output_table.
    DATA result_line TYPE /usi/bal_fieldname_and_value.

    result_line-fieldname = 'SUBRC'.
    WRITE return_code TO result_line-value LEFT-JUSTIFIED.
    INSERT result_line INTO TABLE r_result.

    result_line-fieldname = 'MSGTY'.
    result_line-value     = message-msgty.
    INSERT result_line INTO TABLE r_result.

    result_line-fieldname = 'MSGNO'.
    result_line-value     = message-msgno.
    INSERT result_line INTO TABLE r_result.

    result_line-fieldname = 'MSGID'.
    result_line-value     = message-msgid.
    INSERT result_line INTO TABLE r_result.

    result_line-fieldname = 'MSGV1'.
    result_line-value     = message-msgv1.
    INSERT result_line INTO TABLE r_result.

    result_line-fieldname = 'MSGV2'.
    result_line-value     = message-msgv2.
    INSERT result_line INTO TABLE r_result.

    result_line-fieldname = 'MSGV3'.
    result_line-value     = message-msgv3.
    INSERT result_line INTO TABLE r_result.

    result_line-fieldname = 'MSGV4'.
    result_line-value     = message-msgv4.
    INSERT result_line INTO TABLE r_result.

    IF message-msgid IS NOT INITIAL.
      result_line-fieldname = 'MESSAGE'.
      MESSAGE ID message-msgid TYPE 'S' NUMBER message-msgno
              WITH message-msgv1 message-msgv2 message-msgv3 message-msgv4
              INTO result_line-value.
      INSERT result_line INTO TABLE r_result.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
