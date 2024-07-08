CLASS /usi/cl_bal_dc_src_pos_caller DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_message_details.
    INTERFACES /usi/if_exception_details.
    INTERFACES /usi/if_bal_data_container.
    INTERFACES /usi/if_bal_data_container_nav.

    ALIASES get_classname FOR /usi/if_bal_data_container~get_classname.

    "! Constructor
    "!
    "! @parameter i_source_code_position | The to-be-logged Source-Code-Position of the caller
    METHODS constructor
      IMPORTING i_source_code_position TYPE /usi/bal_source_code_position.

  PRIVATE SECTION.
    DATA source_code_position TYPE /usi/bal_source_code_position.

ENDCLASS.


CLASS /usi/cl_bal_dc_src_pos_caller IMPLEMENTATION.
  METHOD /usi/if_bal_data_container_nav~navigate.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING  operation   = 'SHOW'
                 object_type = 'PROG'
                 object_name = source_code_position-program_name
                 include     = source_code_position-include_name
                 position    = source_code_position-source_line
      EXCEPTIONS OTHERS      = 0.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~deserialize.
    DATA source_code_position TYPE /usi/bal_source_code_position.

    NEW /usi/cl_bal_serializer( )->deserialize_field( EXPORTING i_serialized_data = i_serialized_data_container
                                                                i_name            = 'SOURCE_CODE_POSITION'
                                                      CHANGING  c_data            = source_code_position ).

    r_result = NEW /usi/cl_bal_dc_src_pos_caller( i_source_code_position = source_code_position ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_classname.
    r_result = '/USI/CL_BAL_DC_SRC_POS_CALLER'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_description.
    r_result = TEXT-des.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_false.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    r_result = NEW /usi/cl_bal_serializer( )->serialize_field_as_json( i_data = source_code_position
                                                                       i_name = 'SOURCE_CODE_POSITION' ).
  ENDMETHOD.

  METHOD constructor.
    source_code_position = i_source_code_position.
  ENDMETHOD.
ENDCLASS.
