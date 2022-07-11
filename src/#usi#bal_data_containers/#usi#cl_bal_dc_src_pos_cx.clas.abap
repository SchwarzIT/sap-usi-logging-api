CLASS /usi/cl_bal_dc_src_pos_cx DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_message_details.
    INTERFACES /usi/if_exception_details.
    INTERFACES /usi/if_bal_data_container.
    INTERFACES /usi/if_bal_data_container_nav.

    ALIASES get_classname FOR /usi/if_bal_data_container~get_classname.

    "! Constructor
    "!
    "! @parameter i_source_code_position | The to-be-logged Source-Code-Position of the exception
    METHODS constructor
      IMPORTING
        i_source_code_position TYPE /usi/bal_source_code_position.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA source_code_position TYPE /usi/bal_source_code_position.

ENDCLASS.



CLASS /usi/cl_bal_dc_src_pos_cx IMPLEMENTATION.
  METHOD /usi/if_bal_data_container_nav~navigate.
    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation   = 'SHOW'
        object_type = 'PROG'
        object_name = source_code_position-program_name
        include     = source_code_position-include_name
        position    = source_code_position-source_line
      EXCEPTIONS
        OTHERS      = 0.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~deserialize.
    DATA: exception            TYPE REF TO cx_transformation_error,
          source_code_position TYPE /usi/bal_source_code_position.

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML i_serialized_data_container
          RESULT source_code_position = source_code_position.

        CREATE OBJECT r_result TYPE /usi/cl_bal_dc_src_pos_cx
          EXPORTING
            i_source_code_position = source_code_position.
      CATCH cx_transformation_error INTO exception.
        RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
          EXPORTING
            textid   = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch
            previous = exception.
    ENDTRY.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~get_classname.
    r_result = '/USI/CL_BAL_DC_SRC_POS_CX'.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~get_description.
    r_result = TEXT-des.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_false.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~serialize.
    CALL TRANSFORMATION id
      SOURCE source_code_position = source_code_position
      RESULT XML r_result.
  ENDMETHOD.


  METHOD constructor.
    source_code_position = i_source_code_position.
  ENDMETHOD.
ENDCLASS.
