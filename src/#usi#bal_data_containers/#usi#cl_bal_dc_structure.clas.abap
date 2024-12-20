CLASS /usi/cl_bal_dc_structure DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_container_rnd.

    "! Constructor
    "!
    "! @parameter i_structure | A Structure
    "! @parameter i_title     | Text container
    METHODS constructor
      IMPORTING i_structure TYPE any
                i_title     TYPE REF TO /usi/if_bal_text_container_c40 OPTIONAL.

  PRIVATE SECTION.
    TYPES ty_alv_output TYPE STANDARD TABLE OF /usi/bal_fieldname_and_value WITH EMPTY KEY.

    CONSTANTS: BEGIN OF c_parameter_names,
                 title_classname  TYPE fieldname VALUE 'TITLE_CLASSNAME',
                 serialized_title TYPE fieldname VALUE 'SERIALIZED_TITLE',
                 alv_output       TYPE fieldname VALUE 'ALV_OUTPUT',
               END   OF c_parameter_names.

    DATA: fieldcat   TYPE lvc_t_fcat,
          alv_output TYPE REF TO ty_alv_output,
          structure  TYPE REF TO data,
          title      TYPE REF TO /usi/if_bal_text_container_c40.

    METHODS build_alv_output
      RAISING /usi/cx_bal_root.

    METHODS resolve_structure
      IMPORTING i_structure_ref         TYPE REF TO data
                i_structure_description TYPE REF TO cl_abap_structdescr
                i_structure_suffix      TYPE string OPTIONAL
      RETURNING VALUE(r_result)         TYPE ty_alv_output
      RAISING   /usi/cx_bal_root.

    METHODS resolve_elementary_field
      IMPORTING i_structure_ref  TYPE REF TO data
                i_component_name TYPE string
      RETURNING VALUE(r_result)  TYPE ty_alv_output.

ENDCLASS.


CLASS /usi/cl_bal_dc_structure IMPLEMENTATION.
  METHOD /usi/if_bal_data_container~get_classname.
    r_result = '/USI/CL_BAL_DC_STRUCTURE'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_true.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~deserialize.
    DATA: BEGIN OF deserialized_data,
            title_classname  TYPE classname,
            serialized_title TYPE /usi/bal_xml_string,
            output           TYPE ty_alv_output,
          END   OF deserialized_data,
          title TYPE REF TO /usi/if_bal_text_container_c40.

    NEW /usi/cl_bal_serializer( )->deserialize_fields(
            i_serialized_data = i_serialized_data_container
            i_parameters      = VALUE #( ( name  = c_parameter_names-title_classname
                                           value = REF #( deserialized_data-title_classname ) )
                                         ( name  = c_parameter_names-serialized_title
                                           value = REF #( deserialized_data-serialized_title ) )
                                         ( name  = c_parameter_names-alv_output
                                           value = REF #( deserialized_data-output ) ) ) ).

    IF deserialized_data-title_classname IS NOT INITIAL.
      TRY.
          CALL METHOD (deserialized_data-title_classname)=>/usi/if_bal_text_container_c40~deserialize
            EXPORTING i_serialized_text_container = deserialized_data-serialized_title
            RECEIVING r_result                    = title.
        CATCH cx_sy_dyn_call_error
              /usi/cx_bal_root INTO DATA(exception).
          DATA(exception_text) = exception->get_text( ).
          ASSERT ID /usi/bal_log_writer
                 FIELDS exception_text
                 CONDITION exception IS NOT BOUND.

          CLEAR title.
      ENDTRY.
    ELSE.
      CLEAR title.
    ENDIF.

    DATA(result) = NEW /usi/cl_bal_dc_structure( i_structure = c_parameter_names
                                                 i_title     = title ).
    CREATE DATA result->alv_output.
    result->alv_output->* = deserialized_data-output.

    r_result = result.
  ENDMETHOD.

  METHOD constructor.
    CREATE DATA structure LIKE i_structure.
    ASSIGN structure->* TO FIELD-SYMBOL(<structure>).
    <structure> = i_structure.

    title = i_title.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    DATA parameters TYPE abap_trans_srcbind_tab.

    build_alv_output( ).

    IF title IS BOUND.
      parameters = VALUE #( ( name  = c_parameter_names-title_classname
                              value = NEW classname( title->get_classname( ) ) )
                            ( name  = c_parameter_names-serialized_title
                              value = NEW /usi/bal_xml_string( title->serialize( ) ) ) ).
    ENDIF.
    INSERT VALUE #( name  = c_parameter_names-alv_output
                    value = alv_output ) INTO TABLE parameters.

    r_result = NEW /usi/cl_bal_serializer( )->serialize_fields_as_xml( parameters ).
  ENDMETHOD.

  METHOD build_alv_output.
    IF alv_output IS BOUND.
      RETURN.
    ENDIF.

    TRY.
        DATA(structure_description) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data_ref( structure ) ).
      CATCH cx_sy_move_cast_error INTO DATA(type_mismatch).
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input
                    previous = type_mismatch.
    ENDTRY.

    CREATE DATA alv_output.
    alv_output->* = resolve_structure( i_structure_ref         = structure
                                       i_structure_description = structure_description ).

    IF alv_output->* IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_structure.
    ENDIF.
  ENDMETHOD.

  METHOD resolve_structure.
    LOOP AT i_structure_description->get_components( ) REFERENCE INTO DATA(component).
      CASE component->type->kind.
        WHEN cl_abap_typedescr=>kind_elem.
          INSERT LINES OF resolve_elementary_field( i_structure_ref  = i_structure_ref
                                                    i_component_name = |{ component->name }{ i_structure_suffix }| )
                 INTO TABLE r_result.

        WHEN cl_abap_typedescr=>kind_struct.
          IF component->as_include = abap_true.
            INSERT LINES OF resolve_structure( i_structure_ref         = i_structure_ref
                                               i_structure_description = CAST #( component->type )
                                               i_structure_suffix      = |{ component->suffix }{ i_structure_suffix }| )
                   INTO TABLE r_result.

          ELSE.
            ASSIGN i_structure_ref->* TO FIELD-SYMBOL(<main_structure>).
            ASSIGN COMPONENT component->name OF STRUCTURE <main_structure> TO FIELD-SYMBOL(<sub_structure>).

            TRY.
                DATA(structure_description) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data(
                                                                            <sub_structure> ) ).
              CATCH cx_sy_move_cast_error INTO DATA(type_mismatch).
                RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
                  EXPORTING textid   = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input
                            previous = type_mismatch.
            ENDTRY.

            INSERT LINES OF resolve_structure( i_structure_ref         = REF #( <sub_structure> )
                                               i_structure_description = structure_description )
                   INTO TABLE r_result.

          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD resolve_elementary_field.
    ASSIGN i_structure_ref->* TO FIELD-SYMBOL(<structure>).
    ASSIGN COMPONENT i_component_name OF STRUCTURE <structure> TO FIELD-SYMBOL(<field>).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    INSERT VALUE #( fieldname = i_component_name
                    value     = <field> )
           INTO TABLE r_result.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_description.
    DATA title_text TYPE /usi/if_bal_text_container_c40=>ty_text.

    IF title IS BOUND.
      title_text = title->get_text( ).
    ENDIF.

    r_result = TEXT-des.
    IF title_text IS NOT INITIAL.
      CONCATENATE r_result `: ` title_text INTO r_result IN CHARACTER MODE.
    ENDIF.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container_rnd~render.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING  i_structure_name = '/USI/BAL_FIELDNAME_AND_VALUE'
      CHANGING   ct_fieldcat      = fieldcat
      EXCEPTIONS OTHERS           = 0.

    build_alv_output( ).

    DATA(alv_grid) = NEW cl_gui_alv_grid( i_container ).
    alv_grid->set_table_for_first_display(
      EXPORTING is_layout            = VALUE #( cwidth_opt = abap_true
                                                zebra      = abap_true )
                it_toolbar_excluding = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) )
      CHANGING  it_outtab            = alv_output->*
                it_fieldcatalog      = fieldcat ).
  ENDMETHOD.
ENDCLASS.
