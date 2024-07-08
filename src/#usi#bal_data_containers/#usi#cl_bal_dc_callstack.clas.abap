CLASS /usi/cl_bal_dc_callstack DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_message_details.
    INTERFACES /usi/if_exception_details.
    INTERFACES /usi/if_bal_data_container.
    INTERFACES /usi/if_bal_data_container_rnd.

    ALIASES get_classname FOR /usi/if_bal_data_container~get_classname.

    "! Constructor
    "!
    "! @parameter i_callstack | The to-be-logged callstack
    METHODS constructor
      IMPORTING
        i_callstack TYPE abap_callstack.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: callstack    TYPE abap_callstack,
          fieldcatalog TYPE lvc_t_fcat.

    METHODS get_excluded_grid_functions
      RETURNING
        VALUE(r_result) TYPE ui_functions.

    METHODS get_fieldcatalog
      RETURNING
        VALUE(r_result) TYPE lvc_t_fcat.

    METHODS get_layout
      RETURNING
        VALUE(r_result) TYPE lvc_s_layo.

    METHODS on_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        es_row_no.

ENDCLASS.



CLASS /usi/cl_bal_dc_callstack IMPLEMENTATION.
  METHOD /usi/if_bal_data_container_rnd~render.
    DATA: alv_grid           TYPE REF TO cl_gui_alv_grid,
          excluded_functions TYPE ui_functions,
          layout             TYPE lvc_s_layo.

    excluded_functions = get_excluded_grid_functions( ).
    layout             = get_layout( ).
    fieldcatalog       = get_fieldcatalog( ).

    alv_grid = NEW #( i_parent = i_container ).

    SET HANDLER on_double_click FOR alv_grid.

    alv_grid->set_table_for_first_display( EXPORTING is_layout            = layout
                                                     it_toolbar_excluding = excluded_functions
                                           CHANGING  it_outtab            = callstack
                                                     it_fieldcatalog      = fieldcatalog ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~deserialize.
    DATA: callstack    TYPE abap_callstack,
          deserializer TYPE REF TO /usi/cl_bal_serializer.

    deserializer = NEW #( ).
    deserializer->deserialize_field( EXPORTING i_serialized_data = i_serialized_data_container
                                               i_name            = 'CALLSTACK'
                                     CHANGING  c_data            = callstack ).

    r_result = NEW /usi/cl_bal_dc_callstack( i_callstack = callstack ).
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~get_classname.
    r_result = '/USI/CL_BAL_DC_CALLSTACK'.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~get_description.
    r_result = TEXT-des.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_false.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    DATA serializer TYPE REF TO /usi/cl_bal_serializer.

    serializer = NEW #( ).
    r_result = serializer->serialize_field_as_json( i_data = callstack
                                                    i_name = 'CALLSTACK' ).
  ENDMETHOD.


  METHOD constructor.
    callstack = i_callstack.
  ENDMETHOD.


  METHOD get_excluded_grid_functions.
    INSERT cl_gui_alv_grid=>mc_fc_excl_all        INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_loc_copy        INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_col_optimize    INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_unfix_columns   INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_fix_columns     INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_col_invisible   INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_current_variant INTO TABLE r_result.
  ENDMETHOD.


  METHOD get_fieldcatalog.
    FIELD-SYMBOLS <fieldcatalog_line> TYPE lvc_s_fcat.

    IF fieldcatalog IS NOT INITIAL.
      r_result = fieldcatalog.
      RETURN.
    ENDIF.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name = 'ABAP_CALLSTACK_LINE'
      CHANGING
        ct_fieldcat      = r_result
      EXCEPTIONS
        OTHERS           = 0.

    LOOP AT r_result ASSIGNING <fieldcatalog_line>.

      CASE <fieldcatalog_line>-fieldname.
        WHEN 'FLAG_SYSTEM'.
          <fieldcatalog_line>-no_out = abap_true.
          CONTINUE.
        WHEN 'LINE'.
          <fieldcatalog_line>-reptext = TEXT-c01.
        WHEN 'BLOCKTYPE'.
          <fieldcatalog_line>-reptext = TEXT-c02.
        WHEN 'BLOCKNAME'.
          <fieldcatalog_line>-reptext = TEXT-c03.
      ENDCASE.

      IF <fieldcatalog_line>-reptext IS INITIAL.
        <fieldcatalog_line>-reptext = <fieldcatalog_line>-fieldname.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_layout.
    r_result-zebra      = abap_true.
    r_result-cwidth_opt = abap_true.
  ENDMETHOD.

  METHOD on_double_click.
    FIELD-SYMBOLS <callstack_line> TYPE abap_callstack_line.

    ASSIGN callstack[ es_row_no-row_id ] TO <callstack_line>.
    IF sy-subrc = 0.
      CALL FUNCTION 'RS_TOOL_ACCESS'
        EXPORTING  operation   = 'SHOW'
                   object_type = 'PROG'
                   object_name = <callstack_line>-mainprogram
                   include     = <callstack_line>-include
                   position    = <callstack_line>-line
        EXCEPTIONS OTHERS      = 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
