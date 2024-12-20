*&---------------------------------------------------------------------*
*& Include          /USI/BAL_DEMO_02_P02
*&---------------------------------------------------------------------*
CLASS lcl_task_grid IMPLEMENTATION.
  METHOD constructor.
    field_catalog     = get_field_catalog( ).
    layout            = get_layout( ).
    toolbar_excluding = get_toolbar_excluding( ).

    CREATE OBJECT custom_container
      EXPORTING  container_name = 'CC_MAIN'
      EXCEPTIONS OTHERS         = 0.

    CREATE OBJECT alv_grid
      EXPORTING  i_parent = custom_container
      EXCEPTIONS OTHERS   = 0.

    SET HANDLER on_toolbar FOR alv_grid.
  ENDMETHOD.

  METHOD get_field_catalog.
    FIELD-SYMBOLS <field> TYPE lvc_s_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING  i_structure_name = '/USI/BAL_DEMO_TO_DO_TASK'
      CHANGING   ct_fieldcat      = r_result
      EXCEPTIONS OTHERS           = 0.

    ASSIGN r_result[ fieldname = 'ID' ] TO <field>.
    IF sy-subrc = 0.
      <field>-no_out = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD get_layout.
    CONSTANTS selection_mode_multiple_rows TYPE lvc_libox VALUE 'A'.

    r_result-zebra    = abap_true.
    r_result-sel_mode = selection_mode_multiple_rows.
  ENDMETHOD.

  METHOD get_toolbar_excluding.
    INSERT cl_gui_alv_grid=>mc_mb_view            INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_mb_export          INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_mb_sum             INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_mb_subtot          INTO TABLE r_result.

    INSERT cl_gui_alv_grid=>mc_fc_detail          INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_print           INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_current_variant INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_graph           INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_info            INTO TABLE r_result.
  ENDMETHOD.

  METHOD on_toolbar.
    CONSTANTS: BEGIN OF button_types,
                 button    TYPE tb_btype VALUE 0,
                 separator TYPE tb_btype VALUE 3,
               END   OF button_types.

    INSERT VALUE #( function  = user_commands-refresh
                    butn_type = button_types-button
                    text      = TEXT-b01
                    icon      = icon_refresh )
           INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( butn_type = button_types-separator )
           INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( function  = user_commands-add_task
                    butn_type = button_types-button
                    text      = TEXT-b02
                    icon      = icon_insert_row )
           INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( function  = user_commands-edit_task
                    butn_type = button_types-button
                    text      = TEXT-b03
                    icon      = icon_change_text )
           INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( function  = user_commands-delete_tasks
                    butn_type = button_types-button
                    text      = TEXT-b04
                    icon      = icon_delete_row )
           INTO TABLE e_object->mt_toolbar.
  ENDMETHOD.

  METHOD refresh_alv_grid.
    tasks = i_tasks.

    alv_grid->set_table_for_first_display( EXPORTING  is_layout            = layout
                                                      it_toolbar_excluding = toolbar_excluding
                                           CHANGING   it_outtab            = tasks
                                                      it_fieldcatalog      = field_catalog
                                           EXCEPTIONS OTHERS               = 0 ).
  ENDMETHOD.

  METHOD get_selected_task_ids.
    DATA row_numbers TYPE lvc_t_roid.

    FIELD-SYMBOLS: <row_number> TYPE lvc_s_roid,
                   <task>       TYPE /usi/bal_demo_to_do_task.

    alv_grid->get_selected_rows( IMPORTING et_row_no = row_numbers ).

    LOOP AT row_numbers ASSIGNING <row_number>.
      ASSIGN tasks[ <row_number>-row_id ] TO <task>.
      INSERT <task>-id INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
