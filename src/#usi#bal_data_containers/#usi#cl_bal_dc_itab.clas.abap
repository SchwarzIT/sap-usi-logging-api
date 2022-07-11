CLASS /usi/cl_bal_dc_itab DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPE-POOLS abap.

    INTERFACES /usi/if_bal_message_details.
    INTERFACES /usi/if_exception_details.
    INTERFACES /usi/if_bal_data_container.
    INTERFACES /usi/if_bal_data_container_rnd.

    ALIASES get_classname FOR /usi/if_bal_data_container~get_classname.

    "! Constructor
    "!
    "! @parameter i_internal_table | Internal table
    "! @parameter i_title | Optional: Title text (useful, if more than one Itab is appended)
    "! @parameter i_fieldcatalog | Optional: Field catalog
    METHODS constructor
      IMPORTING
        i_internal_table TYPE ANY TABLE
        i_title          TYPE REF TO /usi/if_bal_text_container_c40 OPTIONAL
        i_fieldcatalog   TYPE lvc_t_fcat OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: ty_fieldcatalog_name TYPE c LENGTH 10,
           BEGIN OF ty_fieldcatalog,
             name         TYPE ty_fieldcatalog_name,
             fieldcatalog TYPE lvc_t_fcat,
           END   OF ty_fieldcatalog,
           ty_fieldcatalogs TYPE HASHED TABLE OF ty_fieldcatalog WITH UNIQUE KEY name.

    CONSTANTS: BEGIN OF fieldcatalog_names,
                 external  TYPE ty_fieldcatalog_name VALUE 'EXTERNAL',
                 internal  TYPE ty_fieldcatalog_name VALUE 'INTERNAL',
                 technical TYPE ty_fieldcatalog_name VALUE 'TECHNICAL',
               END   OF fieldcatalog_names.

    CONSTANTS: BEGIN OF user_commands,
                 set_external_fcat  TYPE ui_func VALUE 'SET_FCAT_EXTERNAL',
                 set_internal_fcat  TYPE ui_func VALUE 'SET_FCAT_INTERNAL',
                 set_technical_fcat TYPE ui_func VALUE 'SET_FCAT_TECHNICAL',
               END   OF user_commands.

    DATA: fieldcatalog_table         TYPE ty_fieldcatalogs,
          internal_table_ref         TYPE REF TO data,
          selected_fieldcatalog_name TYPE ty_fieldcatalog_name,
          title                      TYPE REF TO /usi/if_bal_text_container_c40,
          table_name                 TYPE tabname.

    METHODS get_excluded_grid_functions
      RETURNING
        VALUE(r_result) TYPE ui_functions.

    METHODS get_field_catalog
      IMPORTING
        i_name          TYPE ty_fieldcatalog_name
      RETURNING
        VALUE(r_result) TYPE lvc_t_fcat
      RAISING
        /usi/cx_bal_root.

    METHODS get_layout
      RETURNING
        VALUE(r_result) TYPE lvc_s_layo.

    METHODS get_table_name_by_data
      IMPORTING
        i_internal_table TYPE ANY TABLE
      RETURNING
        VALUE(r_result)  TYPE tabname
      RAISING
        /usi/cx_bal_root.

    METHODS has_field_catalog
      IMPORTING
        i_name          TYPE ty_fieldcatalog_name
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS insert_fieldcatalog
      IMPORTING
        i_name         TYPE ty_fieldcatalog_name
        i_fieldcatalog TYPE lvc_t_fcat.

    METHODS merge_internal_fieldcatalog
      IMPORTING
        i_tabname       TYPE tabname
      RETURNING
        VALUE(r_result) TYPE lvc_t_fcat
      RAISING
        /usi/cx_bal_root.

    METHODS merge_technical_fieldcatalog
      IMPORTING
        i_tabname       TYPE tabname
      RETURNING
        VALUE(r_result) TYPE lvc_t_fcat
      RAISING
        /usi/cx_bal_root.

    METHODS on_alv_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        e_object.

    METHODS on_alv_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        e_ucomm
        sender.

    METHODS refresh_alv_output
      IMPORTING
        i_alv_grid TYPE REF TO cl_gui_alv_grid
      RAISING
        /usi/cx_bal_root.

    METHODS raise_exception_on_subrc
      RAISING
        /usi/cx_bal_root.

ENDCLASS.



CLASS /usi/cl_bal_dc_itab IMPLEMENTATION.
  METHOD /usi/if_bal_data_container_rnd~render.
    DATA: fieldcatalog TYPE lvc_t_fcat,
          alv_grid     TYPE REF TO cl_gui_alv_grid.

    " Create fieldcatalogs
    fieldcatalog = merge_internal_fieldcatalog( table_name ).
    insert_fieldcatalog( i_name         = fieldcatalog_names-internal
                         i_fieldcatalog = fieldcatalog ).

    fieldcatalog = merge_technical_fieldcatalog( table_name ).
    insert_fieldcatalog( i_name         = fieldcatalog_names-technical
                         i_fieldcatalog = fieldcatalog ).

    " Set initial field catalogs name
    IF has_field_catalog( fieldcatalog_names-external ) EQ abap_true.
      selected_fieldcatalog_name = fieldcatalog_names-external.
    ELSE.
      selected_fieldcatalog_name = fieldcatalog_names-internal.
    ENDIF.

    " Call controls
    CREATE OBJECT alv_grid
      EXPORTING
        i_parent          = i_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc NE 0.
      raise_exception_on_subrc( ).
    ENDIF.

    SET HANDLER on_alv_toolbar FOR alv_grid.
    SET HANDLER on_alv_user_command FOR alv_grid.

    refresh_alv_output( alv_grid ).
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~deserialize.
    DATA: exception             TYPE REF TO cx_root,
          exception_text        TYPE string,
          internal_table_ref    TYPE REF TO data,
          internal_table_xml    TYPE string,
          external_fieldcatalog TYPE lvc_t_fcat,
          serialized_title      TYPE /usi/bal_xml_string,
          title                 TYPE REF TO /usi/if_bal_text_container_c40,
          title_classname       TYPE /usi/bal_text_cont_classname,
          table_name            TYPE tabname.

    FIELD-SYMBOLS: <internal_table> TYPE STANDARD TABLE.

    TRY.

        CALL TRANSFORMATION id
          SOURCE XML i_serialized_data_container
          RESULT table_name             = table_name
                 internal_table_xml     = internal_table_xml
                 external_fieldcatalog  = external_fieldcatalog
                 title_classname        = title_classname
                 serialized_title       = serialized_title.

        IF title_classname IS NOT INITIAL.
          TRY.
              CALL METHOD (title_classname)=>/usi/if_bal_text_container_c40~deserialize
                EXPORTING
                  i_serialized_text_container = serialized_title
                RECEIVING
                  r_result                    = title.
            CATCH cx_sy_dyn_call_error
                  /usi/cx_bal_root INTO exception.
              exception_text = exception->get_text( ).
              ASSERT ID /usi/bal_log_writer
                FIELDS exception_text
                CONDITION exception IS NOT BOUND.

              CLEAR title.
          ENDTRY.
        ENDIF.

        CREATE DATA internal_table_ref TYPE STANDARD TABLE OF (table_name)
                                                WITH NON-UNIQUE DEFAULT KEY.
        ASSIGN internal_table_ref->* TO <internal_table>.

        CALL TRANSFORMATION id
          SOURCE XML internal_table_xml
          RESULT internal_table = <internal_table>.

        CREATE OBJECT r_result TYPE /usi/cl_bal_dc_itab
          EXPORTING
            i_internal_table = <internal_table>
            i_title          = title
            i_fieldcatalog   = external_fieldcatalog.

      CATCH cx_transformation_error
            /usi/cx_bal_invalid_input INTO exception.

        RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
          EXPORTING
            textid   = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch
            previous = exception.

    ENDTRY.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~get_classname.
    r_result = '/USI/CL_BAL_DC_ITAB'.
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


  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_true.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~serialize.
    DATA: external_fieldcatalog TYPE lvc_t_fcat,
          internal_table_xml    TYPE string,
          serialized_title      TYPE /usi/bal_xml_string,
          title_classname       TYPE /usi/bal_text_cont_classname.

    FIELD-SYMBOLS: <internal_table> TYPE STANDARD TABLE.

    IF internal_table_ref IS NOT BOUND.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING
          textid = /usi/cx_bal_invalid_input=>data_cont_use_ddic_line_type.
    ENDIF.

    ASSIGN internal_table_ref->* TO <internal_table>.
    CALL TRANSFORMATION id
      SOURCE internal_table = <internal_table>
      RESULT XML internal_table_xml.

    TRY.
        external_fieldcatalog = get_field_catalog( fieldcatalog_names-external ).
      CATCH /usi/cx_bal_root.
        CLEAR external_fieldcatalog.
    ENDTRY.

    IF title IS BOUND.
      serialized_title = title->serialize( ).
      title_classname  = title->get_classname( ).
    ENDIF.

    CALL TRANSFORMATION id
      SOURCE table_name             = table_name
             internal_table_xml     = internal_table_xml
             external_fieldcatalog  = external_fieldcatalog
             title_classname        = title_classname
             serialized_title       = serialized_title
      RESULT XML r_result.
  ENDMETHOD.


  METHOD constructor.
    DATA: exception      TYPE REF TO /usi/cx_bal_root,
          exception_text TYPE string.

    FIELD-SYMBOLS: <internal_table> TYPE STANDARD TABLE.

    TRY.
        table_name = get_table_name_by_data( i_internal_table ).
      CATCH /usi/cx_bal_root INTO exception.
        exception_text = exception->get_text( ).

        ASSERT ID /usi/bal_log_writer
          FIELDS exception_text
          CONDITION exception IS NOT BOUND.
        RETURN.
    ENDTRY.

    title = i_title.

    IF i_fieldcatalog IS NOT INITIAL.
      insert_fieldcatalog( i_name         = fieldcatalog_names-external
                           i_fieldcatalog = i_fieldcatalog ).
    ENDIF.

    CREATE DATA internal_table_ref TYPE STANDARD TABLE OF (table_name)
                                            WITH NON-UNIQUE DEFAULT KEY.
    ASSIGN internal_table_ref->* TO <internal_table>.
    <internal_table> = i_internal_table.
  ENDMETHOD.


  METHOD get_excluded_grid_functions.
    INSERT cl_gui_alv_grid=>mc_fc_col_invisible   INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_col_optimize    INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_current_variant INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_fix_columns     INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_graph           INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_info            INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_loc_copy        INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_print           INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_sort            INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_fc_unfix_columns   INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_mb_paste           INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_mb_subtot          INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_mb_sum             INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_mb_variant         INTO TABLE r_result.
    INSERT cl_gui_alv_grid=>mc_mb_view            INTO TABLE r_result.
  ENDMETHOD.


  METHOD get_field_catalog.
    FIELD-SYMBOLS: <fieldcatalog> TYPE ty_fieldcatalog.

    READ TABLE fieldcatalog_table
      WITH TABLE KEY name = i_name
      ASSIGNING <fieldcatalog>.
    IF sy-subrc EQ 0.
      r_result = <fieldcatalog>-fieldcatalog.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found.
    ENDIF.
  ENDMETHOD.


  METHOD get_layout.
    r_result-zebra       = abap_true.
    r_result-cwidth_opt  = abap_true.
  ENDMETHOD.


  METHOD get_table_name_by_data.
    DATA: type_description  TYPE REF TO cl_abap_typedescr,
          table_description TYPE REF TO cl_abap_tabledescr,
          line_description  TYPE REF TO cl_abap_datadescr.

    type_description = cl_abap_typedescr=>describe_by_data( i_internal_table ).
    table_description ?= type_description.
    line_description = table_description->get_table_line_type( ).

    IF line_description->is_ddic_type( ) NE abap_true OR
       line_description->kind            NE cl_abap_typedescr=>kind_struct.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING
          textid = /usi/cx_bal_invalid_input=>data_cont_use_ddic_line_type.
    ENDIF.

    r_result = line_description->get_relative_name( ).
  ENDMETHOD.


  METHOD has_field_catalog.
    READ TABLE fieldcatalog_table
      TRANSPORTING NO FIELDS
      WITH TABLE KEY name = i_name.
    IF sy-subrc EQ 0.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD insert_fieldcatalog.
    DATA new_fieldcatalog TYPE ty_fieldcatalog.

    new_fieldcatalog-name         = i_name.
    new_fieldcatalog-fieldcatalog = i_fieldcatalog.
    INSERT new_fieldcatalog INTO TABLE fieldcatalog_table.
  ENDMETHOD.


  METHOD merge_internal_fieldcatalog.
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = i_tabname
      CHANGING
        ct_fieldcat            = r_result
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc NE 0.
      raise_exception_on_subrc( ).
    ENDIF.
  ENDMETHOD.


  METHOD merge_technical_fieldcatalog.
    FIELD-SYMBOLS <fieldcatalog_line> TYPE lvc_s_fcat.

    r_result = merge_internal_fieldcatalog( i_tabname ).

    LOOP AT r_result ASSIGNING <fieldcatalog_line>.
      <fieldcatalog_line>-coltext   = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-scrtext_l = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-scrtext_m = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-scrtext_s = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-reptext   = <fieldcatalog_line>-fieldname.
    ENDLOOP.
  ENDMETHOD.


  METHOD on_alv_toolbar.
    DATA toolbar_button TYPE stb_button.

    IF has_field_catalog( fieldcatalog_names-external ) EQ abap_true.
      CLEAR toolbar_button.
      toolbar_button-function = user_commands-set_external_fcat.
      IF selected_fieldcatalog_name EQ fieldcatalog_names-external.
        toolbar_button-disabled = abap_true.
      ENDIF.
      toolbar_button-text     = 'External fieldcatalog'(b01).

      INSERT toolbar_button INTO TABLE e_object->mt_toolbar.
    ENDIF.

    CLEAR toolbar_button.
    toolbar_button-function = user_commands-set_internal_fcat.
    IF selected_fieldcatalog_name EQ fieldcatalog_names-internal.
      toolbar_button-disabled = abap_true.
    ENDIF.
    toolbar_button-text     = 'Regular fieldcatalog'(b02).
    INSERT toolbar_button INTO TABLE e_object->mt_toolbar.

    CLEAR toolbar_button.
    toolbar_button-function = user_commands-set_technical_fcat.
    IF selected_fieldcatalog_name EQ fieldcatalog_names-technical.
      toolbar_button-disabled = abap_true.
    ENDIF.
    toolbar_button-text     = 'Technical fieldnames'(b03).
    INSERT toolbar_button INTO TABLE e_object->mt_toolbar.
  ENDMETHOD.


  METHOD on_alv_user_command.
    DATA: alv_layout       TYPE lvc_s_layo,
          exception        TYPE REF TO /usi/cx_bal_root,
          gui_is_available TYPE abap_bool.

    CASE e_ucomm.
      WHEN user_commands-set_external_fcat.
        selected_fieldcatalog_name = fieldcatalog_names-external.
      WHEN user_commands-set_internal_fcat.
        selected_fieldcatalog_name = fieldcatalog_names-internal.
      WHEN user_commands-set_technical_fcat.
        selected_fieldcatalog_name = fieldcatalog_names-technical.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF has_field_catalog( selected_fieldcatalog_name ) NE abap_true.
      selected_fieldcatalog_name = fieldcatalog_names-internal.
    ENDIF.

    TRY.
        refresh_alv_output( sender ).
      CATCH /usi/cx_bal_root INTO exception.
        CALL FUNCTION 'GUI_IS_AVAILABLE'
          IMPORTING
            return = gui_is_available.

        IF gui_is_available EQ abap_true.
          MESSAGE exception TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
    ENDTRY.
  ENDMETHOD.


  METHOD raise_exception_on_subrc.
    DATA textid TYPE scx_t100key.

    textid-msgid = sy-msgid.
    textid-msgno = sy-msgno.
    textid-attr1 = 'PARAM1'.
    textid-attr2 = 'PARAM2'.
    textid-attr3 = 'PARAM3'.
    textid-attr4 = 'PARAM4'.

    RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
      EXPORTING
        textid = textid
        param1 = sy-msgv1
        param2 = sy-msgv2
        param3 = sy-msgv3
        param4 = sy-msgv4.
  ENDMETHOD.


  METHOD refresh_alv_output.
    DATA: alv_excluded_functions TYPE ui_functions,
          alv_layout             TYPE lvc_s_layo,
          field_catalog          TYPE lvc_t_fcat.

    FIELD-SYMBOLS <internal_table> TYPE STANDARD TABLE.
    ASSIGN internal_table_ref->* TO <internal_table>.

    alv_excluded_functions = get_excluded_grid_functions( ).
    alv_layout             = get_layout( ).

    TRY.
        field_catalog = get_field_catalog( selected_fieldcatalog_name ).
      CATCH /usi/cx_bal_root.
        CLEAR field_catalog.
    ENDTRY.

    i_alv_grid->set_table_for_first_display(
      EXPORTING
        is_layout            = alv_layout
        it_toolbar_excluding = alv_excluded_functions
      CHANGING
        it_outtab            = <internal_table>
        it_fieldcatalog      = field_catalog
      EXCEPTIONS
        OTHERS               = 0 ).
    IF sy-subrc NE 0.
      raise_exception_on_subrc( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
