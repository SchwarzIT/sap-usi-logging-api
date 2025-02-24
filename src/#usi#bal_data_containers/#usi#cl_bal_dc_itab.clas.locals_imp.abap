*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
TYPES: BEGIN OF ty_field_mapping_line,
         target_field_name TYPE fieldname,
         source_field_path TYPE string,
       END   OF ty_field_mapping_line,
       ty_field_mapping TYPE HASHED TABLE OF ty_field_mapping_line WITH UNIQUE KEY target_field_name.

CLASS lcl_table_descriptor DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get_by_tabname
      IMPORTING i_tabname       TYPE tabname
      RETURNING VALUE(r_result) TYPE REF TO lcl_table_descriptor
      RAISING   /usi/cx_bal_root.

    CLASS-METHODS get_by_data
      IMPORTING i_table         TYPE ANY TABLE
      RETURNING VALUE(r_result) TYPE REF TO lcl_table_descriptor
      RAISING   /usi/cx_bal_root.

    CLASS-METHODS get_by_fieldcatalog
      IMPORTING i_fieldcatalog  TYPE lvc_t_fcat
      RETURNING VALUE(r_result) TYPE REF TO lcl_table_descriptor
      RAISING   /usi/cx_bal_root.

    METHODS constructor
      IMPORTING i_line_type_description TYPE REF TO cl_abap_typedescr
      RAISING   /usi/cx_bal_root.

    METHODS get_line_type_dref
      RETURNING VALUE(r_result) TYPE REF TO data.

    METHODS get_table_type_dref
      RETURNING VALUE(r_result) TYPE REF TO data.

    METHODS get_fieldcatalog
      RETURNING VALUE(r_result) TYPE lvc_t_fcat.

    METHODS get_tabname
      RETURNING VALUE(r_result) TYPE tabname
      RAISING   /usi/cx_bal_root.

    METHODS get_field_mapping
      RETURNING VALUE(r_result) TYPE ty_field_mapping
      RAISING   /usi/cx_bal_root.

  PRIVATE SECTION.
    DATA: BEGIN OF type_descriptions,
            line  TYPE REF TO cl_abap_structdescr,
            table TYPE REF TO cl_abap_tabledescr,
          END   OF type_descriptions.

    DATA: BEGIN OF buffers,
            fieldcatalog  TYPE REF TO lvc_t_fcat,
            field_mapping TYPE REF TO ty_field_mapping,
          END   OF buffers.

    METHODS get_normalized_line_type_desc
      IMPORTING i_line_type_description TYPE REF TO cl_abap_typedescr
      RETURNING VALUE(r_result)         TYPE REF TO cl_abap_structdescr
      RAISING   /usi/cx_bal_root.

    METHODS remove_non_elementary_fields
      IMPORTING i_line_type_description TYPE REF TO cl_abap_structdescr
      RETURNING VALUE(r_result)         TYPE REF TO cl_abap_structdescr
      RAISING   /usi/cx_bal_root.

    METHODS get_elementary_fields
      IMPORTING i_line_type_description TYPE REF TO cl_abap_structdescr
                i_source_structure_path TYPE string
      RETURNING VALUE(r_result)         TYPE abap_component_tab.

    METHODS extend_field_map
      IMPORTING i_proposed_fieldname TYPE fieldname
                i_source_field_path  TYPE string
      RETURNING VALUE(r_result)      TYPE fieldname
      RAISING   /usi/cx_bal_root.

    CLASS-METHODS get_type_by_fieldcatalog_line
      IMPORTING i_fieldcatalog_line TYPE lvc_s_fcat
      RETURNING VALUE(r_result)     TYPE REF TO cl_abap_elemdescr
      RAISING   /usi/cx_bal_root.

    CLASS-METHODS get_type_by_ddic_reference
      IMPORTING i_tabname       TYPE tabname
                i_fieldname     TYPE fieldname
      RETURNING VALUE(r_result) TYPE REF TO cl_abap_elemdescr
      RAISING   /usi/cx_bal_root.

    CLASS-METHODS get_type_by_name
      IMPORTING i_type_name     TYPE typename
      RETURNING VALUE(r_result) TYPE REF TO cl_abap_elemdescr
      RAISING   /usi/cx_bal_root.

    CLASS-METHODS get_type_by_properties
      IMPORTING i_internal_type TYPE inttype
                i_length        TYPE intlen
                i_decimals      TYPE decimals
      RETURNING VALUE(r_result) TYPE REF TO cl_abap_elemdescr
      RAISING   /usi/cx_bal_root.

ENDCLASS.


CLASS lcl_table_descriptor IMPLEMENTATION.
  METHOD get_by_tabname.
    DATA type_description TYPE REF TO cl_abap_typedescr.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = i_tabname
                                         RECEIVING  p_descr_ref    = type_description
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    IF sy-subrc <> 0.
      ASSERT ID /usi/bal_log_writer
             FIELDS sy-subrc
             CONDITION 1 = 0.

      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_line_type.
    ENDIF.

    r_result = NEW #( i_line_type_description = type_description ).
  ENDMETHOD.

  METHOD get_by_data.
    DATA: exception         TYPE REF TO cx_root,
          exception_text    TYPE string,
          type_description  TYPE REF TO cl_abap_typedescr,
          table_description TYPE REF TO cl_abap_tabledescr.

    TRY.
        type_description   = cl_abap_typedescr=>describe_by_data( i_table ).
        table_description ?= type_description.
        type_description   = table_description->get_table_line_type( ).

      CATCH cx_sy_move_cast_error INTO exception.
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_line_type
                    previous = exception.

    ENDTRY.

    r_result = NEW #( i_line_type_description = type_description ).
  ENDMETHOD.

  METHOD get_by_fieldcatalog.
    DATA: components       TYPE abap_component_tab,
          exception        TYPE REF TO cx_sy_struct_creation,
          exception_text   TYPE string,
          type_description TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS <field> TYPE lvc_s_fcat.

    LOOP AT i_fieldcatalog ASSIGNING <field>.
      INSERT VALUE #( name = <field>-fieldname
                      type = get_type_by_fieldcatalog_line( <field> ) )
             INTO TABLE components.
    ENDLOOP.

    TRY.
        type_description = cl_abap_structdescr=>get( components ).
      CATCH cx_sy_struct_creation INTO exception.
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_line_type
                    previous = exception.
    ENDTRY.

    r_result = NEW #( i_line_type_description = type_description ).
  ENDMETHOD.

  METHOD get_type_by_fieldcatalog_line.
    IF i_fieldcatalog_line-ref_table IS NOT INITIAL.
      r_result = get_type_by_ddic_reference( i_tabname   = i_fieldcatalog_line-ref_table
                                             i_fieldname = COND #( WHEN i_fieldcatalog_line-ref_field IS NOT INITIAL
                                                                   THEN i_fieldcatalog_line-ref_field
                                                                   ELSE i_fieldcatalog_line-fieldname ) ).

    ELSEIF i_fieldcatalog_line-rollname IS NOT INITIAL.
      r_result = get_type_by_name( i_fieldcatalog_line-rollname ).

    ELSE.
      r_result = get_type_by_properties( i_internal_type = i_fieldcatalog_line-inttype
                                         i_length        = i_fieldcatalog_line-intlen
                                         i_decimals      = i_fieldcatalog_line-decimals ).

    ENDIF.
  ENDMETHOD.

  METHOD get_type_by_ddic_reference.
    DATA: exception         TYPE REF TO cx_root,
          exception_text    TYPE string,
          type_description  TYPE REF TO cl_abap_typedescr,
          struc_description TYPE REF TO cl_abap_structdescr.

    TRY.

        cl_abap_structdescr=>describe_by_name( EXPORTING  p_name      = i_tabname
                                               RECEIVING  p_descr_ref = type_description
                                               EXCEPTIONS OTHERS      = 0 ).

        struc_description ?= type_description.
        struc_description->get_component_type( EXPORTING  p_name      = i_fieldname
                                               RECEIVING  p_descr_ref = type_description
                                               EXCEPTIONS OTHERS      = 0 ).

        IF type_description->kind = cl_abap_typedescr=>kind_elem.
          r_result ?= type_description.
        ELSE.
          ASSERT ID /usi/bal_log_writer
                 FIELDS type_description->kind
                 CONDITION 1 = 0.

          RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
            EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_field_type.
        ENDIF.

      CATCH cx_sy_move_cast_error
            cx_sy_ref_is_initial INTO exception.

        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_field_type
                    previous = exception.

    ENDTRY.
  ENDMETHOD.

  METHOD get_type_by_name.
    DATA: exception        TYPE REF TO cx_root,
          exception_text   TYPE string,
          type_description TYPE REF TO cl_abap_typedescr.

    TRY.

        cl_abap_elemdescr=>describe_by_name( EXPORTING  p_name      = i_type_name
                                             RECEIVING  p_descr_ref = type_description
                                             EXCEPTIONS OTHERS      = 0 ).

        IF type_description->kind = cl_abap_typedescr=>kind_elem.
          r_result ?= type_description.
        ELSE.
          ASSERT ID /usi/bal_log_writer
                 FIELDS type_description->kind
                 CONDITION 1 = 0.

          RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
            EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_field_type.
        ENDIF.

      CATCH cx_sy_move_cast_error
            cx_sy_ref_is_initial INTO exception.

        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_field_type
                    previous = exception.

    ENDTRY.
  ENDMETHOD.

  METHOD get_type_by_properties.
    CONSTANTS: BEGIN OF c_special_type_kinds,
                 int8 TYPE abap_typekind VALUE '8',
               END   OF c_special_type_kinds.

    CONSTANTS: BEGIN OF c_special_type_names,
                 int1 TYPE typename VALUE 'INT1',
                 int2 TYPE typename VALUE 'INT2',
                 int8 TYPE typename VALUE 'INT8',
               END   OF c_special_type_names.

    DATA: field_length   TYPE int4,
          decimals       TYPE int4,
          exception      TYPE REF TO cx_parameter_invalid_range,
          exception_text TYPE string.

    field_length = i_length.
    decimals     = i_decimals.

    TRY.
        r_result = SWITCH #( i_internal_type
                             WHEN cl_abap_typedescr=>typekind_char THEN
                               cl_abap_elemdescr=>get_c( field_length )
                             WHEN cl_abap_typedescr=>typekind_date THEN
                               cl_abap_elemdescr=>get_d( )
                             WHEN cl_abap_typedescr=>typekind_decfloat16 THEN
                               cl_abap_elemdescr=>get_decfloat16( )
                             WHEN cl_abap_typedescr=>typekind_decfloat34
                                 OR cl_abap_typedescr=>typekind_decfloat THEN
                               cl_abap_elemdescr=>get_decfloat34( )
                             WHEN cl_abap_typedescr=>typekind_float THEN
                               cl_abap_elemdescr=>get_f( )
                             WHEN cl_abap_typedescr=>typekind_hex THEN
                               cl_abap_elemdescr=>get_x( field_length )
                             WHEN cl_abap_typedescr=>typekind_int THEN
                               cl_abap_elemdescr=>get_i( )
                             WHEN cl_abap_typedescr=>typekind_int1 THEN
                               get_type_by_name( c_special_type_names-int1 )
                             WHEN cl_abap_typedescr=>typekind_int2 THEN
                               get_type_by_name( c_special_type_names-int2 )
                             WHEN c_special_type_kinds-int8 THEN
                               get_type_by_name( c_special_type_names-int8 )
                             WHEN cl_abap_typedescr=>typekind_num THEN
                               cl_abap_elemdescr=>get_n( field_length )
                             WHEN cl_abap_typedescr=>typekind_packed THEN
                               cl_abap_elemdescr=>get_p( p_length   = field_length
                                                         p_decimals = decimals )
                             WHEN cl_abap_typedescr=>typekind_string THEN
                               cl_abap_elemdescr=>get_string( )
                             WHEN cl_abap_typedescr=>typekind_time THEN
                               cl_abap_elemdescr=>get_t( )
                             WHEN cl_abap_typedescr=>typekind_xstring THEN
                               cl_abap_elemdescr=>get_xstring( ) ).

      CATCH cx_parameter_invalid_range INTO exception.
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_field_type
                    previous = exception.

    ENDTRY.
  ENDMETHOD.

  METHOD constructor.
    DATA: exception      TYPE REF TO cx_root,
          exception_text TYPE string.

    type_descriptions-line = get_normalized_line_type_desc( i_line_type_description ).

    TRY.
        type_descriptions-table = cl_abap_tabledescr=>get( p_line_type = type_descriptions-line
                                                           p_key_kind  = cl_abap_tabledescr=>keydefkind_empty ).

      CATCH cx_sy_type_creation INTO exception.
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_line_type
                    previous = exception.

    ENDTRY.
  ENDMETHOD.

  METHOD get_normalized_line_type_desc.
    TRY.
        CASE i_line_type_description->kind.
          WHEN cl_abap_typedescr=>kind_struct.
            r_result = remove_non_elementary_fields( CAST #( i_line_type_description ) ).

          WHEN cl_abap_typedescr=>kind_elem.
            r_result = cl_abap_structdescr=>get( VALUE #( ( name = 'COLUMN_1'
                                                            type = CAST #( i_line_type_description ) ) ) ).

          WHEN cl_abap_typedescr=>kind_ref.
            r_result = get_normalized_line_type_desc(
                           CAST cl_abap_refdescr( i_line_type_description )->get_referenced_type( ) ).

          WHEN OTHERS.
            ASSERT ID /usi/bal_log_writer
                   FIELDS i_line_type_description->kind
                   CONDITION 1 = 0.

            RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
              EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_line_type.
        ENDCASE.

      CATCH cx_sy_ref_is_initial
            cx_sy_type_creation
            cx_sy_move_cast_error INTO DATA(exception).

        DATA(exception_text) = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_line_type
                    previous = exception.
    ENDTRY.
  ENDMETHOD.

  METHOD remove_non_elementary_fields.
    DATA: exception         TYPE REF TO cx_sy_struct_creation,
          exception_text    TYPE string,
          target_components TYPE abap_component_tab.

    target_components = get_elementary_fields( i_line_type_description = i_line_type_description
                                               i_source_structure_path = `` ).

    TRY.
        r_result = cl_abap_structdescr=>get( target_components ).
      CATCH cx_sy_struct_creation INTO exception.
        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_line_type
                    previous = exception.
    ENDTRY.
  ENDMETHOD.

  METHOD get_elementary_fields.
    LOOP AT i_line_type_description->components REFERENCE INTO DATA(source_component).
      DATA(data_description) = i_line_type_description->get_component_type( source_component->name ).

      DATA(source_structure_path) = |{ COND #( WHEN i_source_structure_path IS NOT INITIAL
                                               THEN |{ i_source_structure_path }-| )
                                    }{ source_component->name }|.

      CASE data_description->kind.
        WHEN cl_abap_typedescr=>kind_struct.
          LOOP AT get_elementary_fields( i_line_type_description = CAST #( data_description )
                                         i_source_structure_path = source_structure_path )
               REFERENCE INTO DATA(elementary_field).
            INSERT elementary_field->* INTO TABLE r_result.
          ENDLOOP.

        WHEN cl_abap_typedescr=>kind_elem.
          TRY.
              INSERT VALUE #( name = extend_field_map( i_proposed_fieldname = source_component->name
                                                       i_source_field_path  = source_structure_path )
                              type = data_description ) INTO TABLE r_result.
            CATCH /usi/cx_bal_root INTO DATA(fieldname_generation_error).
              ASSERT ID /usi/bal_log_writer
                     FIELDS fieldname_generation_error->get_text( )
                     CONDITION 1 = 0.
              CONTINUE.
          ENDTRY.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD extend_field_map.
    CONSTANTS: c_regex_not_found      TYPE i VALUE -1,
               c_length_of_separator  TYPE i VALUE 1,
               c_max_length_fieldname TYPE i VALUE 30.

    DATA: numeric_suffix     TYPE string,
          proposed_fieldname TYPE fieldname.

    IF buffers-field_mapping IS NOT BOUND.
      buffers-field_mapping = NEW #( ).
    ENDIF.

    INSERT VALUE #( target_field_name = i_proposed_fieldname
                    source_field_path = i_source_field_path )
           INTO TABLE buffers-field_mapping->*.
    IF sy-subrc = 0.
      r_result = i_proposed_fieldname.
      RETURN.
    ENDIF.

    DATA(separator_offset) = find( val   = i_proposed_fieldname
                                   regex = '_(\d+) *$'
                                   case  = abap_false ).
    IF separator_offset = c_regex_not_found.
      proposed_fieldname = |{ i_proposed_fieldname+0(28) }_1|.
    ELSE.
      DATA(number_offset) = separator_offset + c_length_of_separator.
      numeric_suffix = i_proposed_fieldname+number_offset.
      numeric_suffix = |{ numeric_suffix + 1 }|.

      "  handling for additional digits (9 -> 10)
      IF number_offset + strlen( numeric_suffix ) > c_max_length_fieldname.
        number_offset    = c_max_length_fieldname - strlen( numeric_suffix ).
        separator_offset = number_offset - 1.

        IF separator_offset < 0.
          " Fatal overflow - fieldname would start with digit, which is forbidden in ABAP!
          RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
            EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_duplicate_field
                      param1 = CONV #( i_proposed_fieldname ).
        ENDIF.
      ENDIF.

      proposed_fieldname = |{ i_proposed_fieldname+0(separator_offset) }_{ numeric_suffix }|.
    ENDIF.

    r_result = extend_field_map( i_proposed_fieldname = proposed_fieldname
                                 i_source_field_path  = i_source_field_path ).
  ENDMETHOD.

  METHOD get_line_type_dref.
    CREATE DATA r_result TYPE HANDLE type_descriptions-line.
  ENDMETHOD.

  METHOD get_table_type_dref.
    CREATE DATA r_result TYPE HANDLE type_descriptions-table.
  ENDMETHOD.

  METHOD get_fieldcatalog.
    DATA: tabname           TYPE tabname,
          data_description  TYPE REF TO cl_abap_datadescr,
          elem_description  TYPE REF TO cl_abap_elemdescr,
          field_description TYPE lvc_s_fcat,
          ddic_description  TYPE dfies.

    IF buffers-fieldcatalog IS NOT BOUND.
      CREATE DATA buffers-fieldcatalog.

      IF type_descriptions-line->is_ddic_type( ) = abap_true.
        tabname = type_descriptions-line->get_relative_name( ).
        CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
          EXPORTING  i_structure_name = tabname
          CHANGING   ct_fieldcat      = buffers-fieldcatalog->*
          EXCEPTIONS OTHERS           = 0.

        LOOP AT buffers-fieldcatalog->* REFERENCE INTO DATA(field).
          CLEAR field->no_out.
          CLEAR field->tech.
        ENDLOOP.

      ELSE.
        LOOP AT type_descriptions-line->components ASSIGNING FIELD-SYMBOL(<component>).
          type_descriptions-line->get_component_type( EXPORTING  p_name      = <component>-name
                                                      RECEIVING  p_descr_ref = data_description
                                                      EXCEPTIONS OTHERS      = 0 ).
          TRY.
              elem_description ?= data_description.
            CATCH cx_sy_move_cast_error.
              CONTINUE.
          ENDTRY.

          CLEAR field_description.
          field_description-fieldname = <component>-name.
          field_description-inttype   = <component>-type_kind.
          field_description-decimals  = <component>-decimals.
          field_description-outputlen = elem_description->output_length.

          IF elem_description->is_ddic_type( ) = abap_true.
            elem_description->get_ddic_field( RECEIVING  p_flddescr = ddic_description
                                              EXCEPTIONS OTHERS     = 0 ).

            field_description-intlen     = ddic_description-leng.
            field_description-domname    = ddic_description-domname.
            field_description-rollname   = ddic_description-rollname.
            field_description-datatype   = ddic_description-datatype.
            field_description-lowercase  = ddic_description-lowercase.

            field_description-checktable = ddic_description-checktable.
            field_description-convexit   = ddic_description-convexit.
            field_description-valexi     = ddic_description-valexi.
            field_description-f4availabl = ddic_description-f4availabl.

            field_description-scrtext_s  = ddic_description-scrtext_s.
            field_description-scrtext_m  = ddic_description-scrtext_m.
            field_description-scrtext_l  = ddic_description-scrtext_l.
            field_description-reptext    = ddic_description-reptext.
            field_description-seltext    = ddic_description-scrtext_l.

          ELSE.
            field_description-coltext = <component>-name.
            field_description-seltext = <component>-name.
            IF    <component>-type_kind = cl_abap_typedescr=>typekind_char
               OR <component>-type_kind = cl_abap_typedescr=>typekind_hex
               OR <component>-type_kind = cl_abap_typedescr=>typekind_num
               OR <component>-type_kind = cl_abap_typedescr=>typekind_packed.
              field_description-intlen = <component>-length.
            ENDIF.

          ENDIF.

          INSERT field_description INTO TABLE buffers-fieldcatalog->*.
        ENDLOOP.
      ENDIF.
    ENDIF.

    r_result = buffers-fieldcatalog->*.
  ENDMETHOD.

  METHOD get_tabname.
    IF type_descriptions-line->is_ddic_type( ) = abap_true.
      r_result = type_descriptions-line->get_relative_name( ).
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>generic_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD get_field_mapping.
    IF buffers-field_mapping IS NOT BOUND.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>generic_not_found.
    ENDIF.
    r_result = buffers-field_mapping->*.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_table_content_copier DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_source_table_ref TYPE REF TO data
                i_target_table_ref TYPE REF TO data
                i_field_mapping    TYPE ty_field_mapping OPTIONAL
      RAISING   /usi/cx_bal_root.

    METHODS copy_table_contents
      RAISING /usi/cx_bal_root.

  PRIVATE SECTION.
    DATA: BEGIN OF source_table,
            content    TYPE REF TO data,
            table_type TYPE REF TO cl_abap_tabledescr,
            line_type  TYPE REF TO cl_abap_datadescr,
          END   OF source_table.

    DATA field_mapping TYPE ty_field_mapping.

    DATA: BEGIN OF target_table,
            content    TYPE REF TO data,
            table_type TYPE REF TO cl_abap_tabledescr,
            line_type  TYPE REF TO cl_abap_structdescr,
          END   OF target_table.

    METHODS get_table_type_description
      IMPORTING i_table_ref     TYPE REF TO data
      RETURNING VALUE(r_result) TYPE REF TO cl_abap_tabledescr
      RAISING   /usi/cx_bal_root.

    METHODS copy_from_elementary_line_type
      RAISING /usi/cx_bal_root.

    METHODS copy_from_referenced_line_type
      RAISING /usi/cx_bal_root.

    METHODS copy_from_structured_line_type
      RAISING /usi/cx_bal_root.

    METHODS copy_field_values
      IMPORTING i_source_line_ref TYPE REF TO data
                i_target_line_ref TYPE REF TO data.

ENDCLASS.


CLASS lcl_table_content_copier IMPLEMENTATION.
  METHOD constructor.
    source_table-content    = i_source_table_ref.
    source_table-table_type = get_table_type_description( source_table-content ).
    source_table-line_type  = source_table-table_type->get_table_line_type( ).

    field_mapping           = i_field_mapping.

    target_table-content    = i_target_table_ref.
    target_table-table_type = get_table_type_description( target_table-content ).
    TRY.
        target_table-line_type ?= target_table-table_type->get_table_line_type( ).
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_line_type.
    ENDTRY.
  ENDMETHOD.

  METHOD get_table_type_description.
    DATA type_description TYPE REF TO cl_abap_typedescr.

    cl_abap_typedescr=>describe_by_data_ref( EXPORTING  p_data_ref           = i_table_ref
                                             RECEIVING  p_descr_ref          = type_description
                                             EXCEPTIONS reference_is_initial = 1
                                                        OTHERS               = 2 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING textid = /usi/cx_bal_invalid_input=>itab_required.
    ENDIF.

    TRY.
        r_result ?= type_description.
      CATCH cx_sy_move_cast_error.
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid = /usi/cx_bal_invalid_input=>itab_required.
    ENDTRY.
  ENDMETHOD.

  METHOD copy_table_contents.
    CASE source_table-line_type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        copy_from_elementary_line_type( ).

      WHEN cl_abap_typedescr=>kind_struct.
        copy_from_structured_line_type( ).

      WHEN cl_abap_typedescr=>kind_ref.
        copy_from_referenced_line_type( ).

      WHEN OTHERS.
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_line_type.

    ENDCASE.
  ENDMETHOD.

  METHOD copy_from_elementary_line_type.
    DATA target_line TYPE REF TO data.

    FIELD-SYMBOLS: <source_table> TYPE ANY TABLE,
                   <source_value> TYPE any,
                   <target_table> TYPE STANDARD TABLE,
                   <target_line>  TYPE any,
                   <target_field> TYPE any.

    ASSIGN source_table-content->* TO <source_table>.
    ASSIGN target_table-content->* TO <target_table>.

    CREATE DATA target_line TYPE HANDLE target_table-line_type.
    ASSIGN target_line->* TO <target_line>.
    ASSIGN COMPONENT 1 OF STRUCTURE <target_line> TO <target_field>.

    LOOP AT <source_table> ASSIGNING <source_value>.
      <target_field> = <source_value>.
      INSERT <target_line> INTO TABLE <target_table>.
    ENDLOOP.
  ENDMETHOD.

  METHOD copy_from_referenced_line_type.
    FIELD-SYMBOLS: <source_table> TYPE ANY TABLE,
                   <source_line>  TYPE any,
                   <source_value> TYPE any,
                   <target_table> TYPE STANDARD TABLE,
                   <target_line>  TYPE any,
                   <target_field> TYPE any.

    DATA target_line TYPE REF TO data.

    ASSIGN source_table-content->* TO <source_table>.
    ASSIGN target_table-content->* TO <target_table>.

    DATA(referenced_type) = CAST cl_abap_refdescr( source_table-line_type )->get_referenced_type( ).
    CASE referenced_type->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        CREATE DATA target_line TYPE HANDLE target_table-line_type.
        ASSIGN target_line->* TO <target_line>.
        ASSIGN COMPONENT 1 OF STRUCTURE <target_line> TO <target_field>.

        LOOP AT <source_table> ASSIGNING <source_line>.
          ASSIGN <source_line>->* TO <source_value>.
          <target_field> = <source_value>.
          INSERT <target_line> INTO TABLE <target_table>.
        ENDLOOP.

      WHEN cl_abap_typedescr=>kind_struct.
        LOOP AT <source_table> ASSIGNING <source_line>.
          CREATE DATA target_line TYPE HANDLE target_table-line_type.

          copy_field_values( i_source_line_ref = <source_line>
                             i_target_line_ref = target_line ).

          ASSIGN target_line->* TO <target_line>.
          INSERT <target_line> INTO TABLE <target_table>.
        ENDLOOP.

      WHEN OTHERS.
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid = /usi/cx_bal_invalid_input=>unsupported_line_type.

    ENDCASE.
  ENDMETHOD.

  METHOD copy_from_structured_line_type.
    DATA: source_line TYPE REF TO data,
          target_line TYPE REF TO data.

    FIELD-SYMBOLS: <source_table> TYPE ANY TABLE,
                   <target_table> TYPE STANDARD TABLE,
                   <target_line>  TYPE any.

    ASSIGN source_table-content->* TO <source_table>.
    ASSIGN target_table-content->* TO <target_table>.

    CREATE DATA target_line TYPE HANDLE target_table-line_type.
    IF source_table-line_type->applies_to_data_ref( target_line ) = abap_true.
      <target_table> = <source_table>.

    ELSE.
      LOOP AT <source_table> REFERENCE INTO source_line.
        CREATE DATA target_line TYPE HANDLE target_table-line_type.

        copy_field_values( i_source_line_ref = source_line
                           i_target_line_ref = target_line ).

        ASSIGN target_line->* TO <target_line>.
        INSERT <target_line> INTO TABLE <target_table>.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD copy_field_values.
    ASSIGN i_source_line_ref->* TO FIELD-SYMBOL(<source_line>).
    ASSIGN i_target_line_ref->* TO FIELD-SYMBOL(<target_line>).

    LOOP AT field_mapping REFERENCE INTO DATA(field_map_line).
      DATA(source_field_absolute_name) = |<SOURCE_LINE>-{ field_map_line->source_field_path }|.
      ASSIGN (source_field_absolute_name) TO FIELD-SYMBOL(<source_field>).
      IF sy-subrc <> 0.
        ASSERT ID /usi/bal_log_writer
               FIELDS source_field_absolute_name
               CONDITION 1 = 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT field_map_line->target_field_name OF STRUCTURE <target_line> TO FIELD-SYMBOL(<target_field>).
      IF sy-subrc <> 0.
        ASSERT ID /usi/bal_log_writer
               FIELDS field_map_line->target_field_name
               CONDITION 1 = 0.
        CONTINUE.
      ENDIF.

      <target_field> = <source_field>.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_persistency_flavor_enum DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES ty_persistency_flavor TYPE n LENGTH 2.

    CLASS-DATA: tabname_xml       TYPE REF TO lcl_persistency_flavor_enum READ-ONLY,
                fieldcatalog_xml  TYPE REF TO lcl_persistency_flavor_enum READ-ONLY,
                tabname_json      TYPE REF TO lcl_persistency_flavor_enum READ-ONLY,
                fieldcatalog_json TYPE REF TO lcl_persistency_flavor_enum READ-ONLY.

    DATA value TYPE ty_persistency_flavor READ-ONLY.

    CLASS-METHODS class_constructor.

    CLASS-METHODS get_by_value
      IMPORTING i_value         TYPE ty_persistency_flavor
      RETURNING VALUE(r_result) TYPE REF TO lcl_persistency_flavor_enum
      RAISING   /usi/cx_bal_root.

    METHODS constructor
      IMPORTING i_value TYPE ty_persistency_flavor.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_instance,
             value    TYPE ty_persistency_flavor,
             instance TYPE REF TO lcl_persistency_flavor_enum,
           END   OF ty_instance,
           ty_instances TYPE HASHED TABLE OF ty_instance WITH UNIQUE KEY value.

    CLASS-DATA instances TYPE ty_instances.

    CLASS-METHODS buffer_instance
      IMPORTING i_value    TYPE ty_persistency_flavor
                i_instance TYPE REF TO lcl_persistency_flavor_enum.

ENDCLASS.


CLASS lcl_persistency_flavor_enum IMPLEMENTATION.
  METHOD class_constructor.
    tabname_xml = NEW #( i_value = 1 ).

    fieldcatalog_xml = NEW #( i_value = 2 ).

    tabname_json = NEW #( i_value = 3 ).

    fieldcatalog_json = NEW #( i_value = 4 ).

    " For the sake of backwards compatibility
    "   (Previous versions used flavor 1, but since they
    "    did not add that information to the XML output,
    "    the deserializer will read '00' from their XML)
    buffer_instance( i_value    = 0
                     i_instance = tabname_xml ).
  ENDMETHOD.

  METHOD constructor.
    value = i_value.

    buffer_instance( i_value    = i_value
                     i_instance = me ).
  ENDMETHOD.

  METHOD buffer_instance.
    INSERT VALUE #( value    = i_value
                    instance = i_instance )
           INTO TABLE instances.
  ENDMETHOD.

  METHOD get_by_value.
    TRY.
        r_result = instances[ value = i_value ]-instance.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


INTERFACE lif_persistency_constants.
  TYPES ty_parameter_name TYPE string.

  CONSTANTS: BEGIN OF c_parameter_names,
               persistency_flavor    TYPE ty_parameter_name VALUE 'PERSISTENCY_FLAVOR',
               table_name            TYPE ty_parameter_name VALUE 'TABLE_NAME',
               internal_fieldcatalog TYPE ty_parameter_name VALUE 'INTERNAL_FIELDCATALOG',
               internal_table_json   TYPE ty_parameter_name VALUE 'INTERNAL_TABLE_JSON',
               internal_table_xml    TYPE ty_parameter_name VALUE 'INTERNAL_TABLE_XML',
               internal_table        TYPE ty_parameter_name VALUE 'INTERNAL_TABLE',
               external_fieldcatalog TYPE ty_parameter_name VALUE 'EXTERNAL_FIELDCATALOG',
               title_classname       TYPE ty_parameter_name VALUE 'TITLE_CLASSNAME',
               serialized_title      TYPE ty_parameter_name VALUE 'SERIALIZED_TITLE',
             END   OF c_parameter_names.

ENDINTERFACE.


INTERFACE lif_serializer.
  INTERFACES lif_persistency_constants.

  METHODS serialize
    RETURNING VALUE(r_result) TYPE /usi/bal_serialized_data
    RAISING   /usi/cx_bal_root.

ENDINTERFACE.


INTERFACE lif_deserializer.
  INTERFACES lif_persistency_constants.

  METHODS get_internal_table
    RETURNING VALUE(r_result) TYPE REF TO data.

  METHODS get_external_fieldcatalog
    RETURNING VALUE(r_result) TYPE lvc_t_fcat
    RAISING   /usi/cx_bal_root.

  METHODS get_title
    RETURNING VALUE(r_result) TYPE REF TO /usi/if_bal_text_container_c40
    RAISING   /usi/cx_bal_root.

ENDINTERFACE.


CLASS lcl_serializer DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_serializer.

    METHODS constructor
      IMPORTING i_table_reference       TYPE REF TO data
                i_table_descriptor      TYPE REF TO lcl_table_descriptor
                i_external_fieldcatalog TYPE lvc_t_fcat                            OPTIONAL
                i_title                 TYPE REF TO /usi/if_bal_text_container_c40 OPTIONAL.

  PRIVATE SECTION.
    DATA: BEGIN OF input,
            table_reference       TYPE REF TO data,
            table_descriptor      TYPE REF TO lcl_table_descriptor,
            external_fieldcatalog TYPE lvc_t_fcat,
            title                 TYPE REF TO /usi/if_bal_text_container_c40,
          END   OF input,
          persistency_flavor TYPE REF TO lcl_persistency_flavor_enum,
          serializer         TYPE REF TO /usi/cl_bal_serializer.

    METHODS get_persistency_flavor
      RETURNING VALUE(r_result) TYPE REF TO lcl_persistency_flavor_enum.

    METHODS serialize_persistency_flavor
      RETURNING VALUE(r_result) TYPE abap_trans_srcbind_tab.

    METHODS serialize_table_descriptor
      RETURNING VALUE(r_result) TYPE abap_trans_srcbind_tab
      RAISING   /usi/cx_bal_root.

    METHODS serialize_table_data
      RETURNING VALUE(r_result) TYPE abap_trans_srcbind_tab
      RAISING   /usi/cx_bal_root.

    METHODS serialize_external_fieldcat
      RETURNING VALUE(r_result) TYPE abap_trans_srcbind_tab
      RAISING   /usi/cx_bal_root.

    METHODS serialize_title
      RETURNING VALUE(r_result) TYPE abap_trans_srcbind_tab.

ENDCLASS.


CLASS lcl_serializer IMPLEMENTATION.
  METHOD constructor.
    input-table_reference       = i_table_reference.
    input-table_descriptor      = i_table_descriptor.
    input-external_fieldcatalog = i_external_fieldcatalog.
    input-title                 = i_title.

    persistency_flavor = get_persistency_flavor( ).

    serializer = NEW #( ).
  ENDMETHOD.

  METHOD get_persistency_flavor.
    TRY.
        input-table_descriptor->get_tabname( ).
        r_result = lcl_persistency_flavor_enum=>tabname_json.
      CATCH /usi/cx_bal_root.
        r_result = lcl_persistency_flavor_enum=>fieldcatalog_json.
    ENDTRY.
  ENDMETHOD.

  METHOD lif_serializer~serialize.
    DATA: parameters TYPE abap_trans_srcbind_tab,
          parameter  TYPE REF TO abap_trans_srcbind.

    LOOP AT serialize_persistency_flavor( ) REFERENCE INTO parameter.
      INSERT parameter->* INTO TABLE parameters.
    ENDLOOP.

    LOOP AT serialize_table_descriptor( ) REFERENCE INTO parameter.
      INSERT parameter->* INTO TABLE parameters.
    ENDLOOP.

    LOOP AT serialize_table_data( ) REFERENCE INTO parameter.
      INSERT parameter->* INTO TABLE parameters.
    ENDLOOP.

    LOOP AT serialize_external_fieldcat( ) REFERENCE INTO parameter.
      INSERT parameter->* INTO TABLE parameters.
    ENDLOOP.

    LOOP AT serialize_title( ) REFERENCE INTO parameter.
      INSERT parameter->* INTO TABLE parameters.
    ENDLOOP.

    r_result = serializer->serialize_fields_as_xml( parameters ).
  ENDMETHOD.

  METHOD serialize_persistency_flavor.
    r_result =
        VALUE #( ( name  = lif_persistency_constants~c_parameter_names-persistency_flavor
                   value = NEW lcl_persistency_flavor_enum=>ty_persistency_flavor( persistency_flavor->value ) ) ).
  ENDMETHOD.

  METHOD serialize_table_descriptor.
    DATA serialized_fieldcatalog TYPE /usi/bal_json_string.

    CASE persistency_flavor.
      WHEN lcl_persistency_flavor_enum=>tabname_json.
        r_result = VALUE #( ( name  = lif_persistency_constants~c_parameter_names-table_name
                              value = NEW tabname( input-table_descriptor->get_tabname( ) ) ) ).

      WHEN lcl_persistency_flavor_enum=>fieldcatalog_json.
        serialized_fieldcatalog = serializer->serialize_field_as_json(
                                      i_data = input-table_descriptor->get_fieldcatalog( )
                                      i_name = lif_persistency_constants~c_parameter_names-internal_fieldcatalog ).

        r_result = VALUE #( ( name  = lif_persistency_constants~c_parameter_names-internal_fieldcatalog
                              value = NEW /usi/bal_json_string( serialized_fieldcatalog ) ) ).

    ENDCASE.
  ENDMETHOD.

  METHOD serialize_table_data.
    DATA: json_converter TYPE REF TO /usi/cl_bal_serializer,
          parameter      TYPE abap_trans_srcbind.

    FIELD-SYMBOLS: <internal_table_json> TYPE /usi/bal_xml_string,
                   <internal_table_data> TYPE STANDARD TABLE.

    parameter-name = lif_persistency_constants~c_parameter_names-internal_table_json.
    CREATE DATA parameter-value TYPE /usi/bal_json_string.
    ASSIGN parameter-value->* TO <internal_table_json>.

    json_converter = NEW #( ).
    ASSIGN input-table_reference->* TO <internal_table_data>.
    <internal_table_json> = json_converter->serialize_field_as_json(
                                i_name = lif_persistency_constants~c_parameter_names-internal_table
                                i_data = <internal_table_data> ).

    INSERT parameter INTO TABLE r_result.
  ENDMETHOD.

  METHOD serialize_external_fieldcat.
    DATA: json_converter TYPE REF TO /usi/cl_bal_serializer,
          result_line    TYPE abap_trans_srcbind.

    FIELD-SYMBOLS <fieldcatalog> TYPE /usi/bal_json_string.

    IF input-external_fieldcatalog IS INITIAL.
      RETURN.
    ENDIF.

    result_line-name = lif_persistency_constants~c_parameter_names-external_fieldcatalog.
    CREATE DATA result_line-value TYPE /usi/bal_json_string.
    ASSIGN result_line-value->* TO <fieldcatalog>.

    json_converter = NEW #( ).
    <fieldcatalog> = json_converter->serialize_field_as_json(
                         i_name = lif_persistency_constants~c_parameter_names-external_fieldcatalog
                         i_data = input-external_fieldcatalog ).

    INSERT result_line INTO TABLE r_result.
  ENDMETHOD.

  METHOD serialize_title.
    IF input-title IS NOT BOUND.
      RETURN.
    ENDIF.
    r_result = VALUE #( ( name  = lif_persistency_constants~c_parameter_names-title_classname
                          value = NEW /usi/bal_text_cont_classname( input-title->get_classname( ) ) )
                        ( name  = lif_persistency_constants~c_parameter_names-serialized_title
                          value = NEW /usi/bal_xml_string( input-title->serialize( ) ) ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_deserializer DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_deserializer.

    METHODS constructor
      IMPORTING i_serialized_data_container TYPE /usi/bal_serialized_data
      RAISING   /usi/cx_bal_root.

  PRIVATE SECTION.
    DATA: BEGIN OF deserialized_data,
            persistency_flavor    TYPE REF TO lcl_persistency_flavor_enum,
            table_descriptor      TYPE REF TO lcl_table_descriptor,
            table_data            TYPE REF TO data,
            external_fieldcatalog TYPE lvc_t_fcat,
            title                 TYPE REF TO /usi/if_bal_text_container_c40,
          END   OF deserialized_data,
          deserializer TYPE REF TO /usi/cl_bal_serializer.

    METHODS get_persistency_flavor
      IMPORTING i_serialized_data_container TYPE /usi/bal_serialized_data
      RETURNING VALUE(r_result)             TYPE REF TO lcl_persistency_flavor_enum
      RAISING   /usi/cx_bal_root.

    METHODS get_table_descriptor
      IMPORTING i_persistency_flavor        TYPE REF TO lcl_persistency_flavor_enum
                i_serialized_data_container TYPE /usi/bal_serialized_data
      RETURNING VALUE(r_result)             TYPE REF TO lcl_table_descriptor
      RAISING   /usi/cx_bal_root.

    METHODS get_table_data
      IMPORTING i_persistency_flavor        TYPE REF TO lcl_persistency_flavor_enum
                i_serialized_data_container TYPE /usi/bal_serialized_data
                i_table_descriptor          TYPE REF TO lcl_table_descriptor
      RETURNING VALUE(r_result)             TYPE REF TO data
      RAISING   /usi/cx_bal_root.

    METHODS get_external_fieldcatalog
      IMPORTING i_persistency_flavor        TYPE REF TO lcl_persistency_flavor_enum
                i_serialized_data_container TYPE /usi/bal_serialized_data
      RETURNING VALUE(r_result)             TYPE lvc_t_fcat
      RAISING   /usi/cx_bal_root.

    METHODS get_title
      IMPORTING i_serialized_data_container TYPE /usi/bal_serialized_data
      RETURNING VALUE(r_result)             TYPE REF TO /usi/if_bal_text_container_c40
      RAISING   /usi/cx_bal_root.

ENDCLASS.


CLASS lcl_deserializer IMPLEMENTATION.
  METHOD constructor.
    deserializer = NEW #( ).

    deserialized_data-persistency_flavor = get_persistency_flavor( i_serialized_data_container ).
    deserialized_data-table_descriptor   = get_table_descriptor(
                                               i_persistency_flavor        = deserialized_data-persistency_flavor
                                               i_serialized_data_container = i_serialized_data_container ).
    deserialized_data-table_data         = get_table_data(
                                               i_persistency_flavor        = deserialized_data-persistency_flavor
                                               i_serialized_data_container = i_serialized_data_container
                                               i_table_descriptor          = deserialized_data-table_descriptor ).

    TRY.
        deserialized_data-external_fieldcatalog = get_external_fieldcatalog(
                                                      i_persistency_flavor        = deserialized_data-persistency_flavor
                                                      i_serialized_data_container = i_serialized_data_container ).
      CATCH /usi/cx_bal_not_found.
        CLEAR deserialized_data-external_fieldcatalog.
    ENDTRY.

    TRY.
        deserialized_data-title = get_title( i_serialized_data_container ).
      CATCH /usi/cx_bal_not_found.
        CLEAR deserialized_data-title.
    ENDTRY.
  ENDMETHOD.

  METHOD get_persistency_flavor.
    DATA persistency_flavor TYPE lcl_persistency_flavor_enum=>ty_persistency_flavor.

    deserializer->deserialize_field(
      EXPORTING i_serialized_data = i_serialized_data_container
                i_name            = lif_persistency_constants~c_parameter_names-persistency_flavor
      CHANGING  c_data            = persistency_flavor ).

    r_result = lcl_persistency_flavor_enum=>get_by_value( persistency_flavor ).
  ENDMETHOD.

  METHOD get_table_descriptor.
    DATA: fieldcat    TYPE lvc_t_fcat,
          json_string TYPE /usi/bal_json_string,
          tabname     TYPE tabname.

    CASE i_persistency_flavor.
      WHEN lcl_persistency_flavor_enum=>tabname_xml
          OR lcl_persistency_flavor_enum=>tabname_json.

        deserializer->deserialize_field(
          EXPORTING i_serialized_data = i_serialized_data_container
                    i_name            = lif_persistency_constants~c_parameter_names-table_name
          CHANGING  c_data            = tabname ).

        r_result = lcl_table_descriptor=>get_by_tabname( tabname ).

      WHEN lcl_persistency_flavor_enum=>fieldcatalog_xml.

        deserializer->deserialize_field(
          EXPORTING i_serialized_data = i_serialized_data_container
                    i_name            = lif_persistency_constants~c_parameter_names-internal_fieldcatalog
          CHANGING  c_data            = fieldcat ).

        r_result = lcl_table_descriptor=>get_by_fieldcatalog( fieldcat ).

      WHEN lcl_persistency_flavor_enum=>fieldcatalog_json.

        deserializer->deserialize_field(
          EXPORTING i_serialized_data = i_serialized_data_container
                    i_name            = lif_persistency_constants~c_parameter_names-internal_fieldcatalog
          CHANGING  c_data            = json_string ).

        deserializer->deserialize_field(
          EXPORTING i_serialized_data = json_string
                    i_name            = lif_persistency_constants~c_parameter_names-internal_fieldcatalog
          CHANGING  c_data            = fieldcat ).

        r_result = lcl_table_descriptor=>get_by_fieldcatalog( fieldcat ).

    ENDCASE.
  ENDMETHOD.

  METHOD get_table_data.
    DATA serialized_table_data TYPE /usi/bal_serialized_data.

    FIELD-SYMBOLS <internal_table_data> TYPE STANDARD TABLE.

    deserializer->deserialize_field(
      EXPORTING i_serialized_data = i_serialized_data_container
                i_name            = COND #( WHEN i_persistency_flavor = lcl_persistency_flavor_enum=>tabname_xml
                                              OR i_persistency_flavor = lcl_persistency_flavor_enum=>fieldcatalog_xml
                                            THEN lif_persistency_constants~c_parameter_names-internal_table_xml
                                            ELSE lif_persistency_constants~c_parameter_names-internal_table_json )
      CHANGING  c_data            = serialized_table_data ).

    r_result = i_table_descriptor->get_table_type_dref( ).
    ASSIGN r_result->* TO <internal_table_data>.

    deserializer->deserialize_field(
      EXPORTING i_name            = lif_persistency_constants~c_parameter_names-internal_table
                i_serialized_data = serialized_table_data
      CHANGING  c_data            = <internal_table_data> ).
  ENDMETHOD.

  METHOD get_external_fieldcatalog.
    DATA serialized_fieldcatalog TYPE /usi/bal_serialized_data.

    CASE i_persistency_flavor.
      WHEN lcl_persistency_flavor_enum=>fieldcatalog_xml
          OR lcl_persistency_flavor_enum=>tabname_xml.

        deserializer->deserialize_field(
          EXPORTING i_serialized_data = i_serialized_data_container
                    i_name            = lif_persistency_constants~c_parameter_names-external_fieldcatalog
          CHANGING  c_data            = r_result ).
        RETURN.

      WHEN lcl_persistency_flavor_enum=>fieldcatalog_json
          OR lcl_persistency_flavor_enum=>tabname_json.

        deserializer->deserialize_field(
          EXPORTING i_serialized_data = i_serialized_data_container
                    i_name            = lif_persistency_constants~c_parameter_names-external_fieldcatalog
          CHANGING  c_data            = serialized_fieldcatalog ).

        IF serialized_fieldcatalog IS NOT INITIAL.
          deserializer->deserialize_field(
            EXPORTING i_name            = lif_persistency_constants~c_parameter_names-external_fieldcatalog
                      i_serialized_data = serialized_fieldcatalog
            CHANGING  c_data            = r_result ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD get_title.
    DATA: exception        TYPE REF TO cx_root,
          exception_text   TYPE string,
          classname        TYPE /usi/bal_text_cont_classname,
          serialized_title TYPE /usi/bal_xml_string.

    deserializer->deserialize_fields(
        i_serialized_data = i_serialized_data_container
        i_parameters      = VALUE #( ( name  = lif_persistency_constants~c_parameter_names-title_classname
                                       value = REF #( classname ) )
                                     ( name  = lif_persistency_constants~c_parameter_names-serialized_title
                                       value = REF #( serialized_title ) ) ) ).

    IF classname IS INITIAL.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>generic_not_found.
    ENDIF.

    TRY.
        CALL METHOD (classname)=>/usi/if_bal_text_container_c40~deserialize
          EXPORTING i_serialized_text_container = serialized_title
          RECEIVING r_result                    = r_result.

      CATCH cx_sy_dyn_call_error
            /usi/cx_bal_root INTO exception.

        exception_text = exception->get_text( ).
        ASSERT ID /usi/bal_log_writer
               FIELDS exception_text
               CONDITION 1 = 0.

        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>/usi/cx_bal_invalid_input
                    previous = exception.
    ENDTRY.
  ENDMETHOD.

  METHOD lif_deserializer~get_external_fieldcatalog.
    IF deserialized_data-external_fieldcatalog IS NOT INITIAL.
      r_result = deserialized_data-external_fieldcatalog.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>generic_not_found.
    ENDIF.
  ENDMETHOD.

  METHOD lif_deserializer~get_internal_table.
    r_result = deserialized_data-table_data.
  ENDMETHOD.

  METHOD lif_deserializer~get_title.
    IF deserialized_data-title IS BOUND.
      r_result = deserialized_data-title.
    ELSE.
      RAISE EXCEPTION TYPE /usi/cx_bal_not_found
        EXPORTING textid = /usi/cx_bal_not_found=>generic_not_found.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_fieldcatalog_name_enum DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES ty_fieldcatalog_name TYPE c LENGTH 10.

    CLASS-DATA: external  TYPE REF TO lcl_fieldcatalog_name_enum READ-ONLY,
                internal  TYPE REF TO lcl_fieldcatalog_name_enum READ-ONLY,
                technical TYPE REF TO lcl_fieldcatalog_name_enum READ-ONLY.

    DATA value TYPE ty_fieldcatalog_name READ-ONLY.

    CLASS-METHODS class_constructor.

    METHODS constructor
      IMPORTING i_fieldcatalog_name TYPE ty_fieldcatalog_name.

ENDCLASS.


CLASS lcl_fieldcatalog_name_enum IMPLEMENTATION.
  METHOD class_constructor.
    external = NEW #( i_fieldcatalog_name = 'EXTERNAL' ).

    internal = NEW #( i_fieldcatalog_name = 'INTERNAL' ).

    technical = NEW #( i_fieldcatalog_name = 'TECHNICAL' ).
  ENDMETHOD.

  METHOD constructor.
    value = i_fieldcatalog_name.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_fieldcatalog_collection DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_table_descriptor      TYPE REF TO lcl_table_descriptor
                i_external_fieldcatalog TYPE lvc_t_fcat OPTIONAL.

    METHODS get_fieldcatalog
      IMPORTING i_fieldcatalog_name TYPE REF TO lcl_fieldcatalog_name_enum
      RETURNING VALUE(r_result)     TYPE lvc_t_fcat.

    METHODS has_fieldcatalog
      IMPORTING i_fieldcatalog_name TYPE REF TO lcl_fieldcatalog_name_enum
      RETURNING VALUE(r_result)     TYPE abap_bool.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_fieldcatalog,
             name         TYPE lcl_fieldcatalog_name_enum=>ty_fieldcatalog_name,
             fieldcatalog TYPE lvc_t_fcat,
           END   OF ty_fieldcatalog,
           ty_fieldcatalogs TYPE HASHED TABLE OF ty_fieldcatalog WITH UNIQUE KEY name.

    DATA fieldcatalogs TYPE ty_fieldcatalogs.

    METHODS insert_fieldcatalog
      IMPORTING i_fieldcatalog_name TYPE REF TO lcl_fieldcatalog_name_enum
                i_fieldcatalog      TYPE lvc_t_fcat.

    METHODS merge_technical_fieldcatalog
      IMPORTING i_table_descriptor TYPE REF TO lcl_table_descriptor
      RETURNING VALUE(r_result)    TYPE lvc_t_fcat.

ENDCLASS.


CLASS lcl_fieldcatalog_collection IMPLEMENTATION.
  METHOD constructor.
    insert_fieldcatalog( i_fieldcatalog_name = lcl_fieldcatalog_name_enum=>internal
                         i_fieldcatalog      = i_table_descriptor->get_fieldcatalog( ) ).

    insert_fieldcatalog( i_fieldcatalog_name = lcl_fieldcatalog_name_enum=>technical
                         i_fieldcatalog      = merge_technical_fieldcatalog( i_table_descriptor ) ).

    IF i_external_fieldcatalog IS NOT INITIAL.
      insert_fieldcatalog( i_fieldcatalog_name = lcl_fieldcatalog_name_enum=>external
                           i_fieldcatalog      = i_external_fieldcatalog ).
    ENDIF.
  ENDMETHOD.

  METHOD insert_fieldcatalog.
    INSERT VALUE #( name         = i_fieldcatalog_name->value
                    fieldcatalog = i_fieldcatalog )
           INTO TABLE fieldcatalogs.
  ENDMETHOD.

  METHOD merge_technical_fieldcatalog.
    r_result = i_table_descriptor->get_fieldcatalog( ).
    LOOP AT r_result ASSIGNING FIELD-SYMBOL(<fieldcatalog_line>).
      <fieldcatalog_line>-coltext   = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-scrtext_l = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-scrtext_m = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-scrtext_s = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-reptext   = <fieldcatalog_line>-fieldname.
      <fieldcatalog_line>-seltext   = <fieldcatalog_line>-fieldname.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_fieldcatalog.
    TRY.
        r_result = fieldcatalogs[ name = i_fieldcatalog_name->value ]-fieldcatalog.
      CATCH cx_sy_itab_line_not_found.
        r_result = get_fieldcatalog( lcl_fieldcatalog_name_enum=>internal ).
    ENDTRY.
  ENDMETHOD.

  METHOD has_fieldcatalog.
    r_result = boolc( line_exists( fieldcatalogs[ name = i_fieldcatalog_name->value ] ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_grid_control DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_container               TYPE REF TO cl_gui_container
                i_fieldcatalog_collection TYPE REF TO lcl_fieldcatalog_collection
                i_internal_table_ref      TYPE REF TO data
      RAISING   /usi/cx_bal_root.

    METHODS display.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF c_user_commands,
                 set_external_fcat  TYPE ui_func VALUE 'SET_FCAT_EXTERNAL',
                 set_internal_fcat  TYPE ui_func VALUE 'SET_FCAT_INTERNAL',
                 set_technical_fcat TYPE ui_func VALUE 'SET_FCAT_TECHNICAL',
               END   OF c_user_commands.

    DATA: BEGIN OF input,
            fieldcatalog_collection TYPE REF TO lcl_fieldcatalog_collection,
            internal_table_ref      TYPE REF TO data,
          END   OF input.

    DATA: BEGIN OF grid,
            instance           TYPE REF TO cl_gui_alv_grid,
            fieldcatalog_name  TYPE REF TO lcl_fieldcatalog_name_enum,
            fieldcatalog       TYPE lvc_t_fcat,
            excluded_functions TYPE ui_functions,
            layout             TYPE lvc_s_layo,
          END   OF grid.

    METHODS get_excluded_functions
      RETURNING VALUE(r_result) TYPE ui_functions.

    METHODS get_layout
      RETURNING VALUE(r_result) TYPE lvc_s_layo.

    METHODS on_alv_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS on_alv_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS refresh_screen.

ENDCLASS.


CLASS lcl_grid_control IMPLEMENTATION.
  METHOD constructor.
    input-fieldcatalog_collection = i_fieldcatalog_collection.
    input-internal_table_ref      = i_internal_table_ref.

    grid-fieldcatalog_name  = COND #( LET default_fcat = lcl_fieldcatalog_name_enum=>external
                                      IN
                                      WHEN input-fieldcatalog_collection->has_fieldcatalog( default_fcat ) = abap_true
                                      THEN default_fcat
                                      ELSE lcl_fieldcatalog_name_enum=>internal ).

    grid-excluded_functions = get_excluded_functions( ).
    grid-layout             = get_layout( ).

    CREATE OBJECT grid-instance
      EXPORTING  i_parent = i_container
      EXCEPTIONS OTHERS   = 0.

    SET HANDLER on_alv_toolbar      FOR grid-instance.
    SET HANDLER on_alv_user_command FOR grid-instance.
  ENDMETHOD.

  METHOD get_excluded_functions.
    r_result = VALUE #( ( cl_gui_alv_grid=>mc_fc_col_invisible )
                        ( cl_gui_alv_grid=>mc_fc_col_optimize  )
                        ( cl_gui_alv_grid=>mc_fc_fix_columns   )
                        ( cl_gui_alv_grid=>mc_fc_graph         )
                        ( cl_gui_alv_grid=>mc_fc_info          )
                        ( cl_gui_alv_grid=>mc_fc_loc_copy      )
                        ( cl_gui_alv_grid=>mc_fc_print         )
                        ( cl_gui_alv_grid=>mc_fc_unfix_columns )
                        ( cl_gui_alv_grid=>mc_mb_paste         )
                        ( cl_gui_alv_grid=>mc_mb_subtot        )
                        ( cl_gui_alv_grid=>mc_mb_sum           )
                        ( cl_gui_alv_grid=>mc_mb_view          ) ).
  ENDMETHOD.

  METHOD get_layout.
    r_result = VALUE #( zebra      = abap_true
                        cwidth_opt = abap_true ).
  ENDMETHOD.

  METHOD on_alv_toolbar.
    IF input-fieldcatalog_collection->has_fieldcatalog( lcl_fieldcatalog_name_enum=>external ) = abap_true.
      INSERT VALUE #( function = c_user_commands-set_external_fcat
                      disabled = boolc( grid-fieldcatalog_name = lcl_fieldcatalog_name_enum=>external )
                      text     = 'External fieldcatalog'(b01) )
             INTO TABLE e_object->mt_toolbar.
    ENDIF.

    INSERT VALUE #( function = c_user_commands-set_internal_fcat
                    disabled = boolc( grid-fieldcatalog_name = lcl_fieldcatalog_name_enum=>internal )
                    text     = 'Regular fieldcatalog'(b02) )
           INTO TABLE e_object->mt_toolbar.

    INSERT VALUE #( function = c_user_commands-set_technical_fcat
                    disabled = boolc( grid-fieldcatalog_name = lcl_fieldcatalog_name_enum=>technical )
                    text     = 'Technical fieldnames'(b03) )
           INTO TABLE e_object->mt_toolbar.
  ENDMETHOD.

  METHOD on_alv_user_command.
    CASE e_ucomm.
      WHEN c_user_commands-set_external_fcat.
        grid-fieldcatalog_name = lcl_fieldcatalog_name_enum=>external.
      WHEN c_user_commands-set_internal_fcat.
        grid-fieldcatalog_name = lcl_fieldcatalog_name_enum=>internal.
      WHEN c_user_commands-set_technical_fcat.
        grid-fieldcatalog_name = lcl_fieldcatalog_name_enum=>technical.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    refresh_screen( ).
  ENDMETHOD.

  METHOD display.
    refresh_screen( ).
  ENDMETHOD.

  METHOD refresh_screen.
    FIELD-SYMBOLS <internal_table> TYPE STANDARD TABLE.

    ASSIGN input-internal_table_ref->* TO <internal_table>.

    grid-fieldcatalog = input-fieldcatalog_collection->get_fieldcatalog( grid-fieldcatalog_name ).

    grid-instance->set_table_for_first_display( EXPORTING  is_layout            = grid-layout
                                                           it_toolbar_excluding = grid-excluded_functions
                                                CHANGING   it_outtab            = <internal_table>
                                                           it_fieldcatalog      = grid-fieldcatalog
                                                EXCEPTIONS OTHERS               = 0 ).
  ENDMETHOD.
ENDCLASS.
