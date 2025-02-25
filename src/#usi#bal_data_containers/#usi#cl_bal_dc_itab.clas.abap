CLASS /usi/cl_bal_dc_itab DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_message_details.
    INTERFACES /usi/if_exception_details.
    INTERFACES /usi/if_bal_data_container.
    INTERFACES /usi/if_bal_data_container_rnd.

    ALIASES get_classname FOR /usi/if_bal_data_container~get_classname.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_internal_table | Internal table
    "! @parameter i_title          | Optional: Title text (useful, if more than one Itab is appended)
    "! @parameter i_fieldcatalog   | Optional: Field catalog
    METHODS constructor
      IMPORTING i_internal_table TYPE ANY TABLE
                i_title          TYPE REF TO /usi/if_bal_text_container_c40 OPTIONAL
                i_fieldcatalog   TYPE lvc_t_fcat                            OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: internal_table_ref    TYPE REF TO data,
          title                 TYPE REF TO /usi/if_bal_text_container_c40,
          external_fieldcatalog TYPE lvc_t_fcat.

ENDCLASS.


CLASS /usi/cl_bal_dc_itab IMPLEMENTATION.
  METHOD /usi/if_bal_data_container~deserialize.
    DATA: deserializer TYPE REF TO lif_deserializer,
          BEGIN OF deserialized_data,
            external_fieldcatalog TYPE lvc_t_fcat,
            internal_table        TYPE REF TO data,
            title                 TYPE REF TO /usi/if_bal_text_container_c40,
          END   OF deserialized_data.

    FIELD-SYMBOLS <internal_table> TYPE STANDARD TABLE.

    deserializer = NEW lcl_deserializer( i_serialized_data_container = i_serialized_data_container ).

    deserialized_data-internal_table = deserializer->get_internal_table( ).
    ASSIGN deserialized_data-internal_table->* TO <internal_table>.

    TRY.
        deserialized_data-external_fieldcatalog = deserializer->get_external_fieldcatalog( ).
      CATCH /usi/cx_bal_not_found.
        CLEAR deserialized_data-external_fieldcatalog.
    ENDTRY.

    TRY.
        deserialized_data-title = deserializer->get_title( ).
      CATCH /usi/cx_bal_not_found.
        CLEAR deserialized_data-title.
    ENDTRY.

    r_result = NEW /usi/cl_bal_dc_itab( i_internal_table = <internal_table>
                                        i_title          = deserialized_data-title
                                        i_fieldcatalog   = deserialized_data-external_fieldcatalog ).
  ENDMETHOD.

  METHOD constructor.
    CREATE DATA internal_table_ref LIKE i_internal_table.
    ASSIGN internal_table_ref->* TO FIELD-SYMBOL(<table>).
    <table> = i_internal_table.

    title                 = i_title.
    external_fieldcatalog = i_fieldcatalog.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    " Normalize table format
    TRY.
        ASSIGN internal_table_ref->* TO FIELD-SYMBOL(<source_table>).

        DATA(table_descriptor) = lcl_table_descriptor=>get_by_data( <source_table> ).
        DATA(target_table_ref) = table_descriptor->get_table_type_dref( ).
        TRY.
            DATA(field_mapping) = table_descriptor->get_field_mapping( ).
          CATCH /usi/cx_bal_root.
            CLEAR field_mapping.
        ENDTRY.

        DATA(table_content_copier) = NEW lcl_table_content_copier( i_source_table_ref = internal_table_ref
                                                                   i_target_table_ref = target_table_ref
                                                                   i_field_mapping    = field_mapping ).
        table_content_copier->copy_table_contents( ).
      CATCH /usi/cx_bal_root INTO DATA(exception).
        RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
          EXPORTING textid   = /usi/cx_bal_invalid_input=>unsupported_line_type
                    previous = exception.
    ENDTRY.

    " Serialize
    DATA(serializer) = CAST lif_serializer( NEW lcl_serializer( i_table_reference       = target_table_ref
                                                                i_table_descriptor      = table_descriptor
                                                                i_external_fieldcatalog = external_fieldcatalog
                                                                i_title                 = title ) ).
    r_result = serializer->serialize( ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container_rnd~render.
    DATA: fieldcatalog_collection TYPE REF TO lcl_fieldcatalog_collection,
          grid_control            TYPE REF TO lcl_grid_control.

    ASSIGN internal_table_ref->* TO FIELD-SYMBOL(<table>).
    DATA(table_descriptor) = lcl_table_descriptor=>get_by_data( <table> ).

    fieldcatalog_collection = NEW #( i_table_descriptor      = table_descriptor
                                     i_external_fieldcatalog = external_fieldcatalog ).

    grid_control = NEW #( i_container               = i_container
                          i_fieldcatalog_collection = fieldcatalog_collection
                          i_internal_table_ref      = internal_table_ref ).

    grid_control->display( ).
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
      r_result = |{ r_result }: { title_text }|.
    ENDIF.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_true.
  ENDMETHOD.
ENDCLASS.
