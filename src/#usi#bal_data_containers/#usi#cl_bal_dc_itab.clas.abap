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
    "! @parameter i_title | Optional: Title text (useful, if more than one Itab is appended)
    "! @parameter i_fieldcatalog | Optional: Field catalog
    METHODS constructor
      IMPORTING
        i_internal_table TYPE ANY TABLE
        i_title          TYPE REF TO /usi/if_bal_text_container_c40 OPTIONAL
        i_fieldcatalog   TYPE lvc_t_fcat OPTIONAL.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: internal_table_ref    TYPE REF TO data,
          table_descriptor      TYPE REF TO lcl_table_descriptor,
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

    CREATE OBJECT deserializer TYPE lcl_deserializer
      EXPORTING
        i_serialized_data_container = i_serialized_data_container.

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

    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_itab
      EXPORTING
        i_internal_table = <internal_table>
        i_title          = deserialized_data-title
        i_fieldcatalog   = deserialized_data-external_fieldcatalog.
  ENDMETHOD.


  METHOD constructor.
    DATA: exception            TYPE REF TO /usi/cx_bal_root,
          exception_text       TYPE string,
          source_table_ref     TYPE REF TO data,
          table_content_copier TYPE REF TO lcl_table_content_copier.

    TRY.
        table_descriptor   = lcl_table_descriptor=>get_by_data( i_internal_table ).
        internal_table_ref = table_descriptor->get_table_type_dref( ).

        GET REFERENCE OF i_internal_table INTO source_table_ref.
        CREATE OBJECT table_content_copier
          EXPORTING
            i_source_table_ref = source_table_ref
            i_target_table_ref = internal_table_ref.
        table_content_copier->copy_table_contents( ).

      CATCH /usi/cx_bal_root INTO exception.
        exception_text = exception->get_text( ).

        ASSERT ID /usi/bal_log_writer
          FIELDS exception_text
          CONDITION exception IS NOT BOUND.
        RETURN.
    ENDTRY.

    title                 = i_title.
    external_fieldcatalog = i_fieldcatalog.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~serialize.
    DATA serializer TYPE REF TO lif_serializer.

    IF internal_table_ref IS NOT BOUND.
      RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
        EXPORTING
          textid = /usi/cx_bal_invalid_input=>unsupported_line_type.
    ENDIF.

    CREATE OBJECT serializer TYPE lcl_serializer
      EXPORTING
        i_table_reference       = internal_table_ref
        i_table_descriptor      = table_descriptor
        i_external_fieldcatalog = external_fieldcatalog
        i_title                 = title.

    r_result = serializer->serialize( ).
  ENDMETHOD.


  METHOD /usi/if_bal_data_container_rnd~render.
    DATA: fieldcatalog_collection TYPE REF TO lcl_fieldcatalog_collection,
          grid_control            TYPE REF TO lcl_grid_control.

    CREATE OBJECT fieldcatalog_collection
      EXPORTING
        i_table_descriptor      = table_descriptor
        i_external_fieldcatalog = external_fieldcatalog.

    CREATE OBJECT grid_control
      EXPORTING
        i_container               = i_container
        i_fieldcatalog_collection = fieldcatalog_collection
        i_internal_table_ref      = internal_table_ref.

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
      CONCATENATE r_result `: ` title_text INTO r_result IN CHARACTER MODE.
    ENDIF.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_true.
  ENDMETHOD.
ENDCLASS.
