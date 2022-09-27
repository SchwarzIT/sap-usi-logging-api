CLASS /usi/cl_bal_serializer DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    "! <h1>Serialize single ABAP-Field as JSON</h1>
    "!
    "! <p>I_DATA must not be a reference pointing to data, but the data itself.
    "! Data-References need to be dereferenced using a Field-Symbol.</p>
    "!
    "! <p>I_NAME is the identifier under which the data will be stored in the generated document.
    "! The exact same identifier (case-sensitive!) will be needed to deserialize the data later.
    "! Using ALL-UPPERCASE for identifiers is highly recommended.</p>
    "!
    "! @parameter i_data | ABAP-Field to serialize
    "! @parameter i_name | Identifier to use in the JSON/XML-Document
    "! @parameter r_result | JSON-Document
    METHODS serialize_field_as_json
      IMPORTING
        i_data          TYPE data
        i_name          TYPE abap_trans_srcname
      RETURNING
        VALUE(r_result) TYPE /usi/bal_json_string.

    "! <h1>Serialize multiple ABAP-Fields as JSON</h1>
    "!
    "! <p>I_PARAMETERS-NAME is the identifier under which the data will be stored in the generated document.
    "! The exact same identifier (case-sensitive!) will be needed to deserialize the data later.
    "! Using ALL-UPPERCASE for identifiers is highly recommended.</p>
    "!
    "! <p>I_PARAMETERS-VALUE is a data reference, that must point directly to the to-be-serialized data.
    "! Data-References pointing to other data-references are NOT supported!</p>
    "!
    "! @parameter i_parameters | Pairs of JSON/XML-Identifiers & Data-References
    "! @parameter r_result | JSON-Document
    METHODS serialize_fields_as_json
      IMPORTING
        i_parameters    TYPE abap_trans_srcbind_tab
      RETURNING
        VALUE(r_result) TYPE /usi/bal_json_string.

    "! <h1>Serialize single ABAP-Field as XML</h1>
    "!
    "! <p>I_DATA must not be a reference pointing to data, but the data itself.
    "! Data-References need to be dereferenced using a Field-Symbol.</p>
    "!
    "! <p>I_NAME is the identifier under which the data will be stored in the generated document.
    "! The exact same identifier (case-sensitive!) will be needed to deserialize the data later.
    "! Using ALL-UPPERCASE for identifiers is highly recommended.</p>
    "!
    "! @parameter i_data | ABAP-Field to serialize
    "! @parameter i_name | Identifier to use in the JSON/XML-Document
    "! @parameter r_result | XML-Document
    METHODS serialize_field_as_xml
      IMPORTING
        i_data          TYPE data
        i_name          TYPE abap_trans_srcname
      RETURNING
        VALUE(r_result) TYPE /usi/bal_xml_string.

    "! <h1>Serialize multiple ABAP-Fields as XML</h1>
    "!
    "! <p>I_PARAMETERS-NAME is the identifier under which the data will be stored in the generated document.
    "! The exact same identifier (case-sensitive!) will be needed to deserialize the data later.
    "! Using ALL-UPPERCASE for identifiers is highly recommended.</p>
    "!
    "! <p>I_PARAMETERS-VALUE is a data reference, that must point directly to the to-be-serialized data.
    "! Data-References pointing to other data-references are NOT supported!</p>
    "!
    "! @parameter i_parameters | Pairs of JSON/XML-Identifiers & Data-References
    "! @parameter r_result | XML-Document
    METHODS serialize_fields_as_xml
      IMPORTING
        i_parameters    TYPE abap_trans_srcbind_tab
      RETURNING
        VALUE(r_result) TYPE /usi/bal_xml_string.

    "! <h1>Deserialize single ABAP-Field from JSON or XML</h1>
    "!
    "! <p>I_SERIALIZED_DATA may contain either a JSON or an XML document.</p>
    "!
    "! <p>I_NAME is the identifier under which the data was stored in the generated document.
    "! It has to be the exact same value (case-sensitive!), that was used during serialization.</p>
    "!
    "! <p>C_DATA must not be a data-reference pointing to the target-field, but the target-field itself.
    "! Data-References need to be dereferenced using a Field-Symbol.</p>
    "!
    "! @parameter i_serialized_data | JSON/XML-Document
    "! @parameter i_name | Identifier used in the JSON/XML-Document
    "! @parameter c_data | Target ABAP-Field (Must have the right type!)
    "! @raising /usi/cx_bal_root | Error during deserialization. A passed data reference might have the wrong type.
    METHODS deserialize_field
      IMPORTING
        i_serialized_data TYPE /usi/bal_serialized_data
        i_name            TYPE abap_trans_resname
      CHANGING
        c_data            TYPE data
      RAISING
        /usi/cx_bal_root.

    "! <h1>Deserialize multiple ABAP-Fields from JSON or XML</h1>
    "!
    "! <p>I_SERIALIZED_DATA may contain either a JSON or an XML document.</p>
    "!
    "! <p>I_PARAMETERS-NAME is the identifier under which the data was stored in the generated document.
    "! It has to be the exact same value (case-sensitive!), that was used during serialization.</p>
    "!
    "! <p>I_PARAMETERS-VALUE must be pointing directly to the target-field.
    "! Data-References pointing to other data-references are NOT supported!</p>
    "!
    "! @parameter i_serialized_data | JSON/XML-Document
    "! @parameter i_parameters | Pairs of JSON/XML-Identifiers & Data-References
    "! @raising /usi/cx_bal_root | Error during deserialization. A passed data reference might have the wrong type.
    METHODS deserialize_fields
      IMPORTING
        i_serialized_data TYPE /usi/bal_serialized_data
        i_parameters      TYPE abap_trans_resbind_tab
      RAISING
        /usi/cx_bal_root.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS serialize_fields
      IMPORTING
        i_parameters    TYPE abap_trans_srcbind_tab
        i_stream_type   TYPE if_sxml=>xml_stream_type
      RETURNING
        VALUE(r_result) TYPE /usi/bal_serialized_data.

ENDCLASS.



CLASS /usi/cl_bal_serializer IMPLEMENTATION.
  METHOD serialize_field_as_json.
    DATA: parameter  TYPE abap_trans_srcbind,
          parameters TYPE abap_trans_srcbind_tab.

    parameter-name = i_name.
    GET REFERENCE OF i_data INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    r_result = serialize_fields_as_json( parameters ).
  ENDMETHOD.

  METHOD serialize_field_as_xml.
    DATA: parameter  TYPE abap_trans_srcbind,
          parameters TYPE abap_trans_srcbind_tab.

    parameter-name = i_name.
    GET REFERENCE OF i_data INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    r_result = serialize_fields_as_xml( parameters ).
  ENDMETHOD.

  METHOD serialize_fields_as_json.
    r_result = serialize_fields( i_parameters  = i_parameters
                                 i_stream_type = if_sxml=>co_xt_json ).
  ENDMETHOD.

  METHOD serialize_fields_as_xml.
    r_result = serialize_fields( i_parameters  = i_parameters
                                 i_stream_type = if_sxml=>co_xt_xml10 ).
  ENDMETHOD.

  METHOD serialize_fields.
    DATA: binary_data              TYPE xstring,
          binary_to_char_converter TYPE REF TO cl_abap_conv_in_ce,
          string_writer            TYPE REF TO cl_sxml_string_writer.

    string_writer = cl_sxml_string_writer=>create( type     = i_stream_type
                                                   encoding = 'UTF-8' ).
    CALL TRANSFORMATION id
      SOURCE (i_parameters)
      RESULT XML string_writer.
    binary_data = string_writer->get_output( abap_false ).

    binary_to_char_converter = cl_abap_conv_in_ce=>create( encoding = 'UTF-8' ).
    binary_to_char_converter->convert(
      EXPORTING
        input = binary_data
      IMPORTING
        data  = r_result ).
  ENDMETHOD.

  METHOD deserialize_field.
    DATA: parameter  TYPE abap_trans_resbind,
          parameters TYPE abap_trans_resbind_tab.

    parameter-name = i_name.
    GET REFERENCE OF c_data INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    deserialize_fields( i_serialized_data = i_serialized_data
                        i_parameters      = parameters ).
  ENDMETHOD.

  METHOD deserialize_fields.
    DATA: binary_data              TYPE xstring,
          char_to_binary_converter TYPE REF TO cl_abap_conv_out_ce,
          exception                TYPE REF TO cx_transformation_error,
          string_reader            TYPE REF TO if_sxml_reader.

    char_to_binary_converter = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
    char_to_binary_converter->convert(
      EXPORTING
        data   = i_serialized_data
      IMPORTING
        buffer = binary_data ).

    string_reader = cl_sxml_string_reader=>create( binary_data ).

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML string_reader
          RESULT (i_parameters).
      CATCH cx_transformation_error INTO exception.
        RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
          EXPORTING
            textid   = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch
            previous = exception.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
