CLASS /usi/cl_bal_dc_xml DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_data_container_rnd.

    ALIASES get_classname FOR /usi/if_bal_data_container~get_classname.

    "! Constructor
    "!
    "! @parameter i_xml_document   | XML-Document as string
    "! @parameter i_document_title | Optional: Document title (Useful, if multiple documents are appended)
    METHODS constructor
      IMPORTING i_xml_document   TYPE string
                i_document_title TYPE REF TO /usi/if_bal_text_container_c40 OPTIONAL.

  PRIVATE SECTION.
    TYPES: ty_xml_table_line TYPE c LENGTH 1000,
           ty_xml_table      TYPE STANDARD TABLE OF ty_xml_table_line WITH EMPTY KEY.

    DATA: xml_document   TYPE string,
          document_title TYPE REF TO /usi/if_bal_text_container_c40,
          xml_table      TYPE ty_xml_table.

    METHODS get_xml_table
      IMPORTING i_xml_document  TYPE string
      RETURNING VALUE(r_result) TYPE ty_xml_table.

    METHODS raise_exception_on_subrc
      RAISING /usi/cx_bal_root.

ENDCLASS.


CLASS /usi/cl_bal_dc_xml IMPLEMENTATION.
  METHOD /usi/if_bal_data_container~deserialize.
    DATA: deserializer              TYPE REF TO /usi/cl_bal_serializer,
          document_title            TYPE REF TO /usi/if_bal_text_container_c40,
          document_title_classname  TYPE /usi/bal_text_cont_classname,
          exception                 TYPE REF TO cx_root,
          exception_text            TYPE string,
          serialized_document_title TYPE /usi/bal_xml_string,
          parameter                 TYPE abap_trans_srcbind,
          parameters                TYPE abap_trans_srcbind_tab,
          xml_document              TYPE string.

    parameter-name = `XML_DOCUMENT`.
    GET REFERENCE OF xml_document INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    parameter-name = `DOCUMENT_TITLE_CLASSNAME`.
    GET REFERENCE OF document_title_classname INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    parameter-name = `SERIALIZED_DOCUMENT_TITLE`.
    GET REFERENCE OF serialized_document_title INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    deserializer = NEW #( ).
    deserializer->deserialize_fields( i_serialized_data = i_serialized_data_container
                                      i_parameters      = parameters ).

    IF document_title_classname IS NOT INITIAL.
      TRY.
          CALL METHOD (document_title_classname)=>/usi/if_bal_text_container_c40~deserialize
            EXPORTING i_serialized_text_container = serialized_document_title
            RECEIVING r_result                    = document_title.
        CATCH cx_sy_dyn_call_error
              /usi/cx_bal_root INTO exception.
          exception_text = exception->get_text( ).
          ASSERT ID /usi/bal_log_writer
                 FIELDS exception_text
                 CONDITION exception IS NOT BOUND.

          CLEAR document_title.
      ENDTRY.
    ENDIF.

    r_result = NEW /usi/cl_bal_dc_xml( i_xml_document   = xml_document
                                       i_document_title = document_title ).
  ENDMETHOD.

  METHOD constructor.
    xml_document   = i_xml_document.
    document_title = i_document_title.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~serialize.
    DATA: document_title_classname  TYPE /usi/bal_text_cont_classname,
          serialized_document_title TYPE /usi/bal_xml_string,
          parameter                 TYPE abap_trans_srcbind,
          parameters                TYPE abap_trans_srcbind_tab,
          serializer                TYPE REF TO /usi/cl_bal_serializer.

    IF document_title IS BOUND.
      document_title_classname  = document_title->get_classname( ).
      serialized_document_title = document_title->serialize( ).
    ENDIF.

    parameter-name = `XML_DOCUMENT`.
    GET REFERENCE OF xml_document INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    parameter-name = `DOCUMENT_TITLE_CLASSNAME`.
    GET REFERENCE OF document_title_classname INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    parameter-name = `SERIALIZED_DOCUMENT_TITLE`.
    GET REFERENCE OF serialized_document_title INTO parameter-value.
    INSERT parameter INTO TABLE parameters.

    serializer = NEW #( ).
    r_result = serializer->serialize_fields_as_json( parameters ).
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_classname.
    r_result = '/USI/CL_BAL_DC_XML'.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~get_description.
    DATA document_title_text TYPE /usi/if_bal_text_container_c40=>ty_text.

    IF document_title IS BOUND.
      document_title_text = document_title->get_text( ).
    ENDIF.
    r_result = TEXT-des.

    IF document_title_text IS NOT INITIAL.
      CONCATENATE r_result `: ` document_title_text INTO r_result IN CHARACTER MODE.
    ENDIF.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container~is_multiple_use_allowed.
    r_result = abap_true.
  ENDMETHOD.

  METHOD /usi/if_bal_data_container_rnd~render.
    DATA: document_size TYPE int4,
          xml_viewer    TYPE REF TO cl_gui_html_viewer,
          url           TYPE c LENGTH 1024.

    CREATE OBJECT xml_viewer
      EXPORTING  parent             = i_container
      EXCEPTIONS cntl_error         = 1
                 cntl_install_error = 2
                 dp_install_error   = 3
                 dp_error           = 4
                 OTHERS             = 5.
    IF sy-subrc <> 0.
      raise_exception_on_subrc( ).
    ENDIF.

    xml_table     = get_xml_table( xml_document ).
    document_size = strlen( xml_document ).
    xml_viewer->load_data( EXPORTING  subtype                = 'xml'
                                      size                   = document_size
                           IMPORTING  assigned_url           = url
                           CHANGING   data_table             = xml_table
                           EXCEPTIONS dp_invalid_parameter   = 1
                                      dp_error_general       = 2
                                      cntl_error             = 3
                                      html_syntax_notcorrect = 4
                                      OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      raise_exception_on_subrc( ).
    ENDIF.

    xml_viewer->show_url( EXPORTING  url                    = url
                          EXCEPTIONS cntl_error             = 1
                                     cnht_error_not_allowed = 2
                                     cnht_error_parameter   = 3
                                     dp_error_general       = 4
                                     OTHERS                 = 5 ).
    IF sy-subrc <> 0.
      raise_exception_on_subrc( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_xml_table.
    DATA: xml_string  TYPE string,
          result_line TYPE ty_xml_table_line,
          line_size   TYPE int4.

    xml_string = i_xml_document.

    DESCRIBE FIELD result_line LENGTH line_size IN CHARACTER MODE.
    WHILE xml_string IS NOT INITIAL.
      result_line = xml_string.
      APPEND result_line TO r_result.
      SHIFT xml_string LEFT BY line_size PLACES IN CHARACTER MODE.
    ENDWHILE.
  ENDMETHOD.

  METHOD raise_exception_on_subrc.
    RAISE EXCEPTION TYPE /usi/cx_bal_invalid_input
      EXPORTING textid = VALUE #( msgid = sy-msgid
                                  msgno = sy-msgno
                                  attr1 = 'PARAM1'
                                  attr2 = 'PARAM2'
                                  attr3 = 'PARAM3'
                                  attr4 = 'PARAM4' )
                param1 = sy-msgv1
                param2 = sy-msgv2
                param3 = sy-msgv3
                param4 = sy-msgv4.
  ENDMETHOD.
ENDCLASS.
