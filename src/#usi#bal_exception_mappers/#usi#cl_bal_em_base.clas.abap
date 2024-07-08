CLASS /usi/cl_bal_em_base DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES /usi/if_bal_exception_mapper.

    "! <h1>Constructor</h1>
    "!
    "! @parameter i_exception      | The to-be-mapped exception
    "! @raising   /usi/cx_bal_root | Not bound
    METHODS constructor
      IMPORTING i_exception TYPE REF TO cx_root
      RAISING   /usi/cx_bal_root.

  PROTECTED SECTION.
    TYPES ty_object_references TYPE STANDARD TABLE OF REF TO object WITH EMPTY KEY.

    DATA exception TYPE REF TO cx_root.

    METHODS get_exceptions_oref_attributes
      RETURNING VALUE(r_result) TYPE ty_object_references.

    METHODS get_attached_data_cont_coll
      IMPORTING i_source_data_cont_coll TYPE REF TO object
                i_target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

    METHODS get_attached_data_container
      IMPORTING i_source_data_container TYPE REF TO object
                i_target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

  PRIVATE SECTION.

ENDCLASS.


CLASS /usi/cl_bal_em_base IMPLEMENTATION.
  METHOD /usi/if_bal_exception_mapper~get_data_containers.
    DATA objects TYPE ty_object_references.
    FIELD-SYMBOLS <object> TYPE REF TO object.

    objects = get_exceptions_oref_attributes( ).
    LOOP AT objects ASSIGNING <object>.
      get_attached_data_cont_coll( i_source_data_cont_coll = <object>
                                   i_target_data_cont_coll = i_target_data_cont_coll ).

      get_attached_data_container( i_source_data_container = <object>
                                   i_target_data_cont_coll = i_target_data_cont_coll ).
    ENDLOOP.
  ENDMETHOD.

  METHOD /usi/if_bal_exception_mapper~get_t100_message.
    DATA text_getter TYPE REF TO /usi/cl_exception_text_getter.

    text_getter = NEW #( i_exception = exception ).

    r_result = text_getter->get_text_as_symsg( ).
  ENDMETHOD.

  METHOD constructor.
    IF i_exception IS NOT BOUND.
      RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
        EXPORTING textid = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch.
    ENDIF.
    exception = i_exception.
  ENDMETHOD.

  METHOD get_attached_data_container.
    DATA data_container TYPE REF TO /usi/if_bal_data_container.

    TRY.
        data_container ?= i_source_data_container.
      CATCH cx_sy_move_cast_error.
        " Wrong type
        RETURN.
    ENDTRY.

    i_target_data_cont_coll->insert( data_container ).
  ENDMETHOD.

  METHOD get_attached_data_cont_coll.
    DATA: data_container_collection TYPE REF TO /usi/if_bal_data_container_col,
          data_containers           TYPE /usi/bal_data_containers.
    FIELD-SYMBOLS <data_container> TYPE REF TO /usi/if_bal_data_container.

    TRY.
        data_container_collection ?= i_source_data_cont_coll.
      CATCH cx_sy_move_cast_error.
        " Wrong type
        RETURN.
    ENDTRY.

    data_containers = data_container_collection->get_data_containers( ).
    LOOP AT data_containers ASSIGNING <data_container>.
      i_target_data_cont_coll->insert( <data_container> ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_exceptions_oref_attributes.
    DATA class_description TYPE REF TO cl_abap_classdescr.

    FIELD-SYMBOLS: <attribute> TYPE abap_attrdescr,
                   <object>    TYPE any.

    class_description ?= cl_abap_classdescr=>describe_by_object_ref( exception ).
    LOOP AT class_description->attributes ASSIGNING <attribute> WHERE     visibility = cl_abap_classdescr=>public
                                                                      AND type_kind  = cl_abap_classdescr=>typekind_oref.
      ASSIGN exception->(<attribute>-name) TO <object>.
      IF NOT (     sy-subrc  = 0
               AND <object> IS BOUND ).
        CONTINUE.
      ENDIF.

      INSERT <object> INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
