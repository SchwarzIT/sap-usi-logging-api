CLASS /usi/cl_bal_dc_collection DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /usi/if_exception_details .
    INTERFACES /usi/if_bal_data_container_col.

    ALIASES: deserialize FOR /usi/if_bal_data_container_col~deserialize,
             insert      FOR /usi/if_bal_data_container_col~insert.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES: BEGIN OF ty_data_cont_coll_item,
             data_container_classname TYPE /usi/bal_data_cont_classname,
             data_container           TYPE REF TO /usi/if_bal_data_container,
           END   OF ty_data_cont_coll_item,
           ty_data_cont_coll_items TYPE SORTED TABLE OF ty_data_cont_coll_item
                                   WITH NON-UNIQUE KEY data_container_classname.

    TYPES: BEGIN OF ty_serialized_data_container,
             data_container_classname  TYPE /usi/bal_data_cont_classname,
             serialized_data_container TYPE /usi/bal_xml_string,
           END   OF ty_serialized_data_container,
           ty_serialized_data_containers TYPE STANDARD TABLE OF ty_serialized_data_container
                                                  WITH NON-UNIQUE DEFAULT KEY.

    DATA: data_cont_coll_items TYPE ty_data_cont_coll_items.

    METHODS is_cardinality_violation
      IMPORTING
        i_data_container TYPE REF TO /usi/if_bal_data_container
      RETURNING
        VALUE(r_result)  TYPE abap_bool.

    METHODS is_duplicate
      IMPORTING
        i_data_container TYPE REF TO /usi/if_bal_data_container
      RETURNING
        VALUE(r_result)  TYPE abap_bool
      RAISING
        /usi/cx_bal_root.
ENDCLASS.



CLASS /usi/cl_bal_dc_collection IMPLEMENTATION.


  METHOD /usi/if_bal_data_container_col~deserialize.
    DATA: data_container             TYPE REF TO /usi/if_bal_data_container,
          exception                  TYPE REF TO cx_root,
          exception_text             TYPE string,
          serialized_data_containers TYPE ty_serialized_data_containers.

    FIELD-SYMBOLS <serialized_data_container> TYPE ty_serialized_data_container.

    TRY.
        CALL TRANSFORMATION id
          SOURCE XML i_serialized_data_cont_coll
          RESULT serialized_data_containers = serialized_data_containers.
      CATCH cx_transformation_error INTO exception.
        RAISE EXCEPTION TYPE /usi/cx_bal_type_mismatch
          EXPORTING
            textid   = /usi/cx_bal_type_mismatch=>/usi/cx_bal_type_mismatch
            previous = exception.
    ENDTRY.

    CREATE OBJECT r_result TYPE /usi/cl_bal_dc_collection.
    LOOP AT serialized_data_containers ASSIGNING <serialized_data_container>.
      TRY.
          CALL METHOD (<serialized_data_container>-data_container_classname)=>/usi/if_bal_data_container~deserialize
            EXPORTING
              i_serialized_data_container = <serialized_data_container>-serialized_data_container
            RECEIVING
              r_result                    = data_container.

          r_result->insert( data_container ).
        CATCH cx_sy_dyn_call_error
              /usi/cx_bal_root INTO exception.
          exception_text = exception->get_text( ).
          ASSERT ID   /usi/bal_log_writer
            FIELDS    exception_text
            CONDITION exception IS NOT BOUND.

          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container_col~get_data_containers.
    FIELD-SYMBOLS <data_cont_coll_item> TYPE ty_data_cont_coll_item.
    LOOP AT data_cont_coll_items ASSIGNING <data_cont_coll_item>.
      INSERT <data_cont_coll_item>-data_container INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container_col~has_data_containers.
    IF data_cont_coll_items IS NOT INITIAL.
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container_col~insert.
    DATA: new_data_cont_coll_item TYPE ty_data_cont_coll_item.

    r_result = me.

    TRY.
        IF is_cardinality_violation( i_data_container ) EQ abap_true OR
           is_duplicate( i_data_container ) EQ abap_true.
          RETURN.
        ENDIF.
      CATCH /usi/cx_bal_root.
        RETURN.
    ENDTRY.

    new_data_cont_coll_item-data_container_classname = i_data_container->get_classname( ).
    new_data_cont_coll_item-data_container           = i_data_container.
    INSERT new_data_cont_coll_item INTO TABLE data_cont_coll_items.
  ENDMETHOD.


  METHOD /usi/if_bal_data_container_col~serialize.
    DATA: exception                  TYPE REF TO /usi/cx_bal_root,
          exception_text             TYPE string,
          serialized_data_containers TYPE ty_serialized_data_containers,
          serialized_data_container  TYPE ty_serialized_data_container.

    FIELD-SYMBOLS <data_cont_coll_item> TYPE ty_data_cont_coll_item.

    LOOP AT data_cont_coll_items ASSIGNING <data_cont_coll_item>.
      TRY.
          serialized_data_container-data_container_classname  = <data_cont_coll_item>-data_container_classname.
          serialized_data_container-serialized_data_container = <data_cont_coll_item>-data_container->serialize( ).
          INSERT serialized_data_container INTO TABLE serialized_data_containers.
        CATCH /usi/cx_bal_root INTO exception.
          " Corrupt container data? Drop container!
          exception_text = exception->get_text( ).
          ASSERT ID   /usi/bal_log_writer
            FIELDS    exception_text
            CONDITION exception IS NOT BOUND.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    CALL TRANSFORMATION id
      SOURCE serialized_data_containers = serialized_data_containers
      RESULT XML r_result.
  ENDMETHOD.


  METHOD is_cardinality_violation.
    DATA: data_container_classname TYPE /usi/bal_data_cont_classname.

    IF i_data_container->is_multiple_use_allowed( ) EQ abap_true.
      " Not restricted
      RETURN.
    ENDIF.

    data_container_classname = i_data_container->get_classname( ).
    READ TABLE data_cont_coll_items
      TRANSPORTING NO FIELDS
      WITH KEY data_container_classname = data_container_classname.
    IF sy-subrc EQ 0.
      " Would violate Cardinality-Restriction [0-1]
      r_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_duplicate.
    DATA: data_container_classname  TYPE /usi/bal_data_cont_classname,
          serialized_data_container TYPE /usi/bal_xml_string.
    FIELD-SYMBOLS: <data_cont_coll_item> TYPE ty_data_cont_coll_item.

    data_container_classname  = i_data_container->get_classname( ).
    serialized_data_container = i_data_container->serialize( ).
    LOOP AT data_cont_coll_items ASSIGNING <data_cont_coll_item>
                                 WHERE data_container_classname EQ data_container_classname.
      CHECK <data_cont_coll_item>-data_container EQ i_data_container
         OR <data_cont_coll_item>-data_container->serialize( ) EQ serialized_data_container.
      r_result = abap_true.
      RETURN.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
