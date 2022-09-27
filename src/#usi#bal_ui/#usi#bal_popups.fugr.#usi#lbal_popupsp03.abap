CLASS lcl_log_message_detail IMPLEMENTATION.
  METHOD constructor.
    DATA: serialized_data_cont_coll TYPE /usi/bal_xml_string,
          data_container_collection TYPE REF TO /usi/if_bal_data_container_col.

    message_parameters        = get_message_parameters( i_message_parameters ).
    serialized_data_cont_coll = get_serialized_data_cont_coll( message_parameters ).
    data_container_collection = /usi/cl_bal_dc_collection=>/usi/if_bal_data_container_col~deserialize(
                                        serialized_data_cont_coll ).

    CREATE OBJECT data_container_selector
      EXPORTING
        i_data_container_collection = data_container_collection.
  ENDMETHOD.

  METHOD get_message_parameters.
    FIELD-SYMBOLS <message_parameter> TYPE bal_s_par.

    LOOP AT i_message_parameters ASSIGNING <message_parameter>.
      CASE <message_parameter>-parname.
        WHEN /usi/cl_bal_enum_message_param=>log_number->value.
          r_result-log_number      = <message_parameter>-parvalue.
        WHEN /usi/cl_bal_enum_message_param=>message_number->value.
          r_result-message_number  = <message_parameter>-parvalue.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_serialized_data_cont_coll.
    DATA: dao_factory                   TYPE REF TO /usi/if_bal_logger_dao_factory,
          data_container_collection_dao TYPE REF TO /usi/if_bal_data_cont_coll_dao.

    dao_factory                   = /usi/cl_bal_logger_dao_factory=>get_instance( ).
    data_container_collection_dao = dao_factory->get_data_container_collection( ).
    r_result                      = data_container_collection_dao->get_collection(
                                          i_log_number     = i_message_parameters-log_number
                                          i_message_number = i_message_parameters-message_number ).
  ENDMETHOD.

  METHOD lif_screen_controller~set_status.
    DATA: BEGIN OF formatted_parameters,
            log_number     TYPE c LENGTH 21,
            message_number TYPE c LENGTH 7,
          END   OF formatted_parameters.

    WRITE message_parameters-log_number TO formatted_parameters-log_number NO-ZERO.
    SHIFT formatted_parameters-log_number LEFT DELETING LEADING space.
    CONCATENATE `#` formatted_parameters-log_number INTO formatted_parameters-log_number IN CHARACTER MODE.

    WRITE message_parameters-message_number TO formatted_parameters-message_number NO-ZERO.
    SHIFT formatted_parameters-message_number LEFT DELETING LEADING space.
    CONCATENATE `#` formatted_parameters-message_number INTO formatted_parameters-message_number IN CHARACTER MODE.

    SET PF-STATUS 'POPUP_CLOSE'.
    SET TITLEBAR 'TITLE_MESSAGE_DETAIL' WITH formatted_parameters-log_number formatted_parameters-message_number.
  ENDMETHOD.

  METHOD lif_screen_controller~display_data.
    DATA: splitter_container   TYPE REF TO cl_gui_splitter_container,
          navigation_container TYPE REF TO cl_gui_container.

    IF main_container IS BOUND.
      RETURN.
    ENDIF.

    CREATE OBJECT main_container
      EXPORTING
        container_name = 'CUSTOM_CONTROL_2000'
      EXCEPTIONS
        OTHERS         = 0.

    CREATE OBJECT splitter_container
      EXPORTING
        parent  = main_container
        rows    = 1
        columns = 2
      EXCEPTIONS
        OTHERS  = 0.
    splitter_container->set_column_width( id    = 1
                                          width = 25 ).

    navigation_container = splitter_container->get_container( row    = 1
                                                              column = 1 ).
    data_container_selector->render( navigation_container ).
    SET HANDLER on_select_container FOR data_container_selector.

    detail_container = splitter_container->get_container( row    = 1
                                                          column = 2 ).
  ENDMETHOD.

  METHOD on_select_container.
    DATA: navigator        TYPE REF TO /usi/if_bal_data_container_nav,
          renderer         TYPE REF TO /usi/if_bal_data_container_rnd,
          render_exception TYPE REF TO /usi/cx_bal_root.

    TRY.
        renderer ?= container.
        free_children( detail_container ).
        TRY.
            renderer->render( detail_container ).
          CATCH /usi/cx_bal_root INTO render_exception.
            MESSAGE render_exception TYPE 'S' DISPLAY LIKE 'E'.
        ENDTRY.
        RETURN.
      CATCH cx_sy_move_cast_error.
        CLEAR renderer.
    ENDTRY.

    TRY.
        navigator ?= container.
        navigator->navigate( ).
        RETURN.
      CATCH cx_sy_move_cast_error.
        CLEAR navigator.
    ENDTRY.

    " Broken data container - it is neither a navigator, nor a renderer!
    ASSERT ID /usi/bal_log_writer
      CONDITION 1 EQ 0.
  ENDMETHOD.

  METHOD lif_screen_controller~on_exit_command.
    IF i_exit_command EQ 'CLOSE'.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDMETHOD.

  METHOD lif_screen_controller~free.
    free_children( main_container ).
    main_container->free( ).
  ENDMETHOD.

  METHOD free_children.
    DATA container TYPE REF TO cl_gui_container.

    FIELD-SYMBOLS <child> TYPE REF TO cl_gui_control.

    LOOP AT i_parent->children ASSIGNING <child>.
      TRY.
          container ?= <child>.
          free_children( container ).
        CATCH cx_sy_move_cast_error.
      ENDTRY.

      <child>->free( ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
