CLASS lcl_data_container_selector IMPLEMENTATION.
  METHOD constructor.
    DATA: sorted_data_containers TYPE ty_sorted_data_containers.
    sorted_data_containers = sort_data_containers( i_data_container_collection ).
    insert_data_containers( sorted_data_containers ).
  ENDMETHOD.

  METHOD sort_data_containers.
    DATA: data_containers       TYPE /usi/bal_data_containers,
          sorted_data_container TYPE ty_sorted_data_container.

    FIELD-SYMBOLS: <data_container> TYPE REF TO /usi/if_bal_data_container.

    data_containers = i_data_container_collection->get_data_containers( ).
    LOOP AT data_containers ASSIGNING <data_container>.
      sorted_data_container-type            = get_data_container_type( <data_container> ).
      sorted_data_container-classname       = <data_container>->get_classname( ).
      sorted_data_container-description     = <data_container>->get_description( ).
      sorted_data_container-data_container  = <data_container>.
      INSERT sorted_data_container INTO TABLE r_result.
    ENDLOOP.
  ENDMETHOD.

  METHOD insert_data_containers.
    CONSTANTS: BEGIN OF node_keys,
                 root      TYPE tv_nodekey VALUE 'ND_ROOT',
                 renderer  TYPE tv_nodekey VALUE 'ND_RENDERER',
                 navigator TYPE tv_nodekey VALUE 'ND_NAVIGATOR',
                 unknown   TYPE tv_nodekey VALUE 'ND_UNKNOWN',
               END   OF node_keys.

    DATA: icon TYPE icon_l2,
          BEGIN OF needed_data_container_groups,
            renderer  TYPE abap_bool,
            navigator TYPE abap_bool,
            unknown   TYPE abap_bool,
          END   OF needed_data_container_groups,
          node_container TYPE ty_node_container,
          parent_node    TYPE tv_nodekey,
          tabix          TYPE n LENGTH 6.

    FIELD-SYMBOLS: <sorted_data_container> TYPE ty_sorted_data_container.

    " Create one item / node per data container
    LOOP AT i_sorted_data_containers ASSIGNING <sorted_data_container>.
      ADD 1 TO tabix.
      CONCATENATE 'ID_' tabix INTO node_container-node_key IN CHARACTER MODE.
      node_container-data_container = <sorted_data_container>-data_container.
      INSERT node_container INTO TABLE node_containers.

      CASE <sorted_data_container>-type.
        WHEN data_container_types-renderer.
          needed_data_container_groups-renderer   = abap_true.
          icon                                    = icon_select_detail.
          parent_node                             = node_keys-renderer.
        WHEN data_container_types-navigator.
          needed_data_container_groups-navigator  = abap_true.
          icon                                    = icon_reference_list.
          parent_node                             = node_keys-navigator.
        WHEN OTHERS.
          needed_data_container_groups-unknown    = abap_true.
          icon                                    = icon_failure.
          parent_node                             = node_keys-unknown.
      ENDCASE.

      insert_node_and_item_container(
        i_node_key        = node_container-node_key
        i_parent          = parent_node
        i_image           = icon
        i_data_container  = node_container-data_container
      ).
    ENDLOOP.

    " Create folders for container types
    IF needed_data_container_groups-unknown EQ abap_true.
      insert_node_and_item_folder(
        i_node_key = node_keys-unknown
        i_parent   = node_keys-root
        i_text     = TEXT-nuk " Node: Unknown
      ).
    ENDIF.
    IF needed_data_container_groups-navigator EQ abap_true.
      insert_node_and_item_folder(
        i_node_key = node_keys-navigator
        i_parent   = node_keys-root
        i_text     = TEXT-nnv " Node: Navigator
      ).
    ENDIF.
    IF needed_data_container_groups-navigator EQ abap_true.
      insert_node_and_item_folder(
        i_node_key = node_keys-renderer
        i_parent   = node_keys-root
        i_text     = TEXT-nrd " Node: Renderer
      ).
    ENDIF.

    " Create the root folder
    insert_node_and_item_folder(
      i_node_key = node_keys-root
      i_text     = TEXT-nrt " Node: Root
    ).
  ENDMETHOD.

  METHOD insert_node_and_item_container.
    DATA: node TYPE treev_node,
          item TYPE mtreeitm.

    node-node_key  = i_node_key.
    node-relatkey  = i_parent.
    node-n_image   = i_image.
    node-isfolder  = abap_false.
    node-relatship = cl_gui_list_tree=>relat_last_child.
    INSERT node INTO TABLE nodes.

    item-node_key  = i_node_key.
    item-item_name = '1'.
    item-class     = cl_gui_list_tree=>item_class_text.
    item-alignment = cl_gui_list_tree=>align_auto.
    item-font      = cl_gui_list_tree=>item_font_prop.
    item-text      = i_data_container->get_description( ).
    INSERT item INTO TABLE items.
  ENDMETHOD.

  METHOD insert_node_and_item_folder.
    DATA: node TYPE treev_node,
          item TYPE mtreeitm.

    CLEAR node.
    node-node_key   = i_node_key.
    node-relatkey   = i_parent.
    node-isfolder   = abap_true.
    INSERT node INTO nodes INDEX 1.

    CLEAR item.
    item-node_key   = i_node_key.
    item-item_name  = '1'.
    item-class      = cl_gui_list_tree=>item_class_text.
    item-alignment  = cl_gui_list_tree=>align_auto.
    item-font       = cl_gui_list_tree=>item_font_prop.
    item-text       = i_text.
    INSERT item INTO items INDEX 1.
  ENDMETHOD.

  METHOD render.
    CREATE OBJECT tree_control
      EXPORTING
        parent              = i_container
        node_selection_mode = cl_gui_list_tree=>node_sel_mode_single
        item_selection      = abap_true
        with_headers        = abap_false
      EXCEPTIONS
        OTHERS              = 0.

    register_events( ).
    SET HANDLER on_tree_item_double_click FOR tree_control.
    SET HANDLER on_tree_node_double_click FOR tree_control.

    tree_control->add_nodes_and_items(
      EXPORTING
        node_table                     = nodes
        item_table                     = items
        item_table_structure_name      = 'MTREEITM'
      EXCEPTIONS
        OTHERS                         = 0
    ).

    tree_control->expand_root_nodes(
      EXPORTING
        expand_subtree            = abap_true
      EXCEPTIONS
        OTHERS                    = 0
    ).
  ENDMETHOD.

  METHOD register_events.
    DATA: tree_event  TYPE cntl_simple_event,
          tree_events TYPE cntl_simple_events.

    tree_event-eventid    = cl_gui_list_tree=>eventid_item_double_click.
    tree_event-appl_event = abap_true.
    INSERT tree_event INTO TABLE tree_events.

    tree_event-eventid    = cl_gui_list_tree=>eventid_node_double_click.
    tree_event-appl_event = abap_true.
    INSERT tree_event INTO TABLE tree_events.

    tree_control->set_registered_events(
      EXPORTING
        events =  tree_events
      EXCEPTIONS
        OTHERS = 0
    ).
  ENDMETHOD.

  METHOD on_tree_item_double_click.
    FIELD-SYMBOLS: <node_containter> TYPE ty_node_container.

    READ TABLE  node_containers
      ASSIGNING <node_containter>
      WITH TABLE KEY node_key = node_key.

    IF sy-subrc EQ 0.
      RAISE EVENT select_container
        EXPORTING
          container = <node_containter>-data_container.
    ENDIF.
  ENDMETHOD.

  METHOD on_tree_node_double_click.
    on_tree_item_double_click( node_key ).
  ENDMETHOD.

  METHOD get_data_container_type.
    DATA: navigator TYPE REF TO /usi/if_bal_data_container_nav,
          renderer  TYPE REF TO /usi/if_bal_data_container_rnd.

    TRY.
        navigator ?= i_data_container.
        r_result = data_container_types-navigator.
      CATCH cx_sy_move_cast_error.
        TRY.
            renderer ?= i_data_container.
            r_result = data_container_types-renderer.
          CATCH cx_sy_move_cast_error.
            r_result = data_container_types-unknown.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
