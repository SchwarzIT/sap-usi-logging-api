CLASS lcl_data_container_selector DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col.

    METHODS render
      IMPORTING
        i_container TYPE REF TO cl_gui_container.

    EVENTS select_container
      EXPORTING
        VALUE(container) TYPE REF TO /usi/if_bal_data_container.

  PRIVATE SECTION.
    TYPES: ty_tree_items          TYPE STANDARD TABLE OF mtreeitm WITH NON-UNIQUE DEFAULT KEY,
           ty_data_container_type TYPE c LENGTH 1,

           BEGIN OF ty_sorted_data_container,
             type           TYPE ty_data_container_type,
             classname      TYPE /usi/bal_data_cont_classname,
             description    TYPE /usi/bal_data_cont_description,
             data_container TYPE REF TO /usi/if_bal_data_container,
           END   OF ty_sorted_data_container,
           ty_sorted_data_containers TYPE SORTED TABLE OF ty_sorted_data_container
                                            WITH UNIQUE KEY type classname description,

           BEGIN OF ty_node_container,
             node_key       TYPE tv_nodekey,
             data_container TYPE REF TO /usi/if_bal_data_container,
           END   OF ty_node_container,
           ty_node_containers TYPE HASHED TABLE OF ty_node_container WITH UNIQUE KEY node_key.

    CONSTANTS: BEGIN OF data_container_types,
                 renderer  TYPE ty_data_container_type VALUE 'R',
                 navigator TYPE ty_data_container_type VALUE 'N',
                 unknown   TYPE ty_data_container_type VALUE '?',
               END   OF data_container_types.

    DATA: node_containers TYPE ty_node_containers,
          nodes           TYPE treev_ntab,
          items           TYPE ty_tree_items,
          tree_control    TYPE REF TO cl_gui_list_tree.

    METHODS get_data_container_type
      IMPORTING
        i_data_container TYPE REF TO /usi/if_bal_data_container
      RETURNING
        VALUE(r_result)  TYPE ty_data_container_type.

    METHODS sort_data_containers
      IMPORTING
        i_data_container_collection TYPE REF TO /usi/if_bal_data_container_col
      RETURNING
        VALUE(r_result)             TYPE ty_sorted_data_containers.

    METHODS insert_data_containers
      IMPORTING
        i_sorted_data_containers TYPE ty_sorted_data_containers.

    METHODS insert_node_and_item_container
      IMPORTING
        i_node_key       TYPE tv_nodekey
        i_parent         TYPE tv_nodekey
        i_image          TYPE icon_l2
        i_data_container TYPE REF TO /usi/if_bal_data_container.

    METHODS insert_node_and_item_folder
      IMPORTING
        i_node_key TYPE tv_nodekey
        i_parent   TYPE tv_nodekey OPTIONAL
        i_text     TYPE scrpcha72.

    METHODS register_events.

    METHODS on_tree_item_double_click FOR EVENT item_double_click OF cl_gui_list_tree
      IMPORTING node_key.

    METHODS on_tree_node_double_click FOR EVENT node_double_click OF cl_gui_list_tree
      IMPORTING node_key.

ENDCLASS.
