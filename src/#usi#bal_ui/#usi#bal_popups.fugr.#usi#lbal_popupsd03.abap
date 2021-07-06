CLASS lcl_log_message_detail DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES lif_screen_controller.

    METHODS constructor
      IMPORTING
        i_message_parameters TYPE bal_t_par
      RAISING
        /usi/cx_bal_root.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_message_parameters,
             log_number     TYPE balognr,
             message_number TYPE /usi/bal_message_number,
           END   OF ty_message_parameters.

    DATA: main_container          TYPE REF TO cl_gui_custom_container,
          detail_container        TYPE REF TO cl_gui_container,
          message_parameters      TYPE ty_message_parameters,
          data_container_selector TYPE REF TO lcl_data_container_selector.

    METHODS get_message_parameters
      IMPORTING
        i_message_parameters TYPE bal_t_par
      RETURNING
        VALUE(r_result)      TYPE ty_message_parameters.

    METHODS get_serialized_data_cont_coll
      IMPORTING
        i_message_parameters TYPE ty_message_parameters
      RETURNING
        VALUE(r_result)      TYPE /usi/bal_xml_string
      RAISING
        /usi/cx_bal_root.

    METHODS on_select_container FOR EVENT select_container OF lcl_data_container_selector
      IMPORTING container.

    METHODS free_children
      IMPORTING
        i_parent TYPE REF TO cl_gui_container.

ENDCLASS.
