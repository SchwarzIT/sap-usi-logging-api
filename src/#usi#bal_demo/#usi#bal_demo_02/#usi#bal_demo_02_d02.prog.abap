*&---------------------------------------------------------------------*
*& Include          /USI/BAL_DEMO_02_D02
*&---------------------------------------------------------------------*
CLASS lcl_task_grid DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: ty_selected_task_ids TYPE STANDARD TABLE OF /usi/bal_demo_to_do_task_id WITH NON-UNIQUE DEFAULT KEY,
           ty_unsorted_tasks    TYPE STANDARD TABLE OF /usi/bal_demo_to_do_task WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF user_commands,
                 refresh      TYPE ui_func VALUE 'REFRESH',
                 add_task     TYPE ui_func VALUE 'ADD_TASK',
                 edit_task    TYPE ui_func VALUE 'EDIT_TASK',
                 delete_tasks TYPE ui_func VALUE 'DELETE_TASKS',
               END   OF user_commands.

    DATA alv_grid TYPE REF TO cl_gui_alv_grid READ-ONLY.

    METHODS constructor.

    METHODS refresh_alv_grid
      IMPORTING
        i_tasks TYPE ty_unsorted_tasks.

    METHODS get_selected_task_ids
      RETURNING
        VALUE(r_result) TYPE ty_selected_task_ids.

  PRIVATE SECTION.
    DATA: custom_container  TYPE REF TO cl_gui_custom_container,
          field_catalog     TYPE lvc_t_fcat,
          layout            TYPE lvc_s_layo,
          toolbar_excluding TYPE ui_functions,
          tasks             TYPE ty_unsorted_tasks.

    METHODS get_field_catalog
      RETURNING
        VALUE(r_result) TYPE lvc_t_fcat.

    METHODS get_layout
      RETURNING
        VALUE(r_result) TYPE lvc_s_layo.

    METHODS get_toolbar_excluding
      RETURNING
        VALUE(r_result) TYPE ui_functions.

    METHODS on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        e_object.

ENDCLASS.
