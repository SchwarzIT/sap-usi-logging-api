*&---------------------------------------------------------------------*
*& Include          /USI/BAL_DEMO_02_D03
*&---------------------------------------------------------------------*
CLASS lcl_main_screen DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-DATA: singleton TYPE REF TO lcl_main_screen READ-ONLY.

    DATA: BEGIN OF screen_fields,
            with_log     TYPE abap_bool,
            user_command TYPE syucomm,
          END   OF screen_fields.

    CLASS-METHODS class_constructor.

    METHODS constructor.

    METHODS on_pbo.

    METHODS on_exit_command.

  PRIVATE SECTION.
    DATA: to_do_list TYPE REF TO /usi/cl_bal_demo_to_do_list,
          task_grid  TYPE REF TO lcl_task_grid.

    DATA: BEGIN OF log,
            logger TYPE REF TO /usi/if_bal_logger,
            token  TYPE REF TO /usi/if_bal_token,
          END   OF log.

    METHODS on_initialization.

    METHODS on_add_task_with_log.

    METHODS on_add_task_without_log.

    METHODS on_edit_task_with_log.

    METHODS on_edit_task_without_log.

    METHODS on_delete_tasks_with_log.

    METHODS on_delete_tasks_without_log.

    METHODS refresh_screen_with_log.

    METHODS refresh_screen_without_log.

    METHODS on_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        e_ucomm.

    METHODS start_log
      IMPORTING
        i_sub_object  TYPE balsubobj
        i_external_id TYPE balnrext OPTIONAL.

    METHODS save_and_destroy_log.

ENDCLASS.
