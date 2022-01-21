*&---------------------------------------------------------------------*
*& Include          /USI/BAL_DEMO_02_D01
*&---------------------------------------------------------------------*
CLASS lcl_popup_maintain_task DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.

    CLASS-DATA: singleton TYPE REF TO lcl_popup_maintain_task.

    DATA: BEGIN OF screen_fields,
            user_command TYPE syucomm,
            task         TYPE /usi/bal_demo_to_do_task,
          END   OF screen_fields.

    CLASS-METHODS class_constructor.

    METHODS display
      IMPORTING
        i_old_task_data TYPE /usi/bal_demo_to_do_task OPTIONAL.

    METHODS on_pbo.

    METHODS on_exit_command.

    METHODS on_user_command.

    METHODS was_input_cancelled
      RETURNING
        VALUE(r_result) TYPE abap_bool.

    METHODS get_task_text
      RETURNING
        VALUE(r_result) TYPE /usi/bal_demo_to_do_task_text.

  PRIVATE SECTION.
    DATA: input_cancelled TYPE abap_bool.

    METHODS hide_useless_fields.

ENDCLASS.
