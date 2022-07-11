*&---------------------------------------------------------------------*
*& Include          /USI/BAL_DEMO_02_P01
*&---------------------------------------------------------------------*
CLASS lcl_popup_maintain_task IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT singleton.
  ENDMETHOD.

  METHOD display.
    CLEAR screen_fields.
    CLEAR input_cancelled.

    screen_fields-task = i_old_task_data.
    CALL SCREEN 0100 STARTING AT 10 10.
  ENDMETHOD.

  METHOD on_pbo.
    SET PF-STATUS 'STATUS_0100'.
    SET TITLEBAR 'TITLE_0100'.

    hide_useless_fields( ).
  ENDMETHOD.

  METHOD hide_useless_fields.
    CONSTANTS: true  TYPE c LENGTH 1 VALUE '1',
               false TYPE c LENGTH 1 VALUE '0'.

    LOOP AT SCREEN.
      CHECK screen-group1 EQ 'ID'.

      IF screen_fields-task-id IS INITIAL.
        screen-active    = false.
        screen-invisible = true.
      ELSE.
        screen-active    = true.
        screen-invisible = false.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_exit_command.
    input_cancelled = abap_true.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD on_user_command.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD was_input_cancelled.
    r_result = input_cancelled.
  ENDMETHOD.

  METHOD get_task_text.
    r_result = screen_fields-task-text.
  ENDMETHOD.
ENDCLASS.
