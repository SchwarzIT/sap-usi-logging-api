*&---------------------------------------------------------------------*
*& Include          /USI/BAL_DEMO_02_P03
*&---------------------------------------------------------------------*
CLASS lcl_main_screen IMPLEMENTATION.
  METHOD class_constructor.
    CREATE OBJECT singleton.
  ENDMETHOD.

  METHOD constructor.
    CREATE OBJECT to_do_list.
  ENDMETHOD.

  METHOD on_pbo.
    on_initialization( ).

    SET PF-STATUS 'STATUS_2000'.
    SET TITLEBAR 'TITLE_2000'.
  ENDMETHOD.

  METHOD on_initialization.
    IF task_grid IS BOUND.
      RETURN.
    ENDIF.

    /usi/cl_auth=>check_tcode( ).
    CREATE OBJECT task_grid.
    SET HANDLER on_user_command FOR task_grid->alv_grid.

    refresh_screen_without_log( ).
  ENDMETHOD.

  METHOD on_exit_command.
    LEAVE TO SCREEN 0.
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_ucomm.
      WHEN task_grid->user_commands-add_task.
        IF screen_fields-with_log EQ abap_true.
          on_add_task_with_log( ).
        ELSE.
          on_add_task_without_log( ).
        ENDIF.

      WHEN task_grid->user_commands-edit_task.
        IF screen_fields-with_log EQ abap_true.
          on_edit_task_with_log( ).
        ELSE.
          on_edit_task_without_log( ).
        ENDIF.

      WHEN task_grid->user_commands-delete_tasks.
        IF screen_fields-with_log EQ abap_true.
          on_delete_tasks_with_log( ).
        ELSE.
          on_delete_tasks_without_log( ).
        ENDIF.

      WHEN task_grid->user_commands-refresh.
        IF screen_fields-with_log EQ abap_true.
          refresh_screen_with_log( ).
        ELSE.
          refresh_screen_without_log( ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD on_add_task_with_log.
    DATA: task_text        TYPE /usi/bal_demo_to_do_task_text,
          create_exception TYPE REF TO /usi/cx_bal_demo_root.

    start_log( i_sub_object = 'CREATE_TASK' ).

    lcl_popup_maintain_task=>singleton->display( ).
    IF lcl_popup_maintain_task=>singleton->was_input_cancelled( ) EQ abap_false.
      task_text = lcl_popup_maintain_task=>singleton->get_task_text( ).
      TRY.
          to_do_list->create_task( task_text ).
        CATCH /usi/cx_bal_demo_root.
          MESSAGE create_exception TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.

    refresh_screen_without_log( ).

    save_and_destroy_log( ).
  ENDMETHOD.

  METHOD on_add_task_without_log.
    DATA: task_text        TYPE /usi/bal_demo_to_do_task_text,
          create_exception TYPE REF TO /usi/cx_bal_demo_root.

    lcl_popup_maintain_task=>singleton->display( ).
    IF lcl_popup_maintain_task=>singleton->was_input_cancelled( ) EQ abap_false.
      task_text = lcl_popup_maintain_task=>singleton->get_task_text( ).
      TRY.
          to_do_list->create_task( task_text ).
        CATCH /usi/cx_bal_demo_root.
          MESSAGE create_exception TYPE 'S' DISPLAY LIKE 'E'.
      ENDTRY.
    ENDIF.

    refresh_screen_without_log( ).
  ENDMETHOD.

  METHOD on_delete_tasks_with_log.
    start_log( i_sub_object = 'DELETE_TASKS' ).
    on_delete_tasks_without_log( ).
    save_and_destroy_log( ).
  ENDMETHOD.

  METHOD on_delete_tasks_without_log.
    DATA task_ids TYPE lcl_task_grid=>ty_selected_task_ids.
    FIELD-SYMBOLS <task_id> TYPE /usi/bal_demo_to_do_task_id.

    task_ids = task_grid->get_selected_task_ids( ).
    LOOP AT task_ids ASSIGNING <task_id>.
      TRY.
          to_do_list->delete_task( <task_id> ).
        CATCH /usi/cx_bal_demo_root.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

    refresh_screen_without_log( ).
  ENDMETHOD.

  METHOD on_edit_task_with_log.
    DATA: task_ids  TYPE lcl_task_grid=>ty_selected_task_ids,
          task      TYPE /usi/bal_demo_to_do_task,
          exception TYPE REF TO /usi/cx_bal_demo_root.

    FIELD-SYMBOLS <task_id> TYPE /usi/bal_demo_to_do_task_id.

    start_log( i_sub_object = 'UPDATE_TASK' ).

    task_ids = task_grid->get_selected_task_ids( ).
    IF lines( task_ids ) NE 1.
      log-logger->add_message( i_message_class  = '/USI/BAL_DEMO_02'
                               i_message_number = '020' ).
      save_and_destroy_log( ).
      MESSAGE s020(/usi/bal_demo_02) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    READ TABLE task_ids INDEX 1 ASSIGNING <task_id>.
    TRY.
        task = to_do_list->read_task( <task_id> ).
        lcl_popup_maintain_task=>singleton->display( task ).
        IF lcl_popup_maintain_task=>singleton->was_input_cancelled( ) EQ abap_false.
          task-text = lcl_popup_maintain_task=>singleton->get_task_text( ).
          to_do_list->update_task( task ).
        ENDIF.
      CATCH /usi/cx_bal_demo_root INTO exception.
        log-logger->add_exception( exception ).
        save_and_destroy_log( ).

        MESSAGE exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    refresh_screen_without_log( ).

    save_and_destroy_log( ).
  ENDMETHOD.

  METHOD on_edit_task_without_log.
    DATA: task_ids  TYPE lcl_task_grid=>ty_selected_task_ids,
          task      TYPE /usi/bal_demo_to_do_task,
          exception TYPE REF TO /usi/cx_bal_demo_root.

    FIELD-SYMBOLS <task_id> TYPE /usi/bal_demo_to_do_task_id.

    task_ids = task_grid->get_selected_task_ids( ).
    IF lines( task_ids ) NE 1.
      MESSAGE s020(/usi/bal_demo_02) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    READ TABLE task_ids INDEX 1 ASSIGNING <task_id>.
    TRY.
        task = to_do_list->read_task( <task_id> ).
        lcl_popup_maintain_task=>singleton->display( task ).
        IF lcl_popup_maintain_task=>singleton->was_input_cancelled( ) EQ abap_false.
          task-text = lcl_popup_maintain_task=>singleton->get_task_text( ).
          to_do_list->update_task( task ).
        ENDIF.
      CATCH /usi/cx_bal_demo_root INTO exception.
        MESSAGE exception TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

    refresh_screen_without_log( ).
  ENDMETHOD.

  METHOD refresh_screen_with_log.
    start_log( i_sub_object = 'READ_TASKS' ).
    refresh_screen_without_log( ).
    save_and_destroy_log( ).
  ENDMETHOD.

  METHOD refresh_screen_without_log.
    DATA tasks TYPE lcl_task_grid=>ty_unsorted_tasks.

    TRY.
        tasks = to_do_list->read_tasks( ).
      CATCH /usi/cx_bal_demo_root.
        CLEAR tasks.
    ENDTRY.
    task_grid->refresh_alv_grid( tasks ).
  ENDMETHOD.

  METHOD start_log.
    DATA factory TYPE REF TO /usi/if_bal_factory.

    factory    = /usi/cl_bal_factory=>get_instance( ).
    log-logger = factory->create_new_logger( i_log_object  = '/USI/BAL_DEMO_02'
                                             i_sub_object  = i_sub_object
                                             i_external_id = i_external_id ).
    log-token  = log-logger->claim_ownership( ).

    IF 1 EQ 0.
      MESSAGE i030(/usi/bal_demo_02).
    ENDIF.
    log-logger->add_message( i_message_type   = /usi/cl_bal_enum_message_type=>information
                             i_message_class  = '/USI/BAL_DEMO_02'
                             i_message_number = '030' ).
  ENDMETHOD.

  METHOD save_and_destroy_log.
    log-logger->save( log-token ).
    log-logger->free( log-token ).
  ENDMETHOD.
ENDCLASS.
