CLASS /usi/cl_bal_demo_to_do_list DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    "! <h1>Create Instance with fake data</h1>
    METHODS constructor.

    "! <h1>Create task (In buffer; No DB-Update)</h1>
    "!
    "! @parameter i_task_text | Task text
    "! @raising /usi/cx_bal_demo_root | Task with same text already exists
    METHODS create_task
      IMPORTING
        i_task_text TYPE /usi/bal_demo_to_do_task_text
      RAISING
        /usi/cx_bal_demo_root.

    "! <h1>Read task from buffer</h1>
    "!
    "! @parameter i_task_id | Task id
    "! @raising /usi/cx_bal_demo_root | Not found
    METHODS read_task
      IMPORTING
        i_task_id       TYPE /usi/bal_demo_to_do_task_id
      RETURNING
        VALUE(r_result) TYPE /usi/bal_demo_to_do_task
      RAISING
        /usi/cx_bal_demo_root.

    "! <h1>Read all tasks from buffer</h1>
    "!
    "! @raising /usi/cx_bal_demo_root | Current buffer content
    METHODS read_tasks
      RETURNING
        VALUE(r_result) TYPE /usi/bal_demo_to_do_tasks
      RAISING
        /usi/cx_bal_demo_root.

    "! <h1>Update task in buffer</h1>
    "!
    "! @parameter i_task | Task
    "! @raising /usi/cx_bal_demo_root | Not found
    METHODS update_task
      IMPORTING
        i_task TYPE /usi/bal_demo_to_do_task
      RAISING
        /usi/cx_bal_demo_root.

    "! <h1>Delete task from buffer</h1>
    "!
    "! @parameter i_task_id | Task ID
    "! @raising /usi/cx_bal_demo_root | Not found
    METHODS delete_task
      IMPORTING
        i_task_id TYPE /usi/bal_demo_to_do_task_id
      RAISING
        /usi/cx_bal_demo_root.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: BEGIN OF to_do_list,
            dao_object      TYPE REF TO /usi/cl_bal_demo_task_dao_fake,
            highest_task_id TYPE /usi/bal_demo_to_do_task_id,
          END OF to_do_list.

    DATA: BEGIN OF log,
            logger TYPE REF TO /usi/if_bal_logger,
            token  TYPE REF TO /usi/if_bal_token,
          END   OF log.

    METHODS create_itab_container
      IMPORTING
        i_table          TYPE ANY TABLE
        i_title_text_key TYPE /usi/cl_bal_tc_report_text_c40=>ty_text_key
        i_title_text     TYPE csequence
      RETURNING
        VALUE(r_result)  TYPE REF TO /usi/cl_bal_dc_itab.

    METHODS get_tasks_by_text
      IMPORTING
        i_task_text     TYPE /usi/bal_demo_to_do_task_text
      RETURNING
        VALUE(r_result) TYPE /usi/bal_demo_to_do_tasks
      RAISING
        /usi/cx_bal_demo_root.

    METHODS log_info_message
      IMPORTING
        i_message_number     TYPE symsgno
        i_message_variable_1 TYPE simple OPTIONAL
        i_message_variable_2 TYPE simple OPTIONAL
        i_message_variable_3 TYPE simple OPTIONAL
        i_message_variable_4 TYPE simple OPTIONAL
        i_details            TYPE REF TO /usi/if_bal_message_details OPTIONAL.

    METHODS start_log
      IMPORTING
        i_sub_object  TYPE balsubobj
        i_external_id TYPE balnrext OPTIONAL.

    METHODS save_and_destroy_log.

ENDCLASS.



CLASS /usi/cl_bal_demo_to_do_list IMPLEMENTATION.


  METHOD constructor.
    DATA: tasks              TYPE /usi/bal_demo_to_do_tasks,
          index_of_last_task TYPE i.

    FIELD-SYMBOLS <last_task> TYPE /usi/bal_demo_to_do_task.

    to_do_list-dao_object = /usi/cl_bal_demo_task_dao_fake=>get_instance( ).

    tasks  = to_do_list-dao_object->read_tasks( ).
    IF tasks IS NOT INITIAL.
      index_of_last_task = lines( tasks ).
      READ TABLE tasks INDEX index_of_last_task ASSIGNING <last_task>.
      to_do_list-highest_task_id = <last_task>-id.
    ENDIF.
  ENDMETHOD.


  METHOD create_itab_container.
    DATA title_text_container TYPE REF TO /usi/cl_bal_tc_report_text_c40.

    title_text_container = /usi/cl_bal_tc_report_text_c40=>create( i_text_key = i_title_text_key
                                                                   i_text     = i_title_text ).

    CREATE OBJECT r_result
      EXPORTING
        i_internal_table = i_table
        i_title          = title_text_container.
  ENDMETHOD.


  METHOD create_task.
    DATA: duplicates              TYPE /usi/bal_demo_to_do_tasks,
          duplicates_container    TYPE REF TO /usi/cl_bal_dc_itab,
          message_variable        TYPE symsgv,
          new_task                TYPE /usi/bal_demo_to_do_task,
          no_duplicates_exception TYPE REF TO /usi/cx_bal_demo_root,
          propagated_exception    TYPE REF TO cx_root.

    start_log( 'CREATE_TASK' ).

    TRY.
        duplicates = get_tasks_by_text( i_task_text ).

        " We already had that text => Reject the duplicate!
        IF 1 EQ 0.
          MESSAGE e004(/usi/bal_demo_02) WITH i_task_text.
        ENDIF.
        TRY.
            message_variable = i_task_text.
            RAISE EXCEPTION TYPE /usi/cx_bal_demo_duplicate
              EXPORTING
                textid = /usi/cx_bal_demo_duplicate=>duplicate_task_text
                param1 = message_variable.
          CLEANUP INTO propagated_exception.
            duplicates_container = create_itab_container( i_table          = duplicates
                                                          i_title_text_key = 'T02'
                                                          i_title_text     = TEXT-t02 ).
            log-logger->add_exception( i_problem_class = /usi/cl_bal_enum_problem_class=>very_important
                                       i_exception     = propagated_exception
                                       i_details       = duplicates_container ).
            save_and_destroy_log( ).
        ENDTRY.

      CATCH /usi/cx_bal_demo_root INTO no_duplicates_exception.

        " We did not have that text so far => Add it
        to_do_list-highest_task_id = to_do_list-highest_task_id + 1.
        new_task-id   = to_do_list-highest_task_id.
        new_task-text = i_task_text.

        to_do_list-dao_object->create_task( new_task ).

        IF 1 EQ 0.
          MESSAGE i003(/usi/bal_demo_02) WITH new_task-text new_task-id.
        ENDIF.
        log_info_message( i_message_number     = '003'
                          i_message_variable_1 = new_task-text
                          i_message_variable_2 = new_task-id ).

        save_and_destroy_log( ).

    ENDTRY.
  ENDMETHOD.


  METHOD delete_task.
    DATA external_id TYPE balnrext.

    external_id = i_task_id.
    start_log( i_sub_object  = 'DELETE_TASK'
               i_external_id = external_id ).

    to_do_list-dao_object->delete_task( i_task_id ).

    save_and_destroy_log( ).
  ENDMETHOD.


  METHOD get_tasks_by_text.
    DATA: tasks                TYPE /usi/bal_demo_to_do_tasks,
          message_variable_1   TYPE symsgv,
          propagated_exception TYPE REF TO cx_root.

    FIELD-SYMBOLS <task> TYPE /usi/bal_demo_to_do_task.

    tasks = to_do_list-dao_object->read_tasks( ).
    LOOP AT tasks ASSIGNING <task> WHERE text EQ i_task_text.
      INSERT <task> INTO TABLE r_result.
    ENDLOOP.

    IF r_result IS INITIAL.
      IF 1 EQ 0.
        MESSAGE i012(/usi/bal_demo_02) WITH i_task_text.
      ENDIF.
      TRY.
          message_variable_1 = i_task_text.
          RAISE EXCEPTION TYPE /usi/cx_bal_demo_not_found
            EXPORTING
              textid = /usi/cx_bal_demo_not_found=>task_not_found_by_text
              param1 = message_variable_1.
        CLEANUP INTO propagated_exception.
          log-logger->add_exception( i_message_type = /usi/cl_bal_enum_message_type=>information
                                     i_exception    = propagated_exception ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD log_info_message.
    log-logger->add_message( i_message_type       = /usi/cl_bal_enum_message_type=>information
                             i_message_class      = '/USI/BAL_DEMO_02'
                             i_message_number     = i_message_number
                             i_message_variable_1 = i_message_variable_1
                             i_message_variable_2 = i_message_variable_2
                             i_message_variable_3 = i_message_variable_3
                             i_message_variable_4 = i_message_variable_4
                             i_details            = i_details ).
  ENDMETHOD.


  METHOD read_task.
    start_log( 'READ_TASK' ).

    r_result = to_do_list-dao_object->read_task( i_task_id ).

    save_and_destroy_log( ).
  ENDMETHOD.


  METHOD read_tasks.
    DATA: line_count           TYPE i,
          message_variable_1   TYPE symsgv,
          propagated_exception TYPE REF TO cx_root,
          table_container      TYPE REF TO /usi/cl_bal_dc_itab.

    start_log( 'READ_TASKS' ).

    r_result = to_do_list-dao_object->read_tasks( ).
    IF r_result IS NOT INITIAL.
      line_count      = lines( r_result ).
      table_container = create_itab_container( i_table          = r_result
                                               i_title_text_key = 'T01'
                                               i_title_text     = TEXT-t01 ).

      IF 1 EQ 0.
        MESSAGE i002(/usi/bal_demo_02) WITH line_count.
      ENDIF.
      log_info_message( i_message_number     = '002'
                        i_message_variable_1 = line_count
                        i_details            = table_container ).

      save_and_destroy_log( ).

    ELSE.
      IF 1 EQ 0.
        MESSAGE s013(/usi/bal_demo_02).
      ENDIF.
      TRY.
          RAISE EXCEPTION TYPE /usi/cx_bal_demo_not_found
            EXPORTING
              textid = /usi/cx_bal_demo_not_found=>no_tasks_found.
        CLEANUP INTO propagated_exception.
          log-logger->add_exception( propagated_exception ).
          save_and_destroy_log( ).
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD save_and_destroy_log.
    log-logger->save( log-token ).
    log-logger->free( log-token ).
  ENDMETHOD.


  METHOD start_log.
    DATA factory TYPE REF TO /usi/if_bal_factory.

    factory    = /usi/cl_bal_factory=>get_instance( ).
    log-logger = factory->create_new_logger( i_log_object  = '/USI/BAL_DEMO_02'
                                             i_sub_object  = i_sub_object
                                             i_external_id = i_external_id ).
    log-token  = log-logger->claim_ownership( ).
  ENDMETHOD.


  METHOD update_task.
    start_log( 'UPDATE_TASK' ).

    to_do_list-dao_object->update_task( i_task ).

    save_and_destroy_log( ).
  ENDMETHOD.
ENDCLASS.
