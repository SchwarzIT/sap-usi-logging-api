CLASS /usi/cl_bal_demo_task_dao_fake DEFINITION PUBLIC FINAL CREATE PRIVATE.
  PUBLIC SECTION.

    "! <h1>Factory method</h1>
    "!
    "! @parameter r_result | The singleton
    CLASS-METHODS get_instance
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/cl_bal_demo_task_dao_fake.

    "! <h1>Constructor</h1>
    "!
    "! <p>Creates Fake-DAO-Object and sets fake-data internally.</p>
    METHODS constructor.

    "! <h1>Insert task into buffer (NO DB-Update)</h1>
    "!
    "! @parameter i_task | new task
    "! @raising /usi/cx_bal_demo_root | ID already in use
    METHODS create_task
      IMPORTING
        i_task TYPE /usi/bal_demo_to_do_task
      RAISING
        /usi/cx_bal_demo_root.

    "! <h1>Read task from buffer</h1>
    "!
    "! @parameter i_task_id | ID of to-be-read task
    "! @parameter r_result | Task data
    "! @raising /usi/cx_bal_demo_root | Not found
    METHODS read_task
      IMPORTING
        i_task_id       TYPE /usi/bal_demo_to_do_task_id
      RETURNING
        VALUE(r_result) TYPE /usi/bal_demo_to_do_task
      RAISING
        /usi/cx_bal_demo_root.

    "! <h1>Read tasks from buffer</h1>
    "!
    "! @parameter r_result | Current buffer content
    METHODS read_tasks
      RETURNING
        VALUE(r_result) TYPE /usi/bal_demo_to_do_tasks.

    "! <h1>Update task in buffer (NO DB-Update)</h1>
    "!
    "! @parameter i_task | to-be-updated task
    "! @raising /usi/cx_bal_demo_root | Not found
    METHODS update_task
      IMPORTING
        i_task TYPE /usi/bal_demo_to_do_task
      RAISING
        /usi/cx_bal_demo_root.

    "! <h1>Delete task from buffer (NO DB-Update)</h1>
    "!
    "! @parameter i_task_id | Task-ID
    "! @raising /usi/cx_bal_demo_root | Not found
    METHODS delete_task
      IMPORTING
        i_task_id TYPE /usi/bal_demo_to_do_task_id
      RAISING
        /usi/cx_bal_demo_root.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA singleton TYPE REF TO /usi/cl_bal_demo_task_dao_fake.

    DATA: tasks TYPE /usi/bal_demo_to_do_tasks.

    METHODS set_fake_data.

    METHODS log_exception
      IMPORTING
        i_exception TYPE REF TO cx_root.

    METHODS log_info_message
      IMPORTING
        i_message_number     TYPE symsgno
        i_message_variable_1 TYPE simple OPTIONAL
        i_message_variable_2 TYPE simple OPTIONAL
        i_message_variable_3 TYPE simple OPTIONAL
        i_message_variable_4 TYPE simple OPTIONAL.

    METHODS get_logger
      RETURNING
        VALUE(r_result) TYPE REF TO /usi/if_bal_logger.

ENDCLASS.



CLASS /usi/cl_bal_demo_task_dao_fake IMPLEMENTATION.
  METHOD get_instance.
    IF singleton IS NOT BOUND.
      CREATE OBJECT singleton.
    ENDIF.

    r_result = singleton.
  ENDMETHOD.

  METHOD constructor.
    set_fake_data( ).
  ENDMETHOD.

  METHOD set_fake_data.
    DATA: task TYPE /usi/bal_demo_to_do_task.

    task-id   = lines( tasks ) + 1.
    task-text = 'Add simple demo'.
    INSERT task INTO TABLE tasks.

    task-id   = lines( tasks ) + 1.
    task-text = 'Add complex demo'.
    INSERT task INTO TABLE tasks.

    task-id   = lines( tasks ) + 1.
    task-text = 'Fix issue #1234'.
    INSERT task INTO TABLE tasks.
  ENDMETHOD.

  METHOD create_task.
    DATA: message_variable_1   TYPE symsgv,
          propagated_exception TYPE REF TO cx_root.

    INSERT i_task INTO TABLE tasks.
    IF sy-subrc NE 0.
      IF 1 EQ 0.
        MESSAGE e010(/usi/bal_demo_02) WITH i_task-id.
      ENDIF.
      TRY.
          WRITE i_task-id TO message_variable_1 LEFT-JUSTIFIED.

          RAISE EXCEPTION TYPE /usi/cx_bal_demo_duplicate
            EXPORTING
              textid = /usi/cx_bal_demo_duplicate=>duplicate_task_id
              param1 = message_variable_1.
        CLEANUP INTO propagated_exception.
          log_exception( propagated_exception ).
      ENDTRY.

    ELSE.
      IF 1 EQ 0.
        MESSAGE i011(/usi/bal_demo_02) WITH i_task-id.
      ENDIF.
      log_info_message( i_message_number     = '011'
                        i_message_variable_1 = i_task-id ).

    ENDIF.
  ENDMETHOD.

  METHOD read_task.
    DATA: message_variable_1   TYPE symsgv,
          propagated_exception TYPE REF TO cx_root.

    READ TABLE tasks INTO r_result WITH TABLE KEY id = i_task_id.

    IF sy-subrc NE 0.
      IF 1 EQ 0.
        MESSAGE e014(/usi/bal_demo_02) WITH i_task_id.
      ENDIF.
      TRY.
          WRITE i_task_id TO message_variable_1 LEFT-JUSTIFIED.

          RAISE EXCEPTION TYPE /usi/cx_bal_demo_not_found
            EXPORTING
              textid = /usi/cx_bal_demo_not_found=>task_not_found_by_id
              param1 = message_variable_1.
        CLEANUP INTO propagated_exception.
          log_exception( propagated_exception ).
      ENDTRY.

    ELSE.
      IF 1 EQ 0.
        MESSAGE i017(/usi/bal_demo_02) WITH i_task_id.
      ENDIF.
      log_info_message( i_message_number     = '017'
                        i_message_variable_1 = i_task_id ).

    ENDIF.
  ENDMETHOD.

  METHOD read_tasks.
    r_result = tasks.
  ENDMETHOD.

  METHOD update_task.
    DATA: task_ref             TYPE REF TO /usi/bal_demo_to_do_task,
          message_variable_1   TYPE symsgv,
          message_variable_2   TYPE symsgv,
          propagated_exception TYPE REF TO cx_root.

    READ TABLE tasks WITH TABLE KEY id = i_task-id REFERENCE INTO task_ref.

    IF sy-subrc NE 0.
      IF 1 EQ 0.
        MESSAGE e014(/usi/bal_demo_02) WITH i_task-id.
      ENDIF.
      TRY.
          WRITE i_task-id TO message_variable_1 LEFT-JUSTIFIED.

          RAISE EXCEPTION TYPE /usi/cx_bal_demo_not_found
            EXPORTING
              textid = /usi/cx_bal_demo_not_found=>task_not_found_by_id
              param1 = message_variable_1.
        CLEANUP INTO propagated_exception.
          log_exception( propagated_exception ).
      ENDTRY.

    ELSE.
      task_ref->text = i_task-text.

      IF 1 EQ 0.
        MESSAGE i016(/usi/bal_demo_02) WITH i_task-id.
      ENDIF.
      log_info_message( i_message_number     = '016'
                        i_message_variable_1 = i_task-id ).

    ENDIF.
  ENDMETHOD.

  METHOD delete_task.
    DATA: message_variable_1   TYPE symsgv,
          message_variable_2   TYPE symsgv,
          propagated_exception TYPE REF TO cx_root.

    DELETE TABLE tasks WITH TABLE KEY id = i_task_id.

    IF sy-subrc NE 0.
      IF 1 EQ 0.
        MESSAGE e014(/usi/bal_demo_02) WITH i_task_id.
      ENDIF.
      TRY.
          WRITE i_task_id TO message_variable_1 LEFT-JUSTIFIED.

          RAISE EXCEPTION TYPE /usi/cx_bal_demo_not_found
            EXPORTING
              textid = /usi/cx_bal_demo_not_found=>task_not_found_by_id
              param1 = message_variable_1.
        CLEANUP INTO propagated_exception.
          log_exception( propagated_exception ).
      ENDTRY.

    ELSE.
      IF 1 EQ 0.
        MESSAGE i015(/usi/bal_demo_02) WITH i_task_id.
      ENDIF.
      log_info_message( i_message_number     = '015'
                        i_message_variable_1 = i_task_id ).

    ENDIF.
  ENDMETHOD.

  METHOD log_exception.
    DATA: logger  TYPE REF TO /usi/if_bal_logger.

    logger = get_logger( ).
    logger->add_exception( i_exception ).
  ENDMETHOD.

  METHOD log_info_message.
    DATA: logger  TYPE REF TO /usi/if_bal_logger.

    logger = get_logger( ).
    logger->add_message( i_message_type       = /usi/cl_bal_enum_message_type=>information
                         i_message_class      = '/USI/BAL_DEMO_02'
                         i_message_number     = i_message_number
                         i_message_variable_1 = i_message_variable_1
                         i_message_variable_2 = i_message_variable_2
                         i_message_variable_3 = i_message_variable_3
                         i_message_variable_4 = i_message_variable_4 ).
  ENDMETHOD.

  METHOD get_logger.
    DATA: factory TYPE REF TO /usi/if_bal_factory.

    " The logger was created by some object, that called us.
    "   => We can just get the existing logger
    "
    " We could also call the create-method and it would return the singleton,
    " but that method requires at least a log object, that would be ignored
    " anyway.
    factory = /usi/cl_bal_factory=>get_instance( ).
    r_result = factory->get_existing_logger( ).
  ENDMETHOD.
ENDCLASS.
