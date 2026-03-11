*&---------------------------------------------------------------------*
*& Report  /USI/BAL_INIT_CUSTOMIZING
*& Purpose Insert/transport USI BAL customizing (objects, subobjects,
*&         exception mapping, log levels, data containers, retention)
*&
*& Handles initialization steps:
*& - https://github.com/SchwarzIT/sap-usi-logging-api/wiki/Create-Log-Objects
*& - https://github.com/SchwarzIT/sap-usi-logging-api/wiki/Maintain-Basic-Customizing
*&---------------------------------------------------------------------*
REPORT /usi/bal_init_customizing.

*************************************************************************
*   WARNING: action only allowed on development systems
*************************************************************************
DATA: lv_client_role      TYPE t000-cccategory.
CALL FUNCTION 'TR_SYS_PARAMS'
  IMPORTING
    system_client_role = lv_client_role
  EXCEPTIONS
    OTHERS             = 3.
IF sy-subrc NE 0 OR lv_client_role NE 'C'.
  MESSAGE 'WARNING: action only allowed on development systems' TYPE 'S' DISPLAY LIKE 'E'.
  RETURN.
ENDIF.
*************************************************************************

"----------------------------------------------------------------------
" Transport: choose order/task and prepare buffers
"----------------------------------------------------------------------
DATA: lt_e071  TYPE STANDARD TABLE OF e071,
      lt_e071k TYPE STANDARD TABLE OF e071k,
      lv_order TYPE tr_trkorr,
      lv_task  TYPE tr_trkorr.

CALL FUNCTION 'TRINT_ORDER_CHOICE'
  IMPORTING
    we_order               = lv_order
    we_task                = lv_task
  TABLES
    wt_e071                = lt_e071
    wt_e071k               = lt_e071k
  EXCEPTIONS
    no_correction_selected = 1
    display_mode           = 2
    object_append_error    = 3
    recursive_call         = 4
    wrong_order_type       = 5
    OTHERS                 = 6.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  RETURN.
ENDIF.

"----------------------------------------------------------------------
" BAL: Log objects & subobjects
"----------------------------------------------------------------------
DATA: lt_log_objects   TYPE STANDARD TABLE OF balobj,
      lt_log_objects_t TYPE STANDARD TABLE OF balobjt,
      lt_sub_objects   TYPE STANDARD TABLE OF balsub,
      lt_sub_objects_t TYPE STANDARD TABLE OF balsubt.

lt_log_objects_t = VALUE #(
  ( spras = 'E' object = '/USI/BAL'         objtxt = 'Logging-API'               )
  ( spras = 'E' object = '/USI/BAL_DEMO_01' objtxt = 'Logging API: Simple Demo'  )
  ( spras = 'E' object = '/USI/BAL_DEMO_02' objtxt = 'Logging API: Complex Demo' )
).
MOVE-CORRESPONDING lt_log_objects_t TO lt_log_objects.

lt_sub_objects_t = VALUE #(
  ( spras = 'E' object = '/USI/BAL'         subobject = 'WRONG_API_CALLS' subobjtxt = 'Wrong API Calls'               )
  ( spras = 'E' object = '/USI/BAL_DEMO_01' subobject = 'RUN'             subobjtxt = 'Execute the demo'              )
  ( spras = 'E' object = '/USI/BAL_DEMO_02' subobject = 'CREATE_TASK'     subobjtxt = 'Create Task'                   )
  ( spras = 'E' object = '/USI/BAL_DEMO_02' subobject = 'DELETE_TASK'     subobjtxt = 'Delete single Task'            )
  ( spras = 'E' object = '/USI/BAL_DEMO_02' subobject = 'DELETE_TASKS'    subobjtxt = 'Delete multiple tasks at once' )
  ( spras = 'E' object = '/USI/BAL_DEMO_02' subobject = 'READ_TASK'       subobjtxt = 'Read Task'                     )
  ( spras = 'E' object = '/USI/BAL_DEMO_02' subobject = 'READ_TASKS'      subobjtxt = 'Read Tasks'                    )
  ( spras = 'E' object = '/USI/BAL_DEMO_02' subobject = 'UPDATE_TASK'     subobjtxt = 'Update Task'                   )
).
MOVE-CORRESPONDING lt_sub_objects_t TO lt_sub_objects.

INSERT balobj   FROM TABLE @lt_log_objects   ACCEPTING DUPLICATE KEYS.
INSERT balobjt  FROM TABLE @lt_log_objects_t ACCEPTING DUPLICATE KEYS.
INSERT balsub   FROM TABLE @lt_sub_objects   ACCEPTING DUPLICATE KEYS.
INSERT balsubt  FROM TABLE @lt_sub_objects_t ACCEPTING DUPLICATE KEYS.

"----------------------------------------------------------------------
" BAL: Exception mapper (/USI/BAL_CX_MAP)
"----------------------------------------------------------------------
DATA lt_cx_map TYPE STANDARD TABLE OF /usi/bal_cx_map.

lt_cx_map = VALUE #(
  ( exception_class = 'IF_MESSAGE' mapper_class = '/USI/CL_BAL_EM_BASE' )
).
INSERT /usi/bal_cx_map FROM TABLE @lt_cx_map ACCEPTING DUPLICATE KEYS.

"----------------------------------------------------------------------
" BAL: Log levels (product specific), data containers, retention time
"----------------------------------------------------------------------
DATA: lt_lv_lobj TYPE STANDARD TABLE OF /usi/bal_lv_lobj,
      lt_lv_data TYPE STANDARD TABLE OF /usi/bal_lv_data,
      lt_lv_rtim TYPE STANDARD TABLE OF /usi/bal_lv_rtim.

" /USI/BAL_LV_LOBJ
lt_lv_lobj = VALUE #(
  ( log_object = '/USI/BAL'         sub_object = 'WRONG_API_CALLS' log_level = 0 auto_save_package_size = 0 )
  ( log_object = '/USI/BAL_DEMO_01' sub_object = ''                log_level = 0 auto_save_package_size = 0 )
  ( log_object = '/USI/BAL_DEMO_02' sub_object = ''                log_level = 0 auto_save_package_size = 0 )
).
INSERT /usi/bal_lv_lobj FROM TABLE @lt_lv_lobj ACCEPTING DUPLICATE KEYS.

" /USI/BAL_LV_DATA
lt_lv_data = VALUE #(
  ( classname = '/USI/CL_BAL_DC_RETCODE_AND_MSG' min_log_level = 1 )
  ( classname = '/USI/CL_BAL_DC_SRC_POS_CALLER'  min_log_level = 1 )
  ( classname = '/USI/CL_BAL_DC_SRC_POS_CX'      min_log_level = 1 )
  ( classname = '/USI/CL_BAL_DC_CALLSTACK'       min_log_level = 6 )
  ( classname = '/USI/CL_BAL_DC_HTML'            min_log_level = 6 )
  ( classname = '/USI/CL_BAL_DC_ITAB'            min_log_level = 6 )
  ( classname = '/USI/CL_BAL_DC_JSON'            min_log_level = 6 )
  ( classname = '/USI/CL_BAL_DC_STRUCTURE'       min_log_level = 6 )
  ( classname = '/USI/CL_BAL_DC_XML'             min_log_level = 6 )
).
INSERT /usi/bal_lv_data FROM TABLE @lt_lv_data ACCEPTING DUPLICATE KEYS.

" /USI/BAL_LV_RTIM
lt_lv_rtim = VALUE #(
  ( log_level = 1 retention_time = 14 )
  ( log_level = 2 retention_time = 14 )
  ( log_level = 3 retention_time = 14 )
  ( log_level = 4 retention_time = 14 )
  ( log_level = 5 retention_time = 14 )
  ( log_level = 6 retention_time = 7  )
).
INSERT /usi/bal_lv_rtim FROM TABLE @lt_lv_rtim ACCEPTING DUPLICATE KEYS.

" Persist DB changes before collecting transport keys
COMMIT WORK.

"----------------------------------------------------------------------
" Transport objects: BAL tables + USI customizing
"----------------------------------------------------------------------
CLEAR: lt_e071, lt_e071k.

" Header entries (E071)
APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = 'BALOBJ'           objfunc = 'K' ) TO lt_e071.
APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = 'BALOBJT'          objfunc = 'K' ) TO lt_e071.
APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = 'BALSUB'           objfunc = 'K' ) TO lt_e071.
APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = 'BALSUBT'          objfunc = 'K' ) TO lt_e071.
APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = '/USI/BAL_CX_MAP'  objfunc = 'K' ) TO lt_e071.
APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = '/USI/BAL_LV_LOBJ' objfunc = 'K' ) TO lt_e071.
APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = '/USI/BAL_LV_DATA' objfunc = 'K' ) TO lt_e071.
APPEND VALUE #( pgmid = 'R3TR' object = 'TABU' obj_name = '/USI/BAL_LV_RTIM' objfunc = 'K' ) TO lt_e071.

" Key entries (E071K)
APPEND VALUE #(
  pgmid      = 'R3TR' object = 'TABU' objname = 'BALOBJ'
  mastertype = 'TABU' mastername = 'BALOBJ' tabkey = '/USI/*'
) TO lt_e071k.

APPEND VALUE #(
  pgmid      = 'R3TR' object = 'TABU' objname = 'BALOBJT'
  mastertype = 'TABU' mastername = 'BALOBJT' tabkey = 'E/USI/*'
) TO lt_e071k.

APPEND VALUE #(
  pgmid      = 'R3TR' object = 'TABU' objname = 'BALSUB'
  mastertype = 'TABU' mastername = 'BALSUB' tabkey = '/USI/*'
) TO lt_e071k.

APPEND VALUE #(
  pgmid      = 'R3TR' object = 'TABU' objname = 'BALSUBT'
  mastertype = 'TABU' mastername = 'BALSUBT' tabkey = 'E/USI/*'
) TO lt_e071k.

APPEND VALUE #(
  pgmid      = 'R3TR' object = 'TABU' objname = '/USI/BAL_CX_MAP'
  mastertype = 'TABU' mastername = '/USI/BAL_CX_MAP' tabkey = 'IF_MESSAGE'
) TO lt_e071k.

APPEND VALUE #(
  pgmid      = 'R3TR' object = 'TABU' objname = '/USI/BAL_LV_LOBJ'
  mastertype = 'TABU' mastername = '/USI/BAL_LV_LOBJ' tabkey = '/USI/*'
) TO lt_e071k.

APPEND VALUE #(
  pgmid      = 'R3TR' object = 'TABU' objname = '/USI/BAL_LV_DATA'
  mastertype = 'TABU' mastername = '/USI/BAL_LV_DATA' tabkey = '/USI/*'
) TO lt_e071k.

APPEND VALUE #(
  pgmid      = 'R3TR' object = 'TABU' objname = '/USI/BAL_LV_RTIM'
  mastertype = 'TABU' mastername = '/USI/BAL_LV_RTIM' tabkey = '*'
) TO lt_e071k.

" Append all collected objects/keys to the selected task
CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
  EXPORTING
    wi_trkorr = lv_task
  TABLES
    wt_e071   = lt_e071
    wt_e071k  = lt_e071k
  EXCEPTIONS
    OTHERS    = 68.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.
