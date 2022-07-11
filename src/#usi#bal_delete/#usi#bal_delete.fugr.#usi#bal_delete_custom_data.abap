FUNCTION /usi/bal_delete_custom_data.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_LOG_HEADERS) TYPE  BALHDR_T
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_log_header_group,
           object      TYPE balobj_d,
           subobject   TYPE balsubobj,
           log_headers TYPE balhdr_t,
         END   OF ty_log_header_group,
         ty_log_header_groups TYPE HASHED TABLE OF ty_log_header_group WITH UNIQUE KEY object subobject.

  DATA: badi              TYPE REF TO /usi/bal_delete_custom_data,
        log_header_groups TYPE ty_log_header_groups,
        log_header_group  TYPE REF TO ty_log_header_group.

  FIELD-SYMBOLS: <log_header> TYPE balhdr.

  LOOP AT i_log_headers ASSIGNING <log_header>.
    READ TABLE log_header_groups
      WITH TABLE KEY object    = <log_header>-object
                     subobject = <log_header>-subobject
      REFERENCE INTO log_header_group.

    IF sy-subrc NE 0.
      CREATE DATA log_header_group.
      log_header_group->object    = <log_header>-object.
      log_header_group->subobject = <log_header>-subobject.
      INSERT log_header_group->* INTO TABLE log_header_groups REFERENCE INTO log_header_group.
    ENDIF.

    INSERT <log_header> INTO TABLE log_header_group->log_headers.
  ENDLOOP.

  LOOP AT log_header_groups REFERENCE INTO log_header_group.
    GET BADI badi
      FILTERS
        object    = log_header_group->object
        subobject = log_header_group->subobject.

    CALL BADI badi->delete_custom_data
      EXPORTING
        i_log_headers = log_header_group->log_headers.
  ENDLOOP.

ENDFUNCTION.
