FUNCTION /usi/bal_set_dc_coll_buffer.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_UNSAVED_DATA_CONT_COLLS) TYPE
*"        /USI/BAL_MSG_DATA_CONT_COLLS
*"----------------------------------------------------------------------
  IF i_unsaved_data_cont_colls IS INITIAL.
    CLEAR data_cont_coll_buffer.
  ELSE.
    data_cont_coll_buffer = NEW #( i_unsaved_data_cont_colls ).
  ENDIF.
ENDFUNCTION.
