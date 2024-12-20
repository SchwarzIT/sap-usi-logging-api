FUNCTION /usi/bal_shlp_exception_mapper.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     REFERENCE(SHLP) TYPE  SHLP_DESCR
*"     REFERENCE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------
  CONSTANTS exception_mapper_interface TYPE seoclsname VALUE '/USI/IF_BAL_EXCEPTION_MAPPER'.

  IF callcontrol-step <> 'SELECT'.
    RETURN.
  ENDIF.

  TRY.
      DATA(plugin_interface) = NEW lcl_plugin_interface( exception_mapper_interface ).

      record_tab[] = plugin_interface->get_implementing_classes( ).
    CATCH /usi/cx_bal_root.
      CLEAR record_tab[].
  ENDTRY.

  callcontrol-step = 'DISP'.
ENDFUNCTION.
