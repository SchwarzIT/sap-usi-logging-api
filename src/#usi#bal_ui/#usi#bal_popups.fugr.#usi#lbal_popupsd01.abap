INTERFACE lif_screen_controller.
  METHODS set_status.

  METHODS display_data.

  METHODS on_exit_command
    IMPORTING
      i_exit_command TYPE syucomm.

  METHODS free.
ENDINTERFACE.
