PROCESS BEFORE OUTPUT.
  MODULE pbo_2000.

PROCESS AFTER INPUT.
  FIELD lcl_main_screen=>singleton->screen_fields-with_log.

  MODULE exit_command_2000 AT EXIT-COMMAND.
