*&---------------------------------------------------------------------*
*& Include          /USI/BAL_DEMO_02_I01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*& PAI for screen 0100 (Popup 'Maintain task text')
*&---------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.
  lcl_popup_maintain_task=>singleton->on_exit_command( ).
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*& PAI for screen 0100 (Popup 'Maintain task text')
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  lcl_popup_maintain_task=>singleton->on_user_command( ).
ENDMODULE.
