FUNCTION-POOL /usi/bal_popups.

INCLUDE /usi/lbal_popupsd01.
INCLUDE /usi/lbal_popupsd02.
INCLUDE /usi/lbal_popupsd03.

DATA: screen_controller TYPE REF TO lif_screen_controller,
      user_command      TYPE syucomm.
