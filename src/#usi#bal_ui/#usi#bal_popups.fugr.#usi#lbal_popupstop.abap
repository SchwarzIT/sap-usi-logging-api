FUNCTION-POOL /usi/bal_popups.

INCLUDE /usi/lbal_popupsd01.
INCLUDE /usi/lbal_popupsd02.
INCLUDE /usi/lbal_popupsd03.
INCLUDE /usi/lbal_popupsd10.

DATA: screen_controller TYPE REF TO lif_screen_controller,
      user_command      TYPE syucomm.

DATA data_cont_coll_buffer TYPE REF TO lcl_data_cont_coll_buffer.
