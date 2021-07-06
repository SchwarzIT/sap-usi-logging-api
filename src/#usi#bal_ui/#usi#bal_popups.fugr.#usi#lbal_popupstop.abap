FUNCTION-POOL /usi/bal_popups.              "MESSAGE-ID ..

TYPE-POOLS: abap, icon.

INCLUDE /usi/lbal_popupsd01.                " Local class (Def): LIF_SCREEN_CONTROLLER
INCLUDE /usi/lbal_popupsd02.                " Local class (Def): LCL_DATA_CONTAINER_SELECTOR
INCLUDE /usi/lbal_popupsd03.                " Local class (Def): LCL_LOG_MESSAGE_DETAIL

DATA: screen_controller TYPE REF TO lif_screen_controller,
      user_command      TYPE syucomm.
