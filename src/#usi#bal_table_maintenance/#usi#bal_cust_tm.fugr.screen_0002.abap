PROCESS BEFORE OUTPUT.
 MODULE LISTE_INITIALISIEREN.
 LOOP AT EXTRACT WITH CONTROL
  TCTRL_/USI/BAL_LV_USER CURSOR NEXTLINE.
   MODULE LISTE_SHOW_LISTE.
 ENDLOOP.
*
PROCESS AFTER INPUT.
 MODULE LISTE_EXIT_COMMAND AT EXIT-COMMAND.
 MODULE LISTE_BEFORE_LOOP.
 LOOP AT EXTRACT.
   MODULE LISTE_INIT_WORKAREA.
   CHAIN.
    FIELD /USI/BAL_LV_USER-UNAME .
    FIELD /USI/BAL_LV_USER-ENDDA .
    FIELD /USI/BAL_LV_USER-LOG_OBJECT .
    FIELD /USI/BAL_LV_USER-SUB_OBJECT .
    FIELD /USI/BAL_LV_USER-LOG_LEVEL .
    FIELD /USI/BAL_LV_USER-AUTO_SAVE .
    MODULE SET_UPDATE_FLAG ON CHAIN-REQUEST.
   ENDCHAIN.
   FIELD VIM_MARKED MODULE LISTE_MARK_CHECKBOX.
   CHAIN.
    FIELD /USI/BAL_LV_USER-UNAME .
    FIELD /USI/BAL_LV_USER-ENDDA .
    FIELD /USI/BAL_LV_USER-LOG_OBJECT .
    FIELD /USI/BAL_LV_USER-SUB_OBJECT .
    MODULE LISTE_UPDATE_LISTE.
   ENDCHAIN.
 ENDLOOP.
 MODULE LISTE_AFTER_LOOP.