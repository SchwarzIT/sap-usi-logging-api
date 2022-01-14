*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 25.11.2020 at 10:57:46
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: /USI/BAL_CX_MAP.................................*
DATA:  BEGIN OF STATUS_/USI/BAL_CX_MAP               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/USI/BAL_CX_MAP               .
CONTROLS: TCTRL_/USI/BAL_CX_MAP
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: /USI/BAL_LV_DATA................................*
DATA:  BEGIN OF STATUS_/USI/BAL_LV_DATA              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/USI/BAL_LV_DATA              .
CONTROLS: TCTRL_/USI/BAL_LV_DATA
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: /USI/BAL_LV_LOBJ................................*
DATA:  BEGIN OF STATUS_/USI/BAL_LV_LOBJ              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/USI/BAL_LV_LOBJ              .
CONTROLS: TCTRL_/USI/BAL_LV_LOBJ
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: /USI/BAL_LV_RTIM................................*
DATA:  BEGIN OF STATUS_/USI/BAL_LV_RTIM              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/USI/BAL_LV_RTIM              .
CONTROLS: TCTRL_/USI/BAL_LV_RTIM
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: /USI/BAL_LV_USER................................*
DATA:  BEGIN OF STATUS_/USI/BAL_LV_USER              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/USI/BAL_LV_USER              .
CONTROLS: TCTRL_/USI/BAL_LV_USER
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: */USI/BAL_CX_MAP               .
TABLES: */USI/BAL_LV_DATA              .
TABLES: */USI/BAL_LV_LOBJ              .
TABLES: */USI/BAL_LV_RTIM              .
TABLES: */USI/BAL_LV_USER              .
TABLES: /USI/BAL_CX_MAP                .
TABLES: /USI/BAL_LV_DATA               .
TABLES: /USI/BAL_LV_LOBJ               .
TABLES: /USI/BAL_LV_RTIM               .
TABLES: /USI/BAL_LV_USER               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .