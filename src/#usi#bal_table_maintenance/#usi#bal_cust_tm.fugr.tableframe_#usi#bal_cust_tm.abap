*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/USI/BAL_CUST_TM
*   generation date: 25.11.2020 at 10:57:46
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/USI/BAL_CUST_TM   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
