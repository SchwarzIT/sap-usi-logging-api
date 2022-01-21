*-----------------------------------------------------------------------*
* Title   | Complex demo for USI Logging API                            *
*-----------------------------------------------------------------------*
* Purpose | Complex demo program for USI Logging API.                   *
*         |                                                             *
*         | Hint: Maintain table /USI/BAL_LV_USER in transaction SM30   *
*         |       and set your user's log level to the maximum (6).     *
*         |       Otherwise this demo is not going to produce any logs. *
*-----------------------------------------------------------------------*
PROGRAM /usi/bal_demo_02.

INCLUDE: /usi/bal_demo_02_top,
         /usi/bal_demo_02_p01,
         /usi/bal_demo_02_p02,
         /usi/bal_demo_02_p03,
         /usi/bal_demo_02_o01,
         /usi/bal_demo_02_o02,
         /usi/bal_demo_02_i01,
         /usi/bal_demo_02_i02.
