INTERFACE /usi/if_bal_data_container_rnd PUBLIC.

  INTERFACES /usi/if_bal_data_container.

  "! Display container data using the given container
  "!
  "! @parameter i_container | GUI Container
  "! @raising /usi/cx_bal_root | Error during rendering
  METHODS render
    IMPORTING
      i_container TYPE REF TO cl_gui_container
    RAISING
      /usi/cx_bal_root.

ENDINTERFACE.
