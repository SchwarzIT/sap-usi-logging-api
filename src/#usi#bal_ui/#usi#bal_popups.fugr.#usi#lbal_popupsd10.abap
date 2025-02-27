CLASS lcl_data_cont_coll_buffer DEFINITION FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING i_data_container_collections TYPE /usi/bal_msg_data_cont_colls.

    METHODS get_data_container_collection
      IMPORTING i_message_number TYPE /usi/bal_message_number
      RETURNING VALUE(r_result)  TYPE REF TO /usi/if_bal_data_container_col
      RAISING   /usi/cx_bal_root.

  PRIVATE SECTION.
    DATA data_container_collections TYPE /usi/bal_msg_data_cont_colls.

ENDCLASS.
