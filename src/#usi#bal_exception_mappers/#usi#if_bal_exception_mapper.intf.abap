INTERFACE /usi/if_bal_exception_mapper
  PUBLIC .

  METHODS get_t100_message
    RETURNING
      VALUE(r_result) TYPE symsg .

  METHODS get_data_containers
    IMPORTING
      i_target_data_cont_coll TYPE REF TO /usi/if_bal_data_container_col.

ENDINTERFACE.
